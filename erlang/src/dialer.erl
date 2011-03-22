-module(dialer).

-include("conf.hrl").
-include("types.hrl").

-export([dial/3, dial_sync/3]).

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

% corresponds to k and alpha in kademlia paper
-define(K, ?DIAL_DEPTH).
-define(A, ?DIAL_BREADTH).

-record(conf, {
	  target, % the end to dial
	  timeout, % the timeout for the entire dialing process
	  ref, caller % reply details
	 }).
-record(state, {
	  fresh, % nodes which have not yet been contacted
	  pinged, % nodes which have been contacted and have not replied
	  waiting, % nodes in pinged which were contacted less than ?DIAL_TIMEOUT ago
	  ponged, % nodes which have been contacted and have replied
	  seen % all nodes which have been seen 
	 }). % invariant: pq:length(waiting) = ?A or pq:empty(fresh)

% --- api ---

dial(To, From, Timeout) ->
    log:info([?MODULE, dialing, To, From, Timeout]),
    Ref = erlang:make_ref(),
    Target = util:to_end(To),
    Conf = #conf{
      target = Target,
      timeout = Timeout,
      ref = Ref,
      caller = self()
     },
    Nodes = [{util:distance(Address, Target), Address}
	     || Address <- From],
    State = #state{
      fresh=pq:from_list(Nodes), 
      pinged=sets:new(),
      waiting=pq:empty(),
      ponged=pq:empty(), 
      seen=sets:new()
     },
    ok = switch:add_handler(?MODULE, {Conf, State}),
    Ref.

dial_sync(Target, Addresses, Timeout) ->
    Ref = dial(Target, Addresses, Timeout),
    receive
	{dialed, Ref, Result} ->
	    {ok, Result}
    after Timeout ->
	    {error, timeout}
    end.

% --- gen_event callbacks ---

init({#conf{timeout=Timeout}=Conf, State}) ->
    erlang:send_after(Timeout, self(), giveup),
    State2 = ping_nodes(Conf, State),
    {ok, {Conf, State2}}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({recv, Address, Telex}, {#conf{target=Target}=Conf, #state{pinged=Pinged}=State}) ->
    case telex:get(Telex, '.see') of
	{error, not_found} -> 
	    log:info([?MODULE, irrelevant, Telex]), % !!! remove
	    {ok, {Conf, State}};
	{ok, Address_binaries} ->
	    Dist = util:distance(Address, Target),
	    Node = {Dist, Address},
	    case sets:is_element(Node, Pinged) of % !!! command ids would make a better check
		false ->
		    log:info([?MODULE, not_seen, Address, Telex]), % !!! remove
		    {ok, {Conf, State}};
		true ->
		    try [{util:distance(Target, Bin), util:binary_to_address(Bin)} || Bin <- Address_binaries] of
			Nodes ->
			    log:info([?MODULE, pong, Node, Nodes]),
			    State2 = ponged(Node, Nodes, State),
			    continue(Conf, State2)
		    catch 
			_:Error ->
			    log:info([?MODULE, bad_see, Address, Telex, Error, erlang:get_stacktrace()]),
			    {ok, {Conf, State}}
		    end
	    end
    end;
handle_event(_, State) ->
    {ok, State}.

handle_info(giveup, {Conf, State}) ->
    log:info([?MODULE, giveup, Conf, State]),
    remove_handler;
handle_info({timeout, Node}, {Conf, #state{waiting=Waiting}=State}) ->
    log:info([?MODULE, timeout, Node]),
    State2 = State#state{waiting=pq:delete(Node, Waiting)},
    continue(Conf, State2);
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% --- internal functions ---

% is the dialing finished yet?
finished(#state{fresh=Fresh, waiting=Waiting, ponged=Ponged}) ->
    (pq:is_empty(Fresh) and pq:is_empty(Waiting)) % no way to continue
    or
    (case pq:length(Ponged) >= ?K of
	 false ->
	     false; % dont yet have K nodes
	 true ->
	     % finish if the K closest nodes we know are closer than all the nodes we haven't checked yet
	     {Dist_fresh, _} = pq:peek(Fresh),
	     {Dist_waiting, _} = pq:peek(Waiting),
	     {Nodes, _} = pq:pop(Ponged, ?K),
	     {Dist_ponged, _} = lists:last(Nodes),
	     (Dist_ponged < Dist_fresh) and (Dist_ponged < Dist_waiting)
     end).

% contact nodes from fresh until the waiting list is full
ping_nodes(#conf{target=Target}, #state{fresh=Fresh, waiting=Waiting, pinged=Pinged}=State) ->
    Num = ?A - pq:length(Waiting),
    {Nodes, Fresh2} = pq:pop(Fresh, Num),
    Telex = {struct, [{'+end', util:end_to_hex(Target)}]},
    lists:foreach(
      fun ({Dist, Address}=Node) -> 
	      log:info([?MODULE, ping, Dist, Address]),
	      switch:send(Address, Telex),
	      erlang:send_after(?DIAL_TIMEOUT, self(), {timeout, Node})
      end, 
      Nodes),
    Waiting2 = pq:push(Nodes, Waiting),
    Pinged2 = sets:union(Pinged, sets:from_list(Nodes)),
    State#state{fresh=Fresh2, waiting=Waiting2, pinged=Pinged2}.

% handle a reply from a node
ponged(Node, See, #state{fresh=Fresh, waiting=Waiting, pinged=Pinged, ponged=Ponged, seen=Seen}=State) ->
    Waiting2 = pq:delete(Node, Waiting),
    Pinged2 = sets:del_element(Node, Pinged),
    Ponged2 = pq:push_one(Node, Ponged),
    New_nodes = lists:filter(fun (See_node) -> not(sets:is_element(See_node, Seen)) end, See),
    Fresh2 = pq:push(New_nodes, Fresh),
    Seen2 = sets:union(Seen, sets:from_list(See)),
    State#state{fresh=Fresh2, waiting=Waiting2, pinged=Pinged2, ponged=Ponged2, seen=Seen2}.
				
% return results to the caller	   
return(#conf{ref=Ref, caller=Caller}, #state{ponged=Ponged}) ->
    {Nodes, _} = pq:pop(Ponged, ?K),
    log:info([?MODULE, returning, Nodes]),
    Result = [Address || {_Dist, Address} <- Nodes],
    Caller ! {dialed, Ref, Result}.

% either continue to dial or return results
% meant for use at the end of a gen_event callback
continue(Conf, State) ->
    case finished(State) of
	true ->
	    return(Conf, State),
	    remove_handler;
	false ->
	    State2 = ping_nodes(Conf, State),
	    {ok, {Conf, State2}}
    end.

% --- end ---
  
