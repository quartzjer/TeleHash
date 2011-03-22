-module(switch).

-include("conf.hrl").
-include("types.hrl").

-export([start_link/0, add_handler/2, add_sup_handler/2, send/2, recv/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket}).
-define(EVENT, switch_event).
-define(SERVER, switch_server).

% --- api ---

start_link() ->
    {ok, Gen_event} = gen_event:start_link({local, ?EVENT}),
    {ok, Gen_server} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    {ok, Gen_event, Gen_server}.

add_handler(Module, Args) ->
    gen_event:add_handler(?EVENT, Module, Args).

add_sup_handler(Module, Args) ->
    gen_event:add_sup_handler(?EVENT, Module, Args).

send(#address{}=Address, Telex) ->
    gen_server:cast(?SERVER, {send, Address, Telex}).

% for testing / debugging
recv(#address{}=Address, Packet) ->
    gen_server:cast(?SERVER, {recv, Address, Packet}).

% --- gen_server callbacks ---

init([]) ->
    {ok, Socket} = gen_udp:open(?DEFAULT_PORT, [binary]),
    {ok, #state{socket=Socket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, #address{host=Host, port=Port}=Address, Telex}, #state{socket=Socket}=State) ->
    Telex2 = telex:set(Telex, '_to', util:address_to_binary(Address)),
    try gen_udp:send(Socket, Host, Port, telex:encode(Telex2)) of
	ok ->
	    log:info([?EVENT, send, Address, Telex2]),
	    gen_event:notify(?EVENT, {send, Address, Telex2});
	Error ->
	    log:info([?EVENT, send_error, Address, Telex2, Error])
    catch
	_:Error ->
	    log:info([?EVENT, send_error, Address, Telex2, Error, erlang:get_stacktrace()])
    end,
    {noreply, State};
handle_cast({recv, #address{}=Address, Packet}, State) -> 
    handle_recv(Address, Packet),
    {noreply, State};
handle_cast(_Packet, State) ->
    {noreply, State}.

handle_info({udp, Socket, {A,B,C,D}, Port, Packet}, #state{socket=Socket}=State) ->
    Host = lists:flatten(io_lib:format("~w.~w.~w.~w", [A,B,C,D])),
    Address = #address{host=Host, port=Port},
    handle_recv(Address, Packet),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket=Socket}) ->
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% --- internal functions ---

handle_recv(Address, Packet) ->
    try telex:decode(Packet) of
	Telex ->
	    log:info([?EVENT, recv, Address, Telex]),
	    gen_event:notify(?EVENT, {recv, Address, Telex})
    catch
	error:{telex, _, _, _}=Error ->
	    log:info([?EVENT, recv_error, Address, Packet, Error])
    end.

% --- end ---
