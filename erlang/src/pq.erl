-module(pq).

-export([empty/0, is_empty/1, length/1, push_one/2, pop_one/1, push/2, pop/2, peek/1, to_list/1, from_list/1, delete/2]).

empty() ->
    gb_sets:empty().

is_empty(Q) ->
    gb_sets:is_empty(Q).

length(Q) ->
    gb_sets:size(Q).

push_one({Prio, Item}, Q) ->
    gb_sets:add_element({Prio, Item}, Q).

pop_one(Q) ->
    case gb_sets:is_empty(Q) of 
	true -> false;
	false ->
	    {{Prio, Item}, Q2} = gb_sets:take_smallest(Q),
	    {{Prio, Item}, Q2}
    end.

push(Items, Q) ->
    lists:foldl(fun ({Prio, Item}, Q2) -> push_one({Prio, Item}, Q2) end, Q, Items).

pop(Q, 0) ->
    {[], Q};
pop(Q, K) when K > 0 ->
    case gb_sets:is_empty(Q) of
	true -> 
	    {[], Q};
	false ->
	    {Item, Q2} = pop_one(Q),
	    {Items, Q3} = pop(Q2, K-1),
	    {[Item | Items], Q3}
    end.

peek(Q) ->
    gb_sets:smallest(Q).

delete({Prio, Item}, Q) ->
    gb_sets:delete_any({Prio, Item}, Q).

to_list(Q) ->
    gb_sets:to_list(Q).

from_list(Q) ->
    gb_sets:from_list(Q).
