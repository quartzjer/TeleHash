-module(util).

-include("conf.hrl").
-include("types.hrl").

-export([address_to_binary/1, binary_to_address/1, binary_to_end/1, hex_to_end/1, end_to_hex/1, to_end/1, distance/2]).
-export([ensure_started/1, set_nth/3]).

% --- api --- 

address_to_binary(#address{host=Host, port=Port}) when is_list(Host) or is_binary(Host) ->
    iolist_to_binary(io_lib:format("~s:~w", [Host, Port])).

binary_to_address(Binary) when is_binary(Binary) ->
    {Host, [ $: | Port_string ]} = lists:splitwith(fun (Char) -> Char /= $: end, binary_to_list(Binary)),
    {Port, []} = string:to_integer(Port_string), 
    #address{host=Host, port=Port}.

binary_to_end(String) ->
    ok = ensure_started(crypto),
    {'end', crypto:sha(String)}.

end_to_hex({'end', End}) when is_binary(End) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [Byte]) || Byte <- binary_to_list(End)]).

hex_to_end(Hex) when is_binary(Hex) ->
    {'end', iolist_to_binary([Char - 48 || Char <- binary_to_list(Hex)])}.

to_end(#address{}=Address) ->
    binary_to_end(address_to_binary(Address));
to_end(Binary) when is_binary(Binary) ->
    binary_to_end(Binary);
to_end({'end', _} = End) ->
    End.

distance(A, B) ->
    {'end', EndA} = to_end(A),
    {'end', EndB} = to_end(B),
    Bytes = lists:zip(binary_to_list(EndA), binary_to_list(EndB)),
    Xor = list_to_binary([ByteA bxor ByteB || {ByteA, ByteB} <- Bytes]),
    <<Dist:?END_BITS>> = Xor,
    Dist.

ensure_started(Module) ->
    case Module:start() of
	ok -> ok;
	{error, {already_started, Module}} -> ok
    end.

set_nth(1, [_Head | Tail], Value) ->
    [Value | Tail];
set_nth(N, [Head | Tail], Value) when N>1 ->
    [Head | set_nth(N-1, Tail, Value)].

% --- end ---
