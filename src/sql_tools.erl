%% @version 0.1
%% @copyright 2008 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>

-module(sql_tools).

-export([quote/1]).

% based on http://code.google.com/p/erlsql/source/browse/trunk/src/erlsql.erl

-spec quote(atom() | binary() | [any()] | integer()) -> binary() | [any(),...].
quote(Atom) when is_atom(Atom) -> quote(atom_to_list(Atom));
quote(Integer) when is_integer(Integer) -> quote(integer_to_list(Integer));
quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])];	%% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

-spec quote([any()],[any()]) -> [any()].
quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->		%% 39 is $'
    quote(Rest, [39, $\\ | Acc]);	%% 39 is $'
quote([34 | Rest], Acc) ->		%% 34 is $"
    quote(Rest, [34, $\\ | Acc]);	%% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).
