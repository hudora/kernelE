%% @version 0.2
%%% File    : mypl_util
%%% Author  : Maximillian Dornseif
%%% Created :  Created by Maximillian Dornseif on 2007-10-03.
-module(mypl_distance).


-define(ZOOM, 5).  %% win size
-define(WSZ, 700).  %% win size
-define(BSZ, 9).    %% board size

%% API
-export([distance/2]).
-compile(export_all).

get_row("EINLAG") -> "98EINLAG";
get_row("AUSLAG") -> "00AUSLAG";
get_row("FEHLER") -> "98FEHLER";
get_row("VERSAN") -> "98VERSAN";
get_row([$K,_,_]) -> "00K";
get_row([H|T]) when H > $9 -> "99" ++ [H|T];
get_row([A,B,_,_,_,_]) -> [A,B];
get_row(X) -> "99" ++ X.

get_rownum(Loc) ->
    get_rownum_helper(get_row(Loc)).

get_rownum_helper([H1|[H2|_T]]) ->
    list_to_integer([H1, H2]).

get_colnum("EINLAG") -> 0;
get_colnum("AUSLAG") -> 50;
get_colnum("FEHLER") -> 10;
get_colnum([$K|Num]) -> list_to_integer(Num);
get_colnum([H|_T]) when H > $9 -> 20;
get_colnum([_,_,H1,H2,_,_]) -> list_to_integer([H1, H2]).

get_aisle(Loc) ->
    get_rownum(Loc) div 2.

same_aisle(Loc1, Loc2) ->
    get_aisle(Loc1) =:= get_aisle(Loc2).

column_distance(Loc1, Loc2) ->
    abs(get_colnum(Loc1) - get_colnum(Loc2)).

distance_to_corridors(Loc) ->
    [column_distance(Loc, "001200"),
     column_distance(Loc, "004400")].

distance_path_via_corridor(Loc1, CorrD1, Loc2, CorrD2) ->
    % distance is Loc1 -> Corridor, + Corridor -> Loc2 + Distance in corridor (1 aisle = 2 rows)
    CorrD1 + CorrD2 + (abs(get_aisle(Loc1) - get_aisle(Loc2)) * 2).

distance(Loc1, Loc2) ->
    Row1 = get_rownum(Loc1),
    Row2 = get_rownum(Loc2),
    if
        Row1 =:= Row2 ->
            % we are in the same row - simple calculation of column distance
            column_distance(Loc1, Loc2);
        true ->
            case same_aisle(Loc1, Loc2) of
                true ->
                    % we are in the same aisle: use a penalty of 5 for changing the side of an aisle
                    % + row distance
                    column_distance(Loc1, Loc2) + 5;
                false ->
                    % we have to change aisles and have different corridort we could take
                    % calculate distance to corridors
                    CorridorDistances = lists:zip(distance_to_corridors(Loc1), distance_to_corridors(Loc2)),
                    % calculate the pathlenths using dirrerent corridors
                    Paths = lists:map(fun({CorridorDistance1, CorridorDistance2}) ->
                                          distance_path_via_corridor(Loc1, CorridorDistance1,
                                                                     Loc2, CorridorDistance2) end,
                                      CorridorDistances),
                    % return the shortest path + 10 Penalty for changing corridors.
                    lists:min(Paths) + 10
            end
    end.
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).

%%% @hidden
get_row_test() ->
    "01"       = get_row("010101"),
    "00K"      = get_row("K01"),
    "98EINLAG" = get_row("EINLAG"),
    "00AUSLAG" = get_row("AUSLAG"),
    "98FEHLER" = get_row("FEHLER"),
    "99UMLAG"  = get_row("UMLAG"),
    1          = get_rownum("010101"),
    0          = get_rownum("K01"),
    98         = get_rownum("EINLAG"),
    0          = get_rownum("AUSLAG"),
    99         = get_rownum("UMLAG"),
    98         = get_rownum("FEHLER"),
    ok.

get_aisle_test() ->
    0           = get_aisle("K01"),
    0           = get_aisle("010101"),
    1           = get_aisle("020202"),
    1           = get_aisle("030303"),
    ok.
    
same_row_test() ->
    false       = same_aisle("K01",    "020202"),
    false       = same_aisle("010101", "020202"),
    false       = same_aisle("010101", "030303"),
    true        = same_aisle("020202", "020202"),
    true        = same_aisle("020202", "030303"),
    true        = same_aisle("020202", "029999"),
    true        = same_aisle("030303", "020202"),
    false       = same_aisle("020202", "055001"),
    ok.

column_distance_test() ->
    14          = column_distance("010101", "011501"),
    20          = column_distance("013501", "011501"),
    0           = column_distance("011501", "011501"),
    ok.

distance_test() ->
    2  = distance("023201", "023001"),
    7  = distance("023201", "033001"),
    30 = distance("012201", "032001"),
    30 = distance("012201", "022001"),
    ok.

%%% @hidden
testrunner() ->
    get_row_test(),
    get_aisle_test(),
    same_row_test(),
    column_distance_test(),
    distance_test(),
    ok.
    

-endif.

