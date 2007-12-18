%% @version 0.2
%%% File    : mypl_util
%%% Author  : Maximillian Dornseif
%%% Created :  Created by Maximillian Dornseif on 2007-10-03.
-module(mypl_display).


-define(ZOOM, 5).  %% win size
-define(WSZ, 700).  %% win size
-define(BSZ, 9).    %% board size

%% API
%-export([]).
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
    

location_to_xy(Loc) ->
    Row = get_rownum(Loc),
    Col = get_colnum(Loc),
    Aisles = (Row+1) div 2,
    if Row < 11 -> AdjustedCol = Col + 3;
       true -> AdjustedCol = Col
    end,
    AdjustedRow = Row + (Aisles*2),
    {AdjustedRow*3, AdjustedCol*2}.

floor_locations() ->
    [X || X <- mypl_db_query:location_list(), string:len(X) > 5, string:substr(X, 6) =:= "1"].


display() ->
    lists:max([{distance(X, "061201")} || X <- floor_locations()]).
    % lists:sort([{distance(X, "050101"), X, location_to_xy(X)} || X <- mypl_db_query:location_list()]).


start() -> spawn(mypl_display, init, []).

init() ->
    put(baselocation, "010101"),
    {MaxDistance} = lists:max([{distance(X, get(baselocation))} || X <- floor_locations()]),
    Factor = 255 / MaxDistance,
    put(colorfactor, Factor),
    I=gs:start(),
    Win=gs:create(window, I,
        [{width, ?WSZ}, {height, ?WSZ + 35},
         {title,"myPL Viewer"}, {map, true}]),
         
    put(locationname,
        gs:create(label, Win, [{label, {text, "------"}},
                               {width, 60}, {height, 25},
                               {x, 0}, {y, 0}])),
    gs:create(canvas, can1, Win,
              [{x,0}, {y, 35}, {width, ?WSZ}, {height, ?WSZ}]),
    put(locationtable, ets:new(locations, [set])),
    draw_locations(lists:sort([{location_to_xy(X), 
                                round(distance(X, get(baselocation)) * Factor),
                                X} || X <- floor_locations()])),
    update_locations(),
    loop().
    

draw_locations([]) -> ok;
draw_locations([{{X, Y}, Color, Name}|Rest]) ->
    Rect = gs:create(rectangle, can1, [{coords, [{X*?ZOOM+1, Y*?ZOOM+1}, {X*?ZOOM+(?ZOOM*3)-1, Y*?ZOOM+(?ZOOM*2)-1}]},
                                       {data, Name},
                                       {buttonpress, true},
                                       {enter, true}]),
    ets:insert(get(locationtable), {Name, Rect}),
    draw_locations(Rest).
    

update_locations() ->
    Baselocation = get(baselocation),
    lists:map(fun({Loc, Rect}) ->
                  Color =  round(distance(Loc, Baselocation) * get(colorfactor)),
                  gs:config(Rect, [{fill, {Color, 255-Color, 0}}])
              end, ets:tab2list(get(locationtable))).

loop() ->
    receive
        {gs, IdOrName, enter, Location, []} ->
            gs:config(get(locationname), [{label, {text, Location}}]),
            loop();
        {gs, IdOrName, buttonpress, Location, Args} ->
            put(baselocation, Location),
            update_locations(),
            loop();
        {gs, IdOrName, EventType, Data, Args} ->
            io:format("Hello There ~w ~w ~w ~w ~n", [IdOrName, EventType, Data, Args]),
            loop()
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

