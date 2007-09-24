-module(mypl_manager).
-export([init_mypl/0]).

-include("mypl.hrl").

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

create_level(Row, Col, Level) when Level =:= 1 ->
    Name = lists:flatten(io_lib:format("~2.10.0B-~2.10.0B-~2.10.0B", [Row, Col, Level])),
    Slot = #location{name=Name, height=(2000-Row-Col-Level), floorlevel=true},
    mnesia:write(Slot);
create_level(Row, Col, Level) ->
    Name = lists:flatten(io_lib:format("~2.10.0B-~2.10.0B-~2.10.0B", [Row, Col, Level])),
    Slot = #location{name=Name, height=2000-Row-Col-Level, floorlevel=false},
    mnesia:write(Slot).

create_column(Row, Col) ->
    for(1,3, fun(Level) -> create_level(Row, Col, Level) end).

create_row(Row) when Row < 5 -> for(13,54, fun(Col) -> create_column(Row, Col) end);
create_row(Row) when Row < 9 -> for(1,54, fun(Col) -> create_column(Row, Col) end);
create_row(Row) -> for(1,58, fun(Col) -> create_column(Row, Col) end).

create_locations() ->
    Fun = fun() -> for(1, 19, fun(Row) -> create_row(Row) end) end,
    mnesia:transaction(Fun).

init_mypl() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(unit_load,        [{disc_copies,[node()]},
                                           {attributes, record_info(fields, unit_load)}]),
    mnesia:create_table(location,         [{disc_copies,[node()]},
                                           {attributes, record_info(fields, location)}]),
    mnesia:create_table(movement,         [{disc_copies,[node()]},
                                           {attributes, record_info(fields, movement)}]),
    create_locations(),
    mnesia:stop().

