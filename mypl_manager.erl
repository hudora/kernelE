-module(mypl_manager).

-export([init_mypl/0]).

-include("mypl.hrl").

init_mypl() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(unit_load,        [{disc_copies,[node()]},
                                           {attributes, record_info(fields, unit_load)}]),
    mnesia:create_table(storage_location, [{disc_copies,[node()]},
                                           {attributes, record_info(fields, storage_location)}]),
    mnesia:create_table(movement,         [{disc_copies,[node()]},
                                           {attributes, record_info(fields, movement)}]),
    mnesia:stop().
