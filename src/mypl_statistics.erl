-module(mypl_statistics).

-export([aggregate/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

aggregate_helper([], Dict) -> Dict;
aggregate_helper([Record|Tail], Dict) ->
    Date = element(1, Record#archive.created_at),
    aggregate_helper(Tail, dict:update_counter({Date, Record#archive.archived_by}, 1, Dict)).
    

aggregate() ->
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(archive)])),
    lists:sort(dict:to_list(aggregate_helper(Records, dict:new()))).
    
