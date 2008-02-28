-module(mypl_statistics).

-export([statistics/0, aggregate/0, aggregate2/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

aggregate_helper([], Dict) -> Dict;
aggregate_helper([Record|Tail], Dict) ->
    Date = element(1, Record#archive.created_at),
    aggregate_helper(Tail, dict:update_counter({Date, Record#archive.archived_by}, 1, Dict)).
    

aggregate() ->
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(archive)])),
    lists:sort(dict:to_list(aggregate_helper(Records, dict:new()))).
    


aggregate2_helper([], Dict) -> Dict;
aggregate2_helper([Record|Tail], Dict) ->
    Date = element(1, Record#archive.created_at),
    Hour = element(1, element(2, Record#archive.created_at)),
    aggregate2_helper(Tail, dict:update_counter({{Date, {Hour, 0, 0}}, Record#archive.archived_by}, 1, Dict)).
    

aggregate2() ->
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(archive)])),
    lists:reverse(lists:sort(dict:to_list(aggregate2_helper(Records, dict:new())))).
    

statistics() ->
    [{empty_pickable_locations, mypl_movements:count_empty_floor_locations()},
     {multi_floorunits,         mypl_movements:more_than_one_floorunit()},
     {requesstracker_entries,   length(mypl_requesttracker:dump_requests())},
     {provpipeline_articles,    length(mypl_prov_query:pipelinearticles())}
     ].
     
