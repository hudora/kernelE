-module(mypl_statistics).

-export([statistics/0, bewegungen/0, bewegungen2/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

bewegungen_helper([], Dict) -> Dict;
bewegungen_helper([Record|Tail], Dict) ->
    Date = element(1, Record#archive.created_at),
    bewegungen_helper(Tail, dict:update_counter({Date, Record#archive.archived_by}, 1, Dict)).
    

bewegungen() ->
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(archive)])),
    lists:sort(dict:to_list(bewegungen_helper(Records, dict:new()))).
    


bewegungen2_helper([], Dict) -> Dict;
bewegungen2_helper([Record|Tail], Dict) ->
    Date = element(1, Record#archive.created_at),
    Hour = element(1, element(2, Record#archive.created_at)),
    bewegungen2_helper(Tail, dict:update_counter({{Date, {Hour, 0, 0}}, Record#archive.archived_by}, 1, Dict)).
    

bewegungen2() ->
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(archive)])),
    lists:reverse(lists:sort(dict:to_list(bewegungen2_helper(Records, dict:new())))).
    

statistics() ->
    [{empty_pickable_locations, mypl_movements:count_empty_floor_locations()},
     {multi_floorunits,
length(mypl_movements:more_than_one_floorunit())},
     {requesstracker_entries,   length(mypl_requesttracker:dump_requests())},
     {provpipeline_articles,    length(mypl_prov_query:pipelinearticles())},
     {provpipeline_new,         length(mypl_prov_query:provpipeline_list_new())},
     {provlists_prepared,       length(mypl_prov_query:provpipeline_list_prepared())},
     {provpipeline_processing,  length(mypl_prov_query:provpipeline_list_processing())},
     {open_movements,           length(mypl_db_query:movement_list())},
     {open_picks,               length(mypl_db_query:pick_list())}
     ].
