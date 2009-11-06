-module(mypl_statistics).

-export([statistics/0, bewegungen/0, bewegungen2/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

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
    

oldest_movement() ->
    case [get_age_for_movement(X) || X <- mypl_db_query:movement_list()] of
        [] -> <<"">>;
        Ages -> mypl_util:ensure_binary(lists:min(Ages))
    end.
    

get_age_for_movement(MovementId) ->
    case mnesia:dirty_read({movement, MovementId}) of
        [] ->
            "2030-01-01 00:00:00.0Z"; % far in the future
        [Movement] ->
            Movement#movement.created_at
    end.

oldest_pick() ->
    case [get_age_for_pick(X) || X <- mypl_db_query:pick_list()] of
        [] -> <<"">>;
        Ages -> mypl_util:ensure_binary(lists:min(Ages))
    end.
    

get_age_for_pick(PickId) ->
    case mnesia:dirty_read({pick, PickId}) of
        [] ->
            "2030-01-01 00:00:00.0Z"; % far in the future
        [Pick] ->
            Pick#pick.created_at
    end.


get_number_of_units() ->
    length(mnesia:dirty_all_keys(unit)).

statistics() ->
    [{empty_pickable_locations, mypl_movements:count_empty_floor_locations()},
     {multi_floorunits,         length(mypl_movements:more_than_one_floorunit())},
     {requesstracker_entries,   length(mypl_requesttracker:dump_requests())},
     {provpipeline_articles,    length(mypl_prov_query:pipelinearticles())},
     {provpipeline_new,         length(mypl_prov_query:provpipeline_list_new())},
     {provlists_prepared,       length(mypl_prov_query:provpipeline_list_prepared())},
     {provpipeline_processing,  length(mypl_prov_query:provpipeline_list_processing())},
     {open_movements,           length(mypl_db_query:movement_list())},
     {open_picks,               length(mypl_db_query:pick_list())},
     {oldest_movement,          oldest_movement()},
     {oldest_pick,              oldest_pick()},
     {units,                    get_number_of_units()},
     {products,                 length(mypl_db_query:count_products())}
     ].

