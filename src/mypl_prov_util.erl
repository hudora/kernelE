%%%-------------------------------------------------------------------
%%% File:      mypl_prov_util.erl
%%% @author    Maximillian Dornseif <md@hudora.de> []
%%% @copyright 2008 Maximillian Dornseif
%%% @doc Utilities regarding the generation of Provisioning 
%%% @end
%%%
%%% @since 2008-02-21 by Maximillian Dornseif
%%%-------------------------------------------------------------------
-module(mypl_prov_util).
-author('Maximillian Dorneif').

-include("mypl.hrl").

%% API
-export([sort_provpipeline/1, shouldprocess/1, sort_provpipeline_helper/1]).

%%====================================================================
%% API
%%====================================================================

% @doc sort provpipeline records in order which they should be handled
%
% Sorts by 
% <ul>
% <li>shut it be shipped NOW?</li>
% <li>attribute `versandtermin'</li>
% <li>attribute `liefertermin'</li>
% <li>priority</li>
% <li>number number of unsuccessfull tries to select  (more tries sort first)</li>
% <li>customer ID</li>
% </ul>
-spec sort_provpipeline([#provpipeline{attributes::[any()]},...]) ->
    [#provpipeline{attributes::[any()]},...].
sort_provpipeline(Records) ->
    lists:sort(fun(A, B) -> sort_provpipeline_helper(A) < sort_provpipeline_helper(B) end, Records).

% @private
% creates a key for sorting
-spec sort_provpipeline_helper(#provpipeline{}) -> term().
sort_provpipeline_helper(Record) ->
    {5-Record#provpipeline.priority,
     not (proplists:get_value(fixtermin, Record#provpipeline.attributes, false) 
          and (shouldprocess(Record) =:= yes)),
     proplists:get_value(versandtermin, Record#provpipeline.attributes, ""),
     proplists:get_value(liefertermin,  Record#provpipeline.attributes, ""), 
     Record#provpipeline.tries,
     proplists:get_value(kernel_customer, Record#provpipeline.attributes, "99999")
    }.
    

%% @doc decides if this Record can be packed/shipped.
%% 
%% Returns:
%%   yes:   MUST be shipped today
%%   maybe: CAN be shipped today
%%   no:    MAY NOT be shipped today
-spec shouldprocess(#provpipeline{attributes::[any()]}) -> 'maybe' | 'no' | 'yes'.
shouldprocess(Record) ->
    {{Year,Month,Day}, _} =  calendar:now_to_datetime(erlang:now()),
    Today = mypl_util:ensure_binary(lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year, Month, Day]))),
    Termin = proplists:get_value(versandtermin, Record#provpipeline.attributes,
                                 proplists:get_value(liefertermin,  Record#provpipeline.attributes, "")),
    Fix = proplists:get_value(fixtermin, Record#provpipeline.attributes, false),
    case {Fix, mypl_util:ensure_binary(Termin) =< Today} of
        {_, true} ->
            % due date reached - ship it
            yes;
        {false, false} ->
            % due date not reached, but we may ship, because no fixed delivery date was given
            maybe;
        {true, false} ->
            % due date not reached, fixed delivery date was given
            no
    end.
    

% ~~ Unit tests
-ifdef(EUNIT).
-compile(export_all).

%%% @hidden
test_init() ->
    mypl_db:run_me_once(),
    % flush database
    mnesia:start(),
    mnesia:clear_table(unit),
    mnesia:clear_table(location),
    mnesia:clear_table(movement),
    mnesia:clear_table(pick),
    mnesia:clear_table(picklist),
    mnesia:clear_table(articleaudit),
    mnesia:clear_table(unitaudit),
    mnesia:clear_table(provpipeline),
    mnesia:clear_table(pickpipeline),
    mnesia:clear_table(provpipeline_processing),
    mnesia:clear_table(provisioninglist),
    mnesia:clear_table(retrievalpipeline),
    % regenerate locations
    % init_location(Name, Height, Floorlevel, Preference, Attributes)
    mypl_db:init_location("EINLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("AUSLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("010101", 2000, true,  6, []),
    mypl_db:init_location("010102", 1950, false, 6, []),
    mypl_db:init_location("010103", 1200, false, 5, []),
    mypl_db:init_location("010201", 2000, true,  7, []),
    mypl_db:init_location("010202", 2000, false,  7, []),
    mypl_db:init_location("010203", 2000, false,  7, []),
    mypl_db:init_location("010301", 2000, true,  7, []),
    mypl_db:init_location("010302", 2000, false,  7, []),
    mypl_db:init_location("010303", 2000, false,  7, []),
    
    {ok, _} = mypl_db:store_at_location("EINLAG", "mui1",  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("010101", "mui2",  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("EINLAG", "mui3", 17, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010201", "mui4", 19, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010301", "mui5", 61, "a0005", 1200),
    {ok, _} = mypl_db:store_at_location("010302", "mui6", 10, "a0005", 1200),
    [{"a0003",10,10,0,0},{"a0004",36,36,0,0},{"a0005",71,71,0,0}] = mypl_db_query:count_products(),
    % provpipeline empty?
    [] = mypl_prov_query:provpipeline_list_new(),
    [] = mypl_prov_query:provpipeline_list_prepared(),
    
    % fill it up!
    mypl_provpipeline:insert_pipeline([lieferschein1, [{10, "a0005", []}, {1,  "a0004", []}], 3, "kunde01", 0, 0,
        [{liefertermin, "2007-12-15"}, {versandtermin, "2007-12-14"}]]),
    mypl_provpipeline:insert_pipeline([lieferschein2, [{10, "a0005", []}, {1,  "a0004", []}], 3, "kunde02", 0, 0,
        [{liefertermin, "2007-12-13"}, {versandtermin, "2007-12-16"}]]),
    
    mypl_provpipeline:insert_pipeline([lieferschein3, [{50, "a0005", []}, {16, "a0004", []}], 4, "kunde02", 0, 0,
        [{liefertermin, "2007-12-13"}, {versandtermin, "2007-12-16"}]]),
    
    mypl_provpipeline:insert_pipeline([lieferschein4, [{1,  "a0005", []}, {1,  "a0004", []}], 1, "kunde03", 0, 0,
        [{liefertermin, "2007-12-10"}]]),
    
    ok.
    

%%% @hidden
sort_provpipeline_test() ->
    test_init(),
    1 = 2,
    ok.

%%% @hidden
testrunner() ->
    ok.
    

-endif.
