%%%%%%%%%% autogenerated code follows
% -export([backup/0,init_location/6,store_at_location/5,store_at_location_multi/1,retrieve/1,init_movement/2,init_movement_to_good_location/1,commit_movement/1,rollback_movement/1,commit_retrieval/1,rollback_retrieval/1,init_pick/2,commit_pick/1,rollback_pick/1,correction/1,count_product/1,count_products/0,unit_list/0,unit_info/1,location_list/0,location_info/1,movement_list/0,movement_info/1,pick_list/0,pick_info/1,find_provisioning_candidates/2,find_provisioning_candidates_multi/1,init_provisionings_multi/1,insert_pipeline/1,get_picklists/0,get_retrievallists/0,get_movementlist/0,commit_picklist/1,commit_retrievallist/1,commit_movementlist/1,is_provisioned/1,provpipeline_info/1,provpipeline_list_new/0,provpipeline_list_processing/0,provpipeline_processing_list_all/0,provisioninglist_list/0,provisioninglist_info/1,delete_pipeline/1,get_articleaudit/1,get_unitaudit/1,get_articlecorrection/1,get_recent_from_archive/1,get_abc/0,get_abcclass/0,make_oid/0,make_nve/0,dump_requests/0,feed_eap/16]).


% implementation for API and backend for backup
backup() ->
    gen_server:call(?SERVER, {backup, {}}, 9999).

% implementation for API and backend for init_location
init_location(Locname,Height,Floorlevel,Preference,Info,Attributes) ->
    gen_server:call(?SERVER, {init_location, {Locname,Height,Floorlevel,Preference,Info,Attributes}}, 9999).

% implementation for API and backend for store_at_location
store_at_location(Locname,Mui,Quantity,Product,Height) ->
    gen_server:call(?SERVER, {store_at_location, {Locname,Mui,Quantity,Product,Height}}, 9999).

% implementation for API and backend for store_at_location_multi
store_at_location_multi(JsonList) ->
    gen_server:call(?SERVER, {store_at_location_multi, {JsonList}}, 9999).

% implementation for API and backend for retrieve
retrieve(Mui) ->
    gen_server:call(?SERVER, {retrieve, {Mui}}, 9999).

% implementation for API and backend for init_movement
init_movement(Mui,Locname) ->
    gen_server:call(?SERVER, {init_movement, {Mui,Locname}}, 9999).

% implementation for API and backend for init_movement_to_good_location
init_movement_to_good_location(Mui) ->
    gen_server:call(?SERVER, {init_movement_to_good_location, {Mui}}, 9999).

% implementation for API and backend for commit_movement
commit_movement(MovementId) ->
    gen_server:call(?SERVER, {commit_movement, {MovementId}}, 9999).

% implementation for API and backend for rollback_movement
rollback_movement(MovementId) ->
    gen_server:call(?SERVER, {rollback_movement, {MovementId}}, 9999).

% implementation for API and backend for commit_retrieval
commit_retrieval(MovementId) ->
    gen_server:call(?SERVER, {commit_retrieval, {MovementId}}, 9999).

% implementation for API and backend for rollback_retrieval
rollback_retrieval(MovementId) ->
    gen_server:call(?SERVER, {rollback_retrieval, {MovementId}}, 9999).

% implementation for API and backend for init_pick
init_pick(Quantity,Mui) ->
    gen_server:call(?SERVER, {init_pick, {Quantity,Mui}}, 9999).

% implementation for API and backend for commit_pick
commit_pick(PickId) ->
    gen_server:call(?SERVER, {commit_pick, {PickId}}, 9999).

% implementation for API and backend for rollback_pick
rollback_pick(PickId) ->
    gen_server:call(?SERVER, {rollback_pick, {PickId}}, 9999).

% implementation for API and backend for correction
correction(JsonList) ->
    gen_server:call(?SERVER, {correction, {JsonList}}, 9999).

% implementation for API and backend for count_product
count_product(Product) ->
    gen_server:call(?SERVER, {count_product, {Product}}, 9999).

% implementation for API and backend for count_products
count_products() ->
    gen_server:call(?SERVER, {count_products, {}}, 9999).

% implementation for API and backend for unit_list
unit_list() ->
    gen_server:call(?SERVER, {unit_list, {}}, 9999).

% implementation for API and backend for unit_info
unit_info(Mui) ->
    gen_server:call(?SERVER, {unit_info, {Mui}}, 9999).

% implementation for API and backend for location_list
location_list() ->
    gen_server:call(?SERVER, {location_list, {}}, 9999).

% implementation for API and backend for location_info
location_info(Locname) ->
    gen_server:call(?SERVER, {location_info, {Locname}}, 9999).

% implementation for API and backend for movement_list
movement_list() ->
    gen_server:call(?SERVER, {movement_list, {}}, 9999).

% implementation for API and backend for movement_info
movement_info(MovementId) ->
    gen_server:call(?SERVER, {movement_info, {MovementId}}, 9999).

% implementation for API and backend for pick_list
pick_list() ->
    gen_server:call(?SERVER, {pick_list, {}}, 9999).

% implementation for API and backend for pick_info
pick_info(PickId) ->
    gen_server:call(?SERVER, {pick_info, {PickId}}, 9999).

% implementation for API and backend for find_provisioning_candidates
find_provisioning_candidates(Quantity,Product) ->
    gen_server:call(?SERVER, {find_provisioning_candidates, {Quantity,Product}}, 9999).

% implementation for API and backend for find_provisioning_candidates_multi
find_provisioning_candidates_multi(JsonList) ->
    gen_server:call(?SERVER, {find_provisioning_candidates_multi, {JsonList}}, 9999).

% implementation for API and backend for init_provisionings_multi
init_provisionings_multi(JsonList) ->
    gen_server:call(?SERVER, {init_provisionings_multi, {JsonList}}, 9999).

% implementation for API and backend for insert_pipeline
insert_pipeline(JsonList) ->
    gen_server:call(?SERVER, {insert_pipeline, {JsonList}}, 9999).

% implementation for API and backend for get_picklists
get_picklists() ->
    gen_server:call(?SERVER, {get_picklists, {}}, 9999).

% implementation for API and backend for get_retrievallists
get_retrievallists() ->
    gen_server:call(?SERVER, {get_retrievallists, {}}, 9999).

% implementation for API and backend for get_movementlist
get_movementlist() ->
    gen_server:call(?SERVER, {get_movementlist, {}}, 9999).

% implementation for API and backend for commit_picklist
commit_picklist(CId) ->
    gen_server:call(?SERVER, {commit_picklist, {CId}}, 9999).

% implementation for API and backend for commit_retrievallist
commit_retrievallist(CId) ->
    gen_server:call(?SERVER, {commit_retrievallist, {CId}}, 9999).

% implementation for API and backend for commit_movementlist
commit_movementlist(CId) ->
    gen_server:call(?SERVER, {commit_movementlist, {CId}}, 9999).

% implementation for API and backend for is_provisioned
is_provisioned(CId) ->
    gen_server:call(?SERVER, {is_provisioned, {CId}}, 9999).

% implementation for API and backend for provpipeline_info
provpipeline_info(CId) ->
    gen_server:call(?SERVER, {provpipeline_info, {CId}}, 9999).

% implementation for API and backend for provpipeline_list_new
provpipeline_list_new() ->
    gen_server:call(?SERVER, {provpipeline_list_new, {}}, 9999).

% implementation for API and backend for provpipeline_list_processing
provpipeline_list_processing() ->
    gen_server:call(?SERVER, {provpipeline_list_processing, {}}, 9999).

% implementation for API and backend for provpipeline_processing_list_all
provpipeline_processing_list_all() ->
    gen_server:call(?SERVER, {provpipeline_processing_list_all, {}}, 9999).

% implementation for API and backend for provisioninglist_list
provisioninglist_list() ->
    gen_server:call(?SERVER, {provisioninglist_list, {}}, 9999).

% implementation for API and backend for provisioninglist_info
provisioninglist_info(CId) ->
    gen_server:call(?SERVER, {provisioninglist_info, {CId}}, 9999).

% implementation for API and backend for delete_pipeline
delete_pipeline(CId) ->
    gen_server:call(?SERVER, {delete_pipeline, {CId}}, 9999).

% implementation for API and backend for get_articleaudit
get_articleaudit(Product) ->
    gen_server:call(?SERVER, {get_articleaudit, {Product}}, 9999).

% implementation for API and backend for get_unitaudit
get_unitaudit(Mui) ->
    gen_server:call(?SERVER, {get_unitaudit, {Mui}}, 9999).

% implementation for API and backend for get_articlecorrection
get_articlecorrection(Product) ->
    gen_server:call(?SERVER, {get_articlecorrection, {Product}}, 9999).

% implementation for API and backend for get_recent_from_archive
get_recent_from_archive(Type) ->
    gen_server:call(?SERVER, {get_recent_from_archive, {Type}}, 9999).

% implementation for API and backend for get_abc
get_abc() ->
    gen_server:call(?SERVER, {get_abc, {}}, 9999).

% implementation for API and backend for get_abcclass
get_abcclass() ->
    gen_server:call(?SERVER, {get_abcclass, {}}, 9999).

% implementation for API and backend for make_oid
make_oid() ->
    gen_server:call(?SERVER, {make_oid, {}}, 9999).

% implementation for API and backend for make_nve
make_nve() ->
    gen_server:call(?SERVER, {make_nve, {}}, 9999).

% implementation for API and backend for dump_requests
dump_requests() ->
    gen_server:call(?SERVER, {dump_requests, {}}, 9999).

% implementation for API and backend for feed_eap
feed_eap(Product,Prod_ve1,Prod_exportpackage,Export_pallet,Prod_x,Prod_y,Prod_z,Prod_g,Ve1_x,Ve1_y,Ve1_z,Ve1_g,Export_x,Export_y,Export_z,Export_g) ->
    gen_server:call(?SERVER, {feed_eap, {Product,Prod_ve1,Prod_exportpackage,Export_pallet,Prod_x,Prod_y,Prod_z,Prod_g,Ve1_x,Ve1_y,Ve1_z,Ve1_g,Export_x,Export_y,Export_z,Export_g}}, 9999).
%%%%%%%%%% call handlers (autogenerated) follow;

handle_call({backup, {}}, _From, State) ->
    Ret = mypl_db:backup(),
    {reply, Ret, State};

handle_call({init_location, {Locname,Height,Floorlevel,Preference,Info,Attributes}}, _From, State) ->
    Ret = mypl_db:init_location(Locname,Height,Floorlevel,Preference,Info,Attributes),
    {reply, Ret, State};

handle_call({store_at_location, {Locname,Mui,Quantity,Product,Height}}, _From, State) ->
    Ret = mypl_db:store_at_location(Locname,Mui,Quantity,Product,Height),
    {reply, Ret, State};

handle_call({store_at_location_multi, {JsonList}}, _From, State) ->
    Ret = mypl_db:store_at_location_multi(JsonList),
    {reply, Ret, State};

handle_call({retrieve, {Mui}}, _From, State) ->
    Ret = mypl_db:retrieve(Mui),
    {reply, Ret, State};

handle_call({init_movement, {Mui,Locname}}, _From, State) ->
    Ret = mypl_db:init_movement(Mui,Locname),
    {reply, Ret, State};

handle_call({init_movement_to_good_location, {Mui}}, _From, State) ->
    Ret = mypl_db:init_movement_to_good_location(Mui),
    {reply, Ret, State};

handle_call({commit_movement, {MovementId}}, _From, State) ->
    Ret = mypl_db:commit_movement(MovementId),
    {reply, Ret, State};

handle_call({rollback_movement, {MovementId}}, _From, State) ->
    Ret = mypl_db:rollback_movement(MovementId),
    {reply, Ret, State};

handle_call({commit_retrieval, {MovementId}}, _From, State) ->
    Ret = mypl_db:commit_retrieval(MovementId),
    {reply, Ret, State};

handle_call({rollback_retrieval, {MovementId}}, _From, State) ->
    Ret = mypl_db:rollback_retrieval(MovementId),
    {reply, Ret, State};

handle_call({init_pick, {Quantity,Mui}}, _From, State) ->
    Ret = mypl_db:init_pick(Quantity,Mui),
    {reply, Ret, State};

handle_call({commit_pick, {PickId}}, _From, State) ->
    Ret = mypl_db:commit_pick(PickId),
    {reply, Ret, State};

handle_call({rollback_pick, {PickId}}, _From, State) ->
    Ret = mypl_db:rollback_pick(PickId),
    {reply, Ret, State};

handle_call({correction, {JsonList}}, _From, State) ->
    Ret = mypl_db:correction(JsonList),
    {reply, Ret, State};

handle_call({count_product, {Product}}, _From, State) ->
    Ret = mypl_db_query:count_product(Product),
    {reply, Ret, State};

handle_call({count_products, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_db_query:count_products()) end),
    {noreply, State, 9999};

handle_call({unit_list, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_db_query:unit_list()) end),
    {noreply, State, 9999};

handle_call({unit_info, {Mui}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_db_query:unit_info(Mui)) end),
    {noreply, State, 9999};

handle_call({location_list, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_db_query:location_list()) end),
    {noreply, State, 9999};

handle_call({location_info, {Locname}}, _From, State) ->
    Ret = mypl_db_query:location_info(Locname),
    {reply, Ret, State};

handle_call({movement_list, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_db_query:movement_list()) end),
    {noreply, State, 9999};

handle_call({movement_info, {MovementId}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_db_query:movement_info(MovementId)) end),
    {noreply, State, 9999};

handle_call({pick_list, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_db_query:pick_list()) end),
    {noreply, State, 9999};

handle_call({pick_info, {PickId}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_db_query:pick_info(PickId)) end),
    {noreply, State, 9999};

handle_call({find_provisioning_candidates, {Quantity,Product}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provisioning:find_provisioning_candidates(Quantity,Product)) end),
    {noreply, State, 9999};

handle_call({find_provisioning_candidates_multi, {JsonList}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provisioning:find_provisioning_candidates_multi(JsonList)) end),
    {noreply, State, 9999};

handle_call({init_provisionings_multi, {JsonList}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provisioning:init_provisionings_multi(JsonList)) end),
    {noreply, State, 9999};

handle_call({insert_pipeline, {JsonList}}, _From, State) ->
    Ret = mypl_provpipeline:insert_pipeline(JsonList),
    {reply, Ret, State};

handle_call({get_picklists, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provpipeline:get_picklists()) end),
    {noreply, State, 9999};

handle_call({get_retrievallists, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provpipeline:get_retrievallists()) end),
    {noreply, State, 9999};

handle_call({get_movementlist, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provpipeline:get_movementlist()) end),
    {noreply, State, 9999};

handle_call({commit_picklist, {CId}}, _From, State) ->
    Ret = mypl_provpipeline:commit_picklist(CId),
    {reply, Ret, State};

handle_call({commit_retrievallist, {CId}}, _From, State) ->
    Ret = mypl_provpipeline:commit_retrievallist(CId),
    {reply, Ret, State};

handle_call({commit_movementlist, {CId}}, _From, State) ->
    Ret = mypl_provpipeline:commit_movementlist(CId),
    {reply, Ret, State};

handle_call({is_provisioned, {CId}}, _From, State) ->
    Ret = mypl_provpipeline:is_provisioned(CId),
    {reply, Ret, State};

handle_call({provpipeline_info, {CId}}, _From, State) ->
    Ret = mypl_provpipeline:provpipeline_info(CId),
    {reply, Ret, State};

handle_call({provpipeline_list_new, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provpipeline:provpipeline_list_new()) end),
    {noreply, State, 9999};

handle_call({provpipeline_list_processing, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provpipeline:provpipeline_list_processing()) end),
    {noreply, State, 9999};

handle_call({provpipeline_processing_list_all, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provpipeline:provpipeline_processing_list_all()) end),
    {noreply, State, 9999};

handle_call({provisioninglist_list, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_provpipeline:provisioninglist_list()) end),
    {noreply, State, 9999};

handle_call({provisioninglist_info, {CId}}, _From, State) ->
    Ret = mypl_provpipeline:provisioninglist_info(CId),
    {reply, Ret, State};

handle_call({delete_pipeline, {CId}}, _From, State) ->
    Ret = mypl_provpipeline:delete_pipeline(CId),
    {reply, Ret, State};

handle_call({get_articleaudit, {Product}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_audit:get_articleaudit(Product)) end),
    {noreply, State, 9999};

handle_call({get_unitaudit, {Mui}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_audit:get_unitaudit(Mui)) end),
    {noreply, State, 9999};

handle_call({get_articlecorrection, {Product}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_audit:get_articlecorrection(Product)) end),
    {noreply, State, 9999};

handle_call({get_recent_from_archive, {Type}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_audit:get_recent_from_archive(Type)) end),
    {noreply, State, 9999};

handle_call({get_abc, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_abcserver:get_abc()) end),
    {noreply, State, 9999};

handle_call({get_abcclass, {}}, From, State) ->
    proc_lib:spawn(fun() -> gen_server:reply(From, mypl_abcserver:get_abcclass()) end),
    {noreply, State, 9999};

handle_call({make_oid, {}}, _From, State) ->
    Ret = mypl_nveserver:make_oid(),
    {reply, Ret, State};

handle_call({make_nve, {}}, _From, State) ->
    Ret = mypl_nveserver:make_nve(),
    {reply, Ret, State};

handle_call({dump_requests, {}}, _From, State) ->
    Ret = mypl_requesttracker:dump_requests(),
    {reply, Ret, State};

handle_call({feed_eap, {Product,Prod_ve1,Prod_exportpackage,Export_pallet,Prod_x,Prod_y,Prod_z,Prod_g,Ve1_x,Ve1_y,Ve1_z,Ve1_g,Export_x,Export_y,Export_z,Export_g}}, _From, State) ->
    Ret = mypl_volumes:feed_eap(Product,Prod_ve1,Prod_exportpackage,Export_pallet,Prod_x,Prod_y,Prod_z,Prod_g,Ve1_x,Ve1_y,Ve1_z,Ve1_g,Export_x,Export_y,Export_z,Export_g),
    {reply, Ret, State}.
%%%%%%%%%% autogenerated code ends

