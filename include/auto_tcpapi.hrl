%%%%%%%%%% autogenerated code follows;
handle_command("backup", _Parameters, State) ->
    % implementation for backup,
    {noreply, reply(220, rfc4627:encode(mypl_server:backup()), reset_buffers(State))};
handle_command("init_location", _Parameters, State) ->
    % implementation for init_location,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Locname,Height,Floorlevel,Preference,Info,Attributes] = Tokens,
    NewLocname = convertString(Locname),
    NewHeight = convertPositiveInteger(Height),
    NewFloorlevel = convertBoolean(Floorlevel),
    NewPreference = convertPositiveInteger(Preference),
    NewInfo = convertString(Info),
    NewAttributes = convertArray(Attributes),
    {noreply, reply(220, rfc4627:encode(mypl_server:init_location(NewLocname,NewHeight,NewFloorlevel,NewPreference,NewInfo,NewAttributes)), reset_buffers(State))};
handle_command("store_at_location", _Parameters, State) ->
    % implementation for store_at_location,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Locname,Mui,Quantity,Product,Height] = Tokens,
    NewLocname = convertString(Locname),
    NewMui = convertString(Mui),
    NewQuantity = convertPositiveInteger(Quantity),
    NewProduct = convertString(Product),
    NewHeight = convertPositiveInteger(Height),
    {noreply, reply(220, rfc4627:encode(mypl_server:store_at_location(NewLocname,NewMui,NewQuantity,NewProduct,NewHeight)), reset_buffers(State))};
handle_command("store_at_location_multi", _Parameters, State) ->
    % implementation for store_at_location_multi,
    {ok, NewJsonList, _} = rfc4627:decode(_Parameters),
    {noreply, reply(220, rfc4627:encode(mypl_server:store_at_location_multi(NewJsonList)), reset_buffers(State))};
handle_command("retrieve", _Parameters, State) ->
    % implementation for retrieve,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Mui] = Tokens,
    NewMui = convertString(Mui),
    {noreply, reply(220, rfc4627:encode(mypl_server:retrieve(NewMui)), reset_buffers(State))};
handle_command("init_movement", _Parameters, State) ->
    % implementation for init_movement,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Mui,Locname] = Tokens,
    NewMui = convertString(Mui),
    NewLocname = convertString(Locname),
    {noreply, reply(220, rfc4627:encode(mypl_server:init_movement(NewMui,NewLocname)), reset_buffers(State))};
handle_command("init_movement_to_good_location", _Parameters, State) ->
    % implementation for init_movement_to_good_location,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Mui] = Tokens,
    NewMui = convertString(Mui),
    {noreply, reply(220, rfc4627:encode(mypl_server:init_movement_to_good_location(NewMui)), reset_buffers(State))};
handle_command("commit_movement", _Parameters, State) ->
    % implementation for commit_movement,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [MovementId] = Tokens,
    NewMovementId = convertString(MovementId),
    {noreply, reply(220, rfc4627:encode(mypl_server:commit_movement(NewMovementId)), reset_buffers(State))};
handle_command("rollback_movement", _Parameters, State) ->
    % implementation for rollback_movement,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [MovementId] = Tokens,
    NewMovementId = convertString(MovementId),
    {noreply, reply(220, rfc4627:encode(mypl_server:rollback_movement(NewMovementId)), reset_buffers(State))};
handle_command("commit_retrieval", _Parameters, State) ->
    % implementation for commit_retrieval,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [MovementId] = Tokens,
    NewMovementId = convertString(MovementId),
    {noreply, reply(220, rfc4627:encode(mypl_server:commit_retrieval(NewMovementId)), reset_buffers(State))};
handle_command("rollback_retrieval", _Parameters, State) ->
    % implementation for rollback_retrieval,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [MovementId] = Tokens,
    NewMovementId = convertString(MovementId),
    {noreply, reply(220, rfc4627:encode(mypl_server:rollback_retrieval(NewMovementId)), reset_buffers(State))};
handle_command("init_pick", _Parameters, State) ->
    % implementation for init_pick,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Quantity,Mui] = Tokens,
    NewQuantity = convertPositiveInteger(Quantity),
    NewMui = convertString(Mui),
    {noreply, reply(220, rfc4627:encode(mypl_server:init_pick(NewQuantity,NewMui)), reset_buffers(State))};
handle_command("commit_pick", _Parameters, State) ->
    % implementation for commit_pick,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [PickId] = Tokens,
    NewPickId = convertString(PickId),
    {noreply, reply(220, rfc4627:encode(mypl_server:commit_pick(NewPickId)), reset_buffers(State))};
handle_command("rollback_pick", _Parameters, State) ->
    % implementation for rollback_pick,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [PickId] = Tokens,
    NewPickId = convertString(PickId),
    {noreply, reply(220, rfc4627:encode(mypl_server:rollback_pick(NewPickId)), reset_buffers(State))};
handle_command("correction", _Parameters, State) ->
    % implementation for correction,
    {ok, NewJsonList, _} = rfc4627:decode(_Parameters),
    {noreply, reply(220, rfc4627:encode(mypl_server:correction(NewJsonList)), reset_buffers(State))};
handle_command("update_unit", _Parameters, State) ->
    % implementation for update_unit,
    {ok, NewJsonList, _} = rfc4627:decode(_Parameters),
    {noreply, reply(220, rfc4627:encode(mypl_server:update_unit(NewJsonList)), reset_buffers(State))};
handle_command("count_product", _Parameters, State) ->
    % implementation for count_product,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Product] = Tokens,
    NewProduct = convertString(Product),
    {noreply, reply(220, rfc4627:encode(mypl_server:count_product(NewProduct)), reset_buffers(State))};
handle_command("count_products", _Parameters, State) ->
    % implementation for count_products,
    {noreply, reply(220, rfc4627:encode(mypl_server:count_products()), reset_buffers(State))};
handle_command("unit_list", _Parameters, State) ->
    % implementation for unit_list,
    {noreply, reply(220, rfc4627:encode(mypl_server:unit_list()), reset_buffers(State))};
handle_command("unit_info", _Parameters, State) ->
    % implementation for unit_info,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Mui] = Tokens,
    NewMui = convertString(Mui),
    {noreply, reply(220, rfc4627:encode(mypl_server:unit_info(NewMui)), reset_buffers(State))};
handle_command("location_list", _Parameters, State) ->
    % implementation for location_list,
    {noreply, reply(220, rfc4627:encode(mypl_server:location_list()), reset_buffers(State))};
handle_command("location_info", _Parameters, State) ->
    % implementation for location_info,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Locname] = Tokens,
    NewLocname = convertString(Locname),
    {noreply, reply(220, rfc4627:encode(mypl_server:location_info(NewLocname)), reset_buffers(State))};
handle_command("movement_list", _Parameters, State) ->
    % implementation for movement_list,
    {noreply, reply(220, rfc4627:encode(mypl_server:movement_list()), reset_buffers(State))};
handle_command("movement_info", _Parameters, State) ->
    % implementation for movement_info,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [MovementId] = Tokens,
    NewMovementId = convertString(MovementId),
    {noreply, reply(220, rfc4627:encode(mypl_server:movement_info(NewMovementId)), reset_buffers(State))};
handle_command("pick_list", _Parameters, State) ->
    % implementation for pick_list,
    {noreply, reply(220, rfc4627:encode(mypl_server:pick_list()), reset_buffers(State))};
handle_command("pick_info", _Parameters, State) ->
    % implementation for pick_info,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [PickId] = Tokens,
    NewPickId = convertString(PickId),
    {noreply, reply(220, rfc4627:encode(mypl_server:pick_info(NewPickId)), reset_buffers(State))};
handle_command("find_empty_location_nice", _Parameters, State) ->
    % implementation for find_empty_location_nice,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Height] = Tokens,
    NewHeight = convertPositiveInteger(Height),
    {noreply, reply(220, rfc4627:encode(mypl_server:find_empty_location_nice(NewHeight)), reset_buffers(State))};
handle_command("find_provisioning_candidates", _Parameters, State) ->
    % implementation for find_provisioning_candidates,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Quantity,Product] = Tokens,
    NewQuantity = convertPositiveInteger(Quantity),
    NewProduct = convertString(Product),
    {noreply, reply(220, rfc4627:encode(mypl_server:find_provisioning_candidates(NewQuantity,NewProduct)), reset_buffers(State))};
handle_command("find_provisioning_candidates_multi", _Parameters, State) ->
    % implementation for find_provisioning_candidates_multi,
    {ok, NewJsonList, _} = rfc4627:decode(_Parameters),
    {noreply, reply(220, rfc4627:encode(mypl_server:find_provisioning_candidates_multi(NewJsonList)), reset_buffers(State))};
handle_command("init_provisionings_multi", _Parameters, State) ->
    % implementation for init_provisionings_multi,
    {ok, NewJsonList, _} = rfc4627:decode(_Parameters),
    {noreply, reply(220, rfc4627:encode(mypl_server:init_provisionings_multi(NewJsonList)), reset_buffers(State))};
handle_command("insert_pipeline", _Parameters, State) ->
    % implementation for insert_pipeline,
    {ok, NewJsonList, _} = rfc4627:decode(_Parameters),
    {noreply, reply(220, rfc4627:encode(mypl_server:insert_pipeline(NewJsonList)), reset_buffers(State))};
handle_command("commit_picklist", _Parameters, State) ->
    % implementation for commit_picklist,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [CId] = Tokens,
    NewCId = convertString(CId),
    {noreply, reply(220, rfc4627:encode(mypl_server:commit_picklist(NewCId)), reset_buffers(State))};
handle_command("commit_retrievallist", _Parameters, State) ->
    % implementation for commit_retrievallist,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [CId] = Tokens,
    NewCId = convertString(CId),
    {noreply, reply(220, rfc4627:encode(mypl_server:commit_retrievallist(NewCId)), reset_buffers(State))};
handle_command("get_picklists", _Parameters, State) ->
    % implementation for get_picklists,
    {noreply, reply(220, rfc4627:encode(mypl_server:get_picklists()), reset_buffers(State))};
handle_command("get_retrievallists", _Parameters, State) ->
    % implementation for get_retrievallists,
    {noreply, reply(220, rfc4627:encode(mypl_server:get_retrievallists()), reset_buffers(State))};
handle_command("get_movementlist", _Parameters, State) ->
    % implementation for get_movementlist,
    {noreply, reply(220, rfc4627:encode(mypl_server:get_movementlist()), reset_buffers(State))};
handle_command("provpipeline_info", _Parameters, State) ->
    % implementation for provpipeline_info,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [CId] = Tokens,
    NewCId = convertString(CId),
    {noreply, reply(220, rfc4627:encode(mypl_server:provpipeline_info(NewCId)), reset_buffers(State))};
handle_command("provpipeline_list_new", _Parameters, State) ->
    % implementation for provpipeline_list_new,
    {noreply, reply(220, rfc4627:encode(mypl_server:provpipeline_list_new()), reset_buffers(State))};
handle_command("provpipeline_list_processing", _Parameters, State) ->
    % implementation for provpipeline_list_processing,
    {noreply, reply(220, rfc4627:encode(mypl_server:provpipeline_list_processing()), reset_buffers(State))};
handle_command("provpipeline_list_prepared", _Parameters, State) ->
    % implementation for provpipeline_list_prepared,
    {noreply, reply(220, rfc4627:encode(mypl_server:provpipeline_list_prepared()), reset_buffers(State))};
handle_command("provisioninglist_list", _Parameters, State) ->
    % implementation for provisioninglist_list,
    {noreply, reply(220, rfc4627:encode(mypl_server:provisioninglist_list()), reset_buffers(State))};
handle_command("provisioninglist_info", _Parameters, State) ->
    % implementation for provisioninglist_info,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [CId] = Tokens,
    NewCId = convertString(CId),
    {noreply, reply(220, rfc4627:encode(mypl_server:provisioninglist_info(NewCId)), reset_buffers(State))};
handle_command("delete_pipeline", _Parameters, State) ->
    % implementation for delete_pipeline,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [CId] = Tokens,
    NewCId = convertString(CId),
    {noreply, reply(220, rfc4627:encode(mypl_server:delete_pipeline(NewCId)), reset_buffers(State))};
handle_command("update_pipeline", _Parameters, State) ->
    % implementation for update_pipeline,
    {ok, NewJsonList, _} = rfc4627:decode(_Parameters),
    {noreply, reply(220, rfc4627:encode(mypl_server:update_pipeline(NewJsonList)), reset_buffers(State))};
handle_command("get_articleaudit", _Parameters, State) ->
    % implementation for get_articleaudit,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Product] = Tokens,
    NewProduct = convertString(Product),
    {noreply, reply(220, rfc4627:encode(mypl_server:get_articleaudit(NewProduct)), reset_buffers(State))};
handle_command("get_unitaudit", _Parameters, State) ->
    % implementation for get_unitaudit,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Mui] = Tokens,
    NewMui = convertString(Mui),
    {noreply, reply(220, rfc4627:encode(mypl_server:get_unitaudit(NewMui)), reset_buffers(State))};
handle_command("get_articlecorrection", _Parameters, State) ->
    % implementation for get_articlecorrection,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Product] = Tokens,
    NewProduct = convertString(Product),
    {noreply, reply(220, rfc4627:encode(mypl_server:get_articlecorrection(NewProduct)), reset_buffers(State))};
handle_command("get_recent_from_archive", _Parameters, State) ->
    % implementation for get_recent_from_archive,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Type] = Tokens,
    NewType = convertString(Type),
    {noreply, reply(220, rfc4627:encode(mypl_server:get_recent_from_archive(NewType)), reset_buffers(State))};
handle_command("get_abc", _Parameters, State) ->
    % implementation for get_abc,
    {noreply, reply(220, rfc4627:encode(mypl_server:get_abc()), reset_buffers(State))};
handle_command("get_abcclass", _Parameters, State) ->
    % implementation for get_abcclass,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Product] = Tokens,
    NewProduct = convertString(Product),
    {noreply, reply(220, rfc4627:encode(mypl_server:get_abcclass(NewProduct)), reset_buffers(State))};
handle_command("make_oid", _Parameters, State) ->
    % implementation for make_oid,
    {noreply, reply(220, rfc4627:encode(mypl_server:make_oid()), reset_buffers(State))};
handle_command("make_nve", _Parameters, State) ->
    % implementation for make_nve,
    {noreply, reply(220, rfc4627:encode(mypl_server:make_nve()), reset_buffers(State))};
handle_command("dump_requests", _Parameters, State) ->
    % implementation for dump_requests,
    {noreply, reply(220, rfc4627:encode(mypl_server:dump_requests()), reset_buffers(State))};
handle_command("feed_eap", _Parameters, State) ->
    % implementation for feed_eap,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Product,Prod_ve1,Prod_exportpackage,Export_pallet,Prod_x,Prod_y,Prod_z,Prod_g,Ve1_x,Ve1_y,Ve1_z,Ve1_g,Export_x,Export_y,Export_z,Export_g] = Tokens,
    NewProduct = convertString(Product),
    NewProd_ve1 = convertPositiveInteger(Prod_ve1),
    NewProd_exportpackage = convertPositiveInteger(Prod_exportpackage),
    NewExport_pallet = convertPositiveInteger(Export_pallet),
    NewProd_x = convertPositiveInteger(Prod_x),
    NewProd_y = convertPositiveInteger(Prod_y),
    NewProd_z = convertPositiveInteger(Prod_z),
    NewProd_g = convertPositiveInteger(Prod_g),
    NewVe1_x = convertPositiveInteger(Ve1_x),
    NewVe1_y = convertPositiveInteger(Ve1_y),
    NewVe1_z = convertPositiveInteger(Ve1_z),
    NewVe1_g = convertPositiveInteger(Ve1_g),
    NewExport_x = convertPositiveInteger(Export_x),
    NewExport_y = convertPositiveInteger(Export_y),
    NewExport_z = convertPositiveInteger(Export_z),
    NewExport_g = convertPositiveInteger(Export_g),
    {noreply, reply(220, rfc4627:encode(mypl_server:feed_eap(NewProduct,NewProd_ve1,NewProd_exportpackage,NewExport_pallet,NewProd_x,NewProd_y,NewProd_z,NewProd_g,NewVe1_x,NewVe1_y,NewVe1_z,NewVe1_g,NewExport_x,NewExport_y,NewExport_z,NewExport_g)), reset_buffers(State))};
handle_command("statistics", _Parameters, State) ->
    % implementation for statistics,
    {noreply, reply(220, rfc4627:encode(mypl_server:statistics()), reset_buffers(State))};
handle_command("bewegungen", _Parameters, State) ->
    % implementation for bewegungen,
    {noreply, reply(220, rfc4627:encode(mypl_server:bewegungen()), reset_buffers(State))};

handle_command("help", _Parameters, State) -> {noreply, reply(220, "Help follows:
backup 
init_location Locname,Height,Floorlevel,Preference,Info,Attributes
store_at_location Locname,Mui,Quantity,Product,Height
store_at_location_multi JsonList
retrieve Mui
init_movement Mui,Locname
init_movement_to_good_location Mui
commit_movement MovementId
rollback_movement MovementId
commit_retrieval MovementId
rollback_retrieval MovementId
init_pick Quantity,Mui
commit_pick PickId
rollback_pick PickId
correction JsonList
update_unit JsonList
count_product Product
count_products 
unit_list 
unit_info Mui
location_list 
location_info Locname
movement_list 
movement_info MovementId
pick_list 
pick_info PickId
find_empty_location_nice Height
find_provisioning_candidates Quantity,Product
find_provisioning_candidates_multi JsonList
init_provisionings_multi JsonList
insert_pipeline JsonList
commit_picklist CId
commit_retrievallist CId
get_picklists 
get_retrievallists 
get_movementlist 
provpipeline_info CId
provpipeline_list_new 
provpipeline_list_processing 
provpipeline_list_prepared 
provisioninglist_list 
provisioninglist_info CId
delete_pipeline CId
update_pipeline JsonList
get_articleaudit Product
get_unitaudit Mui
get_articlecorrection Product
get_recent_from_archive Type
get_abc 
get_abcclass Product
make_oid 
make_nve 
dump_requests 
feed_eap Product,Prod_ve1,Prod_exportpackage,Export_pallet,Prod_x,Prod_y,Prod_z,Prod_g,Ve1_x,Ve1_y,Ve1_z,Ve1_g,Export_x,Export_y,Export_z,Export_g
statistics 
bewegungen ", reset_buffers(State))};
handle_command("quit", _ClientDomain, State) -> {stop, normal, reply(201, "Goodbye", reset_buffers(State))};
handle_command(Command, _Parameters, State) -> {noreply, reply(500, "Unsupported command " ++ Command, State)}.

%%%%%%%%%% autogenerated code ends
