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
handle_command("get_picklists", _Parameters, State) ->
    % implementation for get_picklists,
    {noreply, reply(220, rfc4627:encode(mypl_server:get_picklists()), reset_buffers(State))};
handle_command("get_retrievallists", _Parameters, State) ->
    % implementation for get_retrievallists,
    {noreply, reply(220, rfc4627:encode(mypl_server:get_retrievallists()), reset_buffers(State))};
handle_command("get_movementlist", _Parameters, State) ->
    % implementation for get_movementlist,
    {noreply, reply(220, rfc4627:encode(mypl_server:get_movementlist()), reset_buffers(State))};
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
handle_command("commit_movementlist", _Parameters, State) ->
    % implementation for commit_movementlist,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [CId] = Tokens,
    NewCId = convertString(CId),
    {noreply, reply(220, rfc4627:encode(mypl_server:commit_movementlist(NewCId)), reset_buffers(State))};
handle_command("is_provisioned", _Parameters, State) ->
    % implementation for is_provisioned,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [CId] = Tokens,
    NewCId = convertString(CId),
    {noreply, reply(220, rfc4627:encode(mypl_server:is_provisioned(NewCId)), reset_buffers(State))};
handle_command("provpipeline_list_new", _Parameters, State) ->
    % implementation for provpipeline_list_new,
    {noreply, reply(220, rfc4627:encode(mypl_server:provpipeline_list_new()), reset_buffers(State))};
handle_command("provpipeline_list_processing", _Parameters, State) ->
    % implementation for provpipeline_list_processing,
    {noreply, reply(220, rfc4627:encode(mypl_server:provpipeline_list_processing()), reset_buffers(State))};
handle_command("get_articleaudit", _Parameters, State) ->
    % implementation for get_articleaudit,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Product] = Tokens,
    NewProduct = convertString(Product),
    {noreply, reply(220, rfc4627:encode(mypl_server:get_articleaudit(NewProduct)), reset_buffers(State))};
handle_command("get_abc", _Parameters, State) ->
    % implementation for get_abc,
    {noreply, reply(220, rfc4627:encode(mypl_server:get_abc()), reset_buffers(State))};
handle_command("make_oid", _Parameters, State) ->
    % implementation for make_oid,
    {noreply, reply(220, rfc4627:encode(mypl_server:make_oid()), reset_buffers(State))};
handle_command("make_nve", _Parameters, State) ->
    % implementation for make_nve,
    {noreply, reply(220, rfc4627:encode(mypl_server:make_nve()), reset_buffers(State))};

handle_command("help", _Parameters, State) -> {noreply, reply(220, "Help follows:
backup 
init_location Locname,Height,Floorlevel,Preference,Info,Attributes
store_at_location Locname,Mui,Quantity,Product,Height
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
find_provisioning_candidates Quantity,Product
find_provisioning_candidates_multi JsonList
init_provisionings_multi JsonList
insert_pipeline JsonList
get_picklists 
get_retrievallists 
get_movementlist 
commit_picklist CId
commit_retrievallist CId
commit_movementlist CId
is_provisioned CId
provpipeline_list_new 
provpipeline_list_processing 
get_articleaudit Product
get_abc 
make_oid 
make_nve ", reset_buffers(State))};
handle_command("quit", _ClientDomain, State) -> {stop, normal, reply(201, "Goodbye", reset_buffers(State))};
handle_command(Command, _Parameters, State) -> {noreply, reply(500, "Unsupported command " ++ Command, State)}.

%%%%%%%%%% autogenerated code ends
