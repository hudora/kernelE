%%%%%%%%%% autogenerated code follows;
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
handle_command("location_info", _Parameters, State) ->
    % implementation for location_info,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Locname] = Tokens,
    NewLocname = convertString(Locname),
    {noreply, reply(220, rfc4627:encode(mypl_server:location_info(NewLocname)), reset_buffers(State))};
handle_command("location_list", _Parameters, State) ->
    % implementation for location_list,
    {noreply, reply(220, rfc4627:encode(mypl_server:location_list()), reset_buffers(State))};
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
handle_command("retrive", _Parameters, State) ->
    % implementation for retrive,
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(_Parameters, [$,])),
    [Mui] = Tokens,
    NewMui = convertString(Mui),
    {noreply, reply(220, rfc4627:encode(mypl_server:retrive(NewMui)), reset_buffers(State))};
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
handle_command("create_automatic_movements", _Parameters, State) ->
    % implementation for create_automatic_movements,
    {noreply, reply(220, rfc4627:encode(mypl_server:create_automatic_movements()), reset_buffers(State))};

handle_command("help", _Parameters, State) -> {noreply, reply(220, "Help follows:
init_location Locname,Height,Floorlevel,Preference,Info,Attributes
location_info Locname
location_list 
store_at_location Locname,Mui,Quantity,Product,Height
retrive Mui
init_movement Mui,Locname
init_movement_to_good_location Mui
commit_movement MovementId
rollback_movement MovementId
init_pick Quantity,Mui
commit_pick PickId
rollback_pick PickId
count_product Product
count_products 
find_provisioning_candidates Quantity,Product
find_provisioning_candidates_multi JsonList
init_provisionings_multi JsonList
create_automatic_movements ", reset_buffers(State))};
handle_command("quit", _ClientDomain, State) -> {stop, normal, reply(201, "Goodbye", reset_buffers(State))};
handle_command(Command, _Parameters, State) -> {noreply, reply(500, "Unsupported command " ++ Command, State)}.

%%%%%%%%%% autogenerated code ends
