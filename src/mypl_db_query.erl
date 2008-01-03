%% @version 0.2
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc myPL/kernel-E storage Engine Query Interface
%%
%% This implements queries related to mypl_db.
%%
%% Counting functionality uses four ways to count:
%% <dl>
%% <dt>full_quantity</dt>      <dd>is the quantity of Products "in the Books" (Buchbestand)</dd>
%% <dt>available_quantity</dt> <dd>is the quantity of Products currently involved in nothing</dd>
%% <dt>pick_quantity</dt>      <dd>is the quantity of Products reserved for a Pick</dd>
%% <dt>movement_quantity</dt>  <dd>is the quantity of Products currently involved in a Movement</dd>
%% </dl>
%%
%% So
%% <ul>
%% <li>available_quantity = full_quantity - pick_quantity - movement_quantity</li>
%% <li>full_quantity = SUM(Unit.quantity)</li>
%% <li>pick_quantity = SUM(Unit.pick_quantity)</li>
%% <li>movement_quantity = SUM(Unit.quantity) WHERE is_moving(Unit)</li>
%% </ul>
%%
%% As you see `full_quantity' and `pick_quantity' are natively stored in the database.
%% `movement_quantity' is based on the `full_quantity' of Units that are involved in a Movement.
%% `available_quantity' is caclulated on the fly based on the other three.
%% @end

-module(mypl_db_query).

-include_lib("stdlib/include/qlc.hrl").
-include("include/mypl.hrl").

-import(mypl_db_util).

-export([count_product/1, count_products/0,
         open_movements_for_product/1, find_floor_units_for_product/1,
         unit_list/0, unit_info/1, location_list/0, location_info/1,
         movement_list/0, movement_info/1, pick_list/0, pick_info/1]).

% @private
count_product_helper([], Fquantity, Pquantity, Mquantity) -> 
    {Fquantity, Fquantity - Pquantity - Mquantity, Pquantity, Mquantity};
count_product_helper(Units, Fquantity, Pquantity, Mquantity) ->
    [P|T] = Units,
    case mypl_db_util:unit_moving(P) of
        no ->
            count_product_helper(T, P#unit.quantity+Fquantity, P#unit.pick_quantity+Pquantity, Mquantity);
        yes -> 
            count_product_helper(T, P#unit.quantity+Fquantity, P#unit.pick_quantity+Pquantity, P#unit.quantity+Mquantity)
    end.

%% @spec count_product(Product) -> 
%%     {{Full_quantity, Available_quantity, Pick_quantity, Movement_quantity}, [muID()]}
%% @doc finds out what of a product is available. Besides the list of MUIs where the Product is stored
%% four quantities Full_quantity, Available_quantity, Pick_quantity, Movement_quantity are returned.
%% 
%% E.g. count_product("10001") -> {{ 36, 19, 0, 17}, ["NVE031233412431234", "NVE0313443215435435"]}
count_product(Product) ->
    Fun = fun() ->
        Units = mypl_db_util:do(qlc:q([X || X <- mnesia:table(unit), X#unit.product =:= Product])),
        {count_product_helper(Units, 0, 0, 0), lists:map(fun(X) -> X#unit.mui end, Units)}
    end,
    mypl_db_util:transaction(Fun).
    

% @private
% count_products() -> [{product, quantities}, ...]
count_products_helper([], Fdict, Pdict, Mdict) ->
    lists:map(fun({K, V}) -> {K,
                              V, % equals dict:fetch(K, Fdict)
                              V-dict:fetch(K, Pdict)-dict:fetch(K, Mdict),
                              dict:fetch(K, Pdict),
                              dict:fetch(K, Mdict)} end, dict:to_list(Fdict));
count_products_helper(Units, Fdict, Pdict, Mdict) ->
    [P|T] = Units,
    case mypl_db_util:unit_moving(P) of
        no ->
            count_products_helper(T, dict:update_counter(P#unit.product, P#unit.quantity, Fdict),
                                     dict:update_counter(P#unit.product, P#unit.pick_quantity, Pdict),
                                     dict:update_counter(P#unit.product, 0, Mdict));
        yes -> 
            count_products_helper(T, dict:update_counter(P#unit.product, P#unit.quantity, Fdict),
                                     dict:update_counter(P#unit.product, P#unit.pick_quantity, Pdict),
                                     dict:update_counter(P#unit.product, P#unit.quantity, Mdict))
    end.
%% @spec count_products() -> 
%%     [{Product, Full_quantity, Available_quantity, Pick_quantity, Movement_quantity}]
%% @see count_product
%% @doc Counts all Products in the Warehouse and Returns a list of 
%% Full_quantity, Available_quantity, Pick_quantity, Movement_quantity for all Products.
%%
%% E.g. count_products() -> [{"10001",10,10,0,0},{"10002",36,1,18,17},{"10003",94,94,0,0}]
count_products() ->
    Fun = fun() ->
        Units = mypl_db_util:do(qlc:q([X || X <- mnesia:table(unit)])),
        count_products_helper(Units, dict:new(), dict:new(), dict:new())
    end,
    lists:sort(mypl_db_util:transaction(Fun)).
    

%% @spec open_movements_for_product(string()) -> [mypl_db:movementID()]
%% @doc returns a list of all open movements for a Product.
open_movements_for_product(Product) ->
    {_, Muis} = count_product(Product),
    Fun = fun() ->
        [X#movement.id || X <- lists:map(fun(X) -> 
                                             mypl_db_util:unit_movement(mypl_db_util:mui_to_unit(X))
                                         end, Muis), X /= false]
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

find_floor_units_for_product(Product) ->
    [X || X <- mypl_db_util:do(qlc:q([X || X <- mnesia:table(unit),
                                           X#unit.product =:= Product, unit_floor_helper(X)]))].

unit_floor_helper(Unit) ->
     Loc = mypl_db_util:read_location(Unit#unit.location),
     Loc#location.floorlevel =:= true.
    

%% @spec unit_list() -> [mypl_db:muiId()]
%% @doc Get a list of all unit IDs
unit_list() ->
    {atomic, Ret} = mnesia:transaction(fun() -> mnesia:all_keys(unit) end),
    Ret.
    

%% @spec unit_info(muiID()) -> tuple()
%% @doc gets a tuple with information concerning a unit
unit_info(Mui) -> 
    Fun = fun() ->
        case mypl_db_util:mui_to_unit_trans(Mui) of
            {error, Reason, Info} ->
                {error, Reason, Info};
            Unit ->
                case mypl_db_util:unit_movement(Unit) of
                    false ->
                        Movements = [];
                    Movement ->
                        Movements = [Movement#movement.id]
                end,
                PickIds  = mypl_db_util:do(qlc:q([X#pick.id || X <- mnesia:table(pick), X#pick.from_unit =:= Mui])),
                {ok,
                 {{mui ,           Unit#unit.mui},
                  {quantity,       Unit#unit.quantity},
                  {product,        Unit#unit.product},
                  {height,         Unit#unit.height},
                  {pick_quantity,  Unit#unit.pick_quantity},
                  {location,       Unit#unit.location},
                  {created_at,     Unit#unit.created_at},
                  {attributes,     []},
                  {movements,      Movements},
                  {picks,          PickIds}
                 }
                }
        end
    end,
    mypl_db_util:transaction(Fun).
    

%% @doc Get a list of all location names
location_list() ->
    lists:sort(mypl_db_util:transaction(fun() -> mnesia:all_keys(location) end)).
    

%% @spec location_info(locationName()) -> tuple()
%% @doc gets a tuple with information concerning a location
location_info(Locname) -> 
    Fun = fun() ->
        case mypl_db_util:read_location(Locname) of
            unknown_location ->
                {error, unknown_location};
            Location ->
                {ok, 
                 {{name,          Location#location.name},
                  {height,        Location#location.height},
                  {floorlevel,    Location#location.floorlevel},
                  {preference,    Location#location.preference},
                  {info,          Location#location.info},
                  {attributes,    Location#location.attributes},
                  {allocated_by,  Location#location.allocated_by},
                  {reserved_for,  Location#location.reserved_for}
                 }}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

%% @spec movement_list() -> [mypl_db:movementID()]
%% @doc gets a List with all movement IDs
movement_list() ->
    lists:sort(mypl_db_util:transaction(fun() -> mnesia:all_keys(movement) end)).
    

%% @spec movement_info(movementId()) -> {ok, [Attribute]}
%%      Attribute = {Name::string(), Value::term()}
%% @doc gets a tuple with information concerning a movement.
%%
%% Attributes returned are id, mui, from_location, to_location, attributes, created_at, quantity and product.

movement_info(MovementId) -> 
    Fun = fun() ->
        case mnesia:read({movement, MovementId}) of
            [] -> 
                % not found in the active database - check archive
                case mypl_audit:get_from_archive(movement, MovementId) of
                    [] ->
                        {error, unknown_movement, {MovementId}};
                    [Movement] ->
                        Proplist = movement_info_helper(Movement),
                        {ok, Proplist ++ [{status, archived}]}
                end;
            [Movement] ->
                Proplist = movement_info_helper(Movement),
                {ok, Proplist ++ [{status, open}]}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

movement_info_helper(Movement) ->
    Unit = mypl_db_util:mui_to_unit(Movement#movement.mui),
    Quantity = Unit#unit.quantity,
    Product = Unit#unit.product,
    [{id ,            Movement#movement.id},
     {mui,            Movement#movement.mui},
     {from_location,  Movement#movement.from_location},
     {to_location,    Movement#movement.to_location},
     {attributes,     Movement#movement.attributes},
     {created_at,     Movement#movement.created_at},
     {quantity,       Quantity},
     {product,        Product}
    ].


%% @spec pick_list() -> [PickID]
%% @doc gets a List with all pick_list IDs
pick_list() ->
    {atomic, Ret} = mnesia:transaction(fun() -> mnesia:all_keys(pick) end),
    Ret.
    

%% @spec pick_info(pickId()) -> {ok, [Attribute]}
%%      Attribute = {Name::string(), Value::term()}
%% @doc gets a proplist with information concerning a pick
pick_info(PickId) -> 
    Fun = fun() ->
        case mnesia:read({pick, PickId}) of
            [] ->
                % not found in the active database - check archive
                case mypl_audit:get_from_archive(pick, PickId) of
                    [] ->
                        {error, unknown_pick, {PickId}};
                    [Pick] -> 
                        Proplist = pick_info_helper(Pick),
                        {ok, Proplist ++ [{status, open}]}
                end;
            [Pick] ->
                Proplist = pick_info_helper(Pick),
                {ok, Proplist ++ [{status, open}]}
        end
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

pick_info_helper(Pick) -> 
    Unit = mypl_db_util:mui_to_unit(Pick#pick.from_unit),
    [{id ,           Pick#pick.id},
     {from_unit,     Pick#pick.from_unit},
     {from_location, Unit#unit.location},
     {quantity,      Pick#pick.quantity},
     {product,       Unit#unit.product},
     {attributes,    Pick#pick.attributes},
     {created_at,    Pick#pick.created_at}
    ].
    


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
    % regenerate locations
    % init_location(Name, Height, Floorlevel, Preference, Attributes)
    mypl_db:init_location("EINLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("AUSLAG", 6000, true,  0, [{no_picks}]),
    mypl_db:init_location("010101", 2000, true,  6, []),
    mypl_db:init_location("010102", 1950, false, 6, []),
    mypl_db:init_location("010103", 1200, false, 5, []),
    mypl_db:init_location("010201", 2000, true,  7, []),
    ok.

%%% @hidden
%%% test if counting works as expected
mypl_simple_counting_test() ->
    test_init(),
    {error, unknown_movement, {"gibtsnicht"}} = movement_info("gibtsnicht"),
    Mui3 = mypl_util:generate_mui(),
    Mui4 = mypl_util:generate_mui(),
    Mui6 = mypl_util:generate_mui(),
    {ok, _} = mypl_db:store_at_location("EINLAG", mypl_util:generate_mui(),  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("010101", mypl_util:generate_mui(),  5, "a0003", 1200),
    {ok, _} = mypl_db:store_at_location("EINLAG", Mui3, 17, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("010102", Mui4, 19, "a0004", 1200),
    {ok, _} = mypl_db:store_at_location("EINLAG", mypl_util:generate_mui(), 23, "a0005", 1200),
    {ok, _} = mypl_db:store_at_location("010103", Mui6, 71, "a0005", 1200),
    % Full_quantity, Available_quantity, Pick_quantity, Movement_quantity
    {{ 10, 10,  0, 0}, _} = count_product("a0003"),
    {{ 36, 36,  0, 0}, _} = count_product("a0004"),
    {{ 94, 94,  0, 0}, _} = count_product("a0005"),
    {{  0,  0,  0, 0}, _} = count_product("a0006"),
    [{"a0003",10,10,0,0},{"a0004",36,36,0,0},{"a0005",94,94,0,0}] = count_products(),
    
    % now test while stuff is moving
    {ok, Pick1} = mypl_db:init_pick(29, Mui6),
    {{ 94, 65, 29, 0}, _} = count_product("a0005"),
    [{"a0003",10,10,0,0},{"a0004",36,36,0,0},{"a0005",94,65,29,0}] = count_products(),
    mypl_db:rollback_pick(Pick1),
    {{ 94, 94,  0, 0}, _} = count_product("a0005"),
    [{"a0003",10,10,0,0},{"a0004",36,36,0,0},{"a0005",94,94,0,0}] = count_products(),
    
    {{ 36, 36, 0,  0}, _} = count_product("a0004"),
    {ok, Movement3} = mypl_db:init_movement(Mui3, "010102"),
    [Movement3] = movement_list(),
    {{ 36, 19, 0, 17}, _} = count_product("a0004"),
    [{"a0003",10,10,0,0},{"a0004",36,19,0,17},{"a0005",94,94,0,0}] = count_products(),
    {ok, Pick2} = mypl_db:init_pick(18, Mui4),
    {{ 36, 1, 18, 17}, _} = count_product("a0004"),
    [{"a0003",10,10,0,0},{"a0004",36,1,18,17},{"a0005",94,94,0,0}] = count_products(),
    mypl_db:commit_pick(Pick2),
    mypl_db:rollback_movement(Movement3),
    [] = movement_list(),
    
    [{"a0003",10,10,0,0},{"a0004",18,18,0,0},{"a0005",94,94,0,0}] = count_products(),
    {ok, Movement4} = mypl_db:init_movement(Mui3, "010102"),
    [Movement4] = open_movements_for_product("a0004"),
    [{"a0003",10,10,0,0},{"a0004",18,1,0,17},{"a0005",94,94,0,0}] = count_products(),
    mypl_db:commit_movement(Movement4),
    [] = open_movements_for_product("a0004"),
    [{"a0003",10,10,0,0},{"a0004",18,18,0,0},{"a0005",94,94,0,0}] = count_products(),
    
    {error,unknown_location} = location_info("GIBTSNICHT"),
    ok.

testrunner() ->
    mypl_simple_counting_test().
    
-endif.
