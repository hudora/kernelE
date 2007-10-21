%% @version 0.1
%% @copyright 2007 HUDORA GmbH
%% @author Maximillian Dornseif <md@hudora.de>
%% @doc audit log for myPL/kernel-E
%%
%% This implements a audit log for all changes in inventory and for all movements of Units/NVEs/SSCCs.
%% articleaudit documents changes in inventory whereas unitaudit documents all movements.
%%
%% kernel-E saves two kind of audit logs: changes of stock of inventory and
%% creation, movement and removal of Units/MUIs/NVEs. It is expected that
%% higher layers implement their own logging and that kernel-E logs are only
%% used for debugging.
%%
%% The stock of inventory audit log (called "articleaudit") saves (quantity,
%% product date) and possibly additional references. By consulting the
%% articleaudit log we should always be able to calculate the current amount
%% of goods in stock. All operations changing stock of inventory are coupled
%% with the generation of  articleaudit entries in an transaction and thus
%% should succeed or fail always booth - even in the presence of software
%% errors.
%%
%% The "unitaudit" log documents all creation, movement and disbandment of
%% Units/MUIs/NVEs. Log entries contain a timestamp, the Unit-ID, the quantity
%% and the product on the unit and possible additional references.
%%
%% @TODO add functionality to read the auditlogs
%% @end

-module(mypl_audit).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

-export([articleaudit/6, articleaudit/5, articleaudit/4, unitaudit/4, unitaudit/3, unitaudit/2,
         compress_audit/0, compress_audit/1]).

%%% we assume all test and initialisation functionality is provided vby other modules

%%%
%%% auditlog - to be called whenever goods enter or leave the warehouse
%%%

%% @private
%% @spec articleaudit(integer(), product(), string(), muiID(), string(), externalReferences()) -> {ok, atomic}
%% @doc dump information about changes of stock.
%%
%% Transaction can be an movementID or an pickID.
articleaudit(Quantity, Product, Text, Mui, Transaction, References) ->
    Fun = fun() ->
            mnesia:write(#articleaudit{id="a" ++ mypl_util:oid(), quantity=Quantity, product=Product,
                                   text=Text, mui=Mui, transaction=Transaction,
                                   references=References, created_at=calendar:universal_time()})
          end,
    mnesia:transaction(Fun).

%% @private
%% @spec articleaudit(integer(), product(), string(), muiID(), all()) -> {ok, atomic}
%% @doc dump information about changes of stock
articleaudit(Quantity, Product, Text, Mui, Transaction) ->
    articleaudit(Quantity, Product, Text, Mui, Transaction, []).

%% @private
%% @spec articleaudit(integer(), product(), string(), muiID()) -> {ok, atomic}
%% @doc dump information about changes of stock
articleaudit(Quantity, Product, Text, Mui) ->
    articleaudit(Quantity, Product, Text, Mui, undefined).

%% @private
%% @spec unitaudit(unitRecord(), string(), string(), externalReferences()) -> {ok, atomic}
%% @doc to be called whenever Units are moved in the warehouse.
unitaudit(Unit, Text, Transaction, References) ->
    Fun = fun() ->
            mnesia:write(#unitaudit{id="A" ++ mypl_util:oid(),
                                    mui=Unit#unit.mui, quantity=Unit#unit.quantity, product=Unit#unit.product,
                                    text=Text, transaction=Transaction,
                                    references=References, created_at=calendar:universal_time()})
          end,
    mnesia:transaction(Fun).

%% @spec unitaudit(unitRecord(), string(), string()) -> {ok, atomic}
%% @doc to be called whenever Units are moved in the warehouse.
unitaudit(Unit, Text, Transaction) ->
    unitaudit(Unit, Text, Transaction, []).

%% @spec unitaudit(unitRecord(), string()) -> {ok, atomic}
%% @doc to be called whenever Units are moved in the warehouse.
unitaudit(Unit, Text) ->
    unitaudit(Unit, Text, undefined).

compress_articleaudit_helper(Quantity, []) ->
    Quantity;
compress_articleaudit_helper(Quantity, [Record|T]) ->
    NewQuantity = Quantity + Record#articleaudit.quantity,
    ok = mnesia:delete({articleaudit, Record#articleaudit.id}),
    compress_articleaudit_helper(NewQuantity, T).
    
compress_articleaudit(Product, BeforeDate) ->
    BeforeDays = calendar:date_to_gregorian_days(BeforeDate),
    Records = mypl_db_util:do(qlc:q([X || X <- mnesia:table(articleaudit),
                                          X#articleaudit.product =:= Product,
                                          X#articleaudit.text /= "Zusammenfassung vorheriger Einträge",
                                          BeforeDays > calendar:date_to_gregorian_days(element(1, X#articleaudit.created_at))])),
    case Records of
        [] ->
            nothing_to_do;
        Records1 ->
            Fun = fun() ->
                Quantity = compress_articleaudit_helper(0, Records1),
                articleaudit(Quantity, Product, "Zusammenfassung vorheriger Einträge", undefined),
                {Quantity, Product}
            end,
            {atomic, Ret} = mnesia:transaction(Fun),
            Ret
    end.
    

compress_articleaudit(BeforeDate) ->
    Products = lists:usort(mypl_db_util:do(qlc:q([X#articleaudit.product || X <- mnesia:table(articleaudit)]))),
    lists:map(fun(Product) -> compress_articleaudit(Product, BeforeDate) end, Products).
    

compress_unitaudit_helper([]) -> ok;
compress_unitaudit_helper([Record|T]) ->
    % TODO: possibly check if the unit stil exists in the warehouse
    ok = mnesia:delete({unitaudit, Record#unitaudit.id}),
    compress_unitaudit_helper(T).

compress_unitaudit(BeforeDate) ->
    BeforeDays = calendar:date_to_gregorian_days(BeforeDate),
    Records = mypl_db_util:do(qlc:q([X|| X <- mnesia:table(unitaudit),
                                         BeforeDays > calendar:date_to_gregorian_days(element(1, X#unitaudit.created_at))])),
    Fun = fun() ->
        compress_unitaudit_helper(Records)
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

compress_audit(BeforeDate) ->
    compress_unitaudit(BeforeDate),
    compress_articleaudit(BeforeDate).

compress_audit() ->
    {Date, _} = calendar:now_to_datetime(erlang:now()),
    BeforeDate = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 180),
    compress_audit(BeforeDate).
    

