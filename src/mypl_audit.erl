%% @version 0.2
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
%% After a very short delay the unitaudit data is written to a couchdb database
%% and removed from kernelE See mypl_audit_transfer.erl
%%
%% @end

-module(mypl_audit).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

-export([run_me_once/0, get_articleaudit/1, get_unitaudit/1, get_articlecorrection/1,
         articleaudit/6, articleaudit/5, articleaudit/4,
         unitaudit_mui/2, unitaudit/4, unitaudit/3, unitaudit/2,
         kommiauftragaudit/4,
         archive/2, 
         get_from_archive/2, get_recent_from_archive/1]).

%%% we assume all test and initialisation functionality is provided vby other modules

%%%
%%% auditlog - to be called whenever goods enter or leave the warehouse
%%%

-spec run_me_once() -> 'ok'.
run_me_once() ->
    % Tables kept in RAM with disk based backing
    mypl_db:init_table_info(mnesia:create_table(articleaudit, [{disc_copies, [node()]}, {attributes, record_info(fields, articleaudit)}]), articleaudit),
    mnesia:add_table_index(articleaudit, #articleaudit.product),
    mypl_db:init_table_info(mnesia:create_table(archive, [{disc_copies, [node()]}, {attributes, record_info(fields, archive)}]), archive),
    mnesia:add_table_index(archive, #archive.type),
    mnesia:add_table_index(archive, #archive.body_id),
    mypl_db:init_table_info(mnesia:create_table(unitaudit, [{disc_copies, [node()]}, {attributes, record_info(fields, unitaudit)}]), unitaudit),
    mnesia:add_table_index(unitaudit, #unitaudit.mui),
    mypl_db:init_table_info(mnesia:create_table(kommiauftragaudit, [{disc_copies, [node()]}, {attributes, record_info(fields, kommiauftragaudit)}]), kommiauftragaudit),
    mnesia:add_table_index(kommiauftragaudit, #kommiauftragaudit.komminr),
    mnesia:add_table_index(kommiauftragaudit, #kommiauftragaudit.auftrnr),
    ok.


%% @doc returns all audit records related to an article
%% @deprecated
get_articleaudit(Product) ->
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(articleaudit),
                                                X#articleaudit.product =:= Product])),
    lists:map(fun(X) -> get_articleaudit_helper(X) end, lists:sort(Records)).
    

%% @private
%% @deprecated
get_articleaudit_helper(Aaudit) ->
    [
     {quantity,    Aaudit#articleaudit.quantity},
     {product,     Aaudit#articleaudit.product},
     {text,        Aaudit#articleaudit.text},
     {mui,         Aaudit#articleaudit.mui},
     {transaction, Aaudit#articleaudit.transaction},
     {references,  Aaudit#articleaudit.references},
     {created_at,  Aaudit#articleaudit.created_at}
    ].
    
%% @doc returns a list of corrections for an article
get_articlecorrection(Product) ->
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(correction),
                                                X#correction.product =:= Product])),
    lists:map(fun(X) -> get_articlecorrection_helper(X) end, lists:sort(Records)).
    

%% @private
get_articlecorrection_helper(Correction) ->
    [
     {id,               Correction#correction.id},
     {old_quantity,     Correction#correction.old_quantity},
     {change_quantity,  Correction#correction.change_quantity},
     {product,          Correction#correction.product},
     {mui,              Correction#correction.mui},
     {location,         Correction#correction.location},
     {text,             Correction#correction.text},
     {created_at,       Correction#correction.created_at}
    ] ++ Correction#correction.attributes.
    

%% @doc returns all audit recodes related to an Mui
get_unitaudit(Mui) ->
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(unitaudit),
                                                X#unitaudit.mui =:= Mui])),
    lists:map(fun(X) -> get_unitaudit_helper(X) end, lists:sort(Records)).
    

%% @private
get_unitaudit_helper(Uaudit) ->
    [
     {quantity,    Uaudit#unitaudit.quantity},
     {product,     Uaudit#unitaudit.product},
     {text,        Uaudit#unitaudit.text},
     {mui,         Uaudit#unitaudit.mui},
     {transaction, Uaudit#unitaudit.transaction},
     {references,  Uaudit#unitaudit.references},
     {created_at,  Uaudit#unitaudit.created_at}
    ].
    

%% @private
%% @spec articleaudit(integer(), product(), string(), muiID(), string(), externalReferences()) -> {ok, atomic}
%% @doc dump information about changes of stock.
%%
%% Transaction can be an movementID or an pickID.
-spec articleaudit(integer(),nonempty_string(),string(),nonempty_string(),string(),mypl_db:jsondict()) -> ok.
articleaudit(Quantity, Product, Text, Mui, Transaction, References) ->
    Fun = fun() ->
            mnesia:write(#articleaudit{id="a" ++ mypl_util:oid(), quantity=Quantity, product=Product,
                                       text=Text, mui=Mui, transaction=Transaction,
                                       references=References, created_at=calendar:universal_time()})
          end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.
    

%% @private
%% @spec articleaudit(integer(), product(), string(), muiID(), all()) -> {ok, atomic}
%% @doc dump information about changes of stock
-spec articleaudit(integer(),nonempty_string(),string(),nonempty_string(),any()) -> ok.
articleaudit(Quantity, Product, Text, Mui, Transaction) ->
    articleaudit(Quantity, Product, Text, Mui, Transaction, []).

%% @private
%% @spec articleaudit(integer(), product(), string(), muiID()) -> {ok, atomic}
%% @doc dump information about changes of stock
-spec articleaudit(integer(),nonempty_string(),string(),nonempty_string()) -> ok.
articleaudit(Quantity, Product, Text, Mui) ->
    articleaudit(Quantity, Product, Text, Mui, undefined).
    

-spec unitaudit_mui(nonempty_string(),string()) -> ok.
unitaudit_mui(Mui, Text) ->
    Fun = fun() ->
            mnesia:write(#unitaudit{id="A" ++ mypl_util:oid(),
                                    mui=Mui, text=Text, created_at=calendar:universal_time()})
          end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.
    

%% @private
%% @spec unitaudit(unitRecord(), string(), string(), externalReferences()) -> {ok, atomic}
%% @doc to be called whenever Units are moved in the warehouse.
-spec unitaudit(#unit{},string(),string(),mypl_db:jsondict()) -> ok.
unitaudit(Unit, Text, Transaction, References) ->
    Id = "A" ++ mypl_util:oid(),
    Fun = fun() ->
        mnesia:write(#unitaudit{id=Id,
                                mui=Unit#unit.mui, quantity=Unit#unit.quantity, product=Unit#unit.product,
                                text=Text, transaction=Transaction,
                                references=References, created_at=calendar:universal_time()})
    end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.
    

%% @spec unitaudit(unitRecord(), string(), string()) -> {ok, atomic}
%% @doc to be called whenever Units are moved in the warehouse.
-spec unitaudit(#unit{},string(),string()) -> ok.
unitaudit(Unit, Text, Transaction) ->
    unitaudit(Unit, Text, Transaction, []).
    

%% @spec unitaudit(unitRecord(), string()) -> {ok, atomic}
%% @doc to be called whenever Units are moved in the warehouse.
-spec unitaudit(#unit{},string()) -> ok.
unitaudit(Unit, Text) ->
    unitaudit(Unit, Text, "").
    

%% @spec kommiauftragaudit(provpipelineRecord(), string(), string(), externalReferences()) -> {ok, atomic}
%% @doc to be called whenever Units are moved in the warehouse.
-spec kommiauftragaudit(#provpipeline{},string(),string(),mypl_db:jsondict()) -> ok.
kommiauftragaudit(Kommiauftrag, Text, Transaction, References) ->
    Id = "K" ++ mypl_util:oid(),
    Auftragsnummer = proplists:get_value(auftragsnummer, Kommiauftrag#provpipeline.attributes, []),
    Customer = proplists:get_value(kernel_customer, Kommiauftrag#provpipeline.attributes, []),
    Fun = fun() ->
        mnesia:write(#kommiauftragaudit{id=Id,
                                        komminr=Kommiauftrag#provpipeline.id,
                                        auftrnr=Auftragsnummer,
                                        customer=Customer,
                                        text=Text, transaction=Transaction,
                                        references=References,
                                        created_at=calendar:universal_time()})
    end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.

%% @doc archives an Unit, Movement, Pick when it is deleted.
-spec archive(tuple(), atom()) -> 'ok'.
archive(Object, Archivaltype) ->
    Fun = fun() ->
        mnesia:dirty_write(#archive{id="R" ++ mypl_util:oid() ++ element(2, Object),
                                    body=Object, archived_by=Archivaltype,
                                    created_at=mypl_util:timestamp(),
                                    type=element(1, Object),
                                    body_id=element(2, Object)})
    end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.
    


%% @doc retrives the entry with type and id from the archive

%% @deprecated
get_from_archive(Type, Id) ->
    [X#archive.body || X <- mnesia:dirty_match_object(#archive{body_id = Id, type = Type, _ = '_'})].
    

%% doc get all entries if type generated in the last 5 days
%% @deprecated
get_recent_from_archive(Type) when is_atom(Type) ->
    {Date, _} = calendar:now_to_datetime(erlang:now()),
    {Start, {0,0,0}, 0} = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 5),
    mypl_db_util:do_trans(qlc:q([X#archive.body || X <- mnesia:table(archive),
                                                   X#archive.type =:= Type,
                                                   X#archive.created_at > Start]));
%% @deprecated
get_recent_from_archive(Type) ->
    get_recent_from_archive(erlang:list_to_atom(Type)).
    
