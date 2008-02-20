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
%% @TODO add functionality to read the auditlogs
%% @end

-module(mypl_audit).

-include_lib("stdlib/include/qlc.hrl").
-include("mypl.hrl").

% speichert alle Warenbewegungen zu Protokollzwecken
-record(articleaudit, {id,           % eindeutiger Bezeichner
                       quantity,     % Einkeiten des produkts
                       product,      % ArtNr
                       text,         % text describing the transaction
                       mui,          % bebuchte Unit
                       transaction,  % movement or pick ID
                       references,   % list of tuples to be used by the client application, not used by the myPL kernel
                       created_at
                   }).


% speichert alle Unitbewegungen zu Protokollzwecken
-record(unitaudit, {id,           % eindeutiger Bezeichner
                    mui,          % bebuchte Unit
                    quantity,     % Einkeiten des produkts
                    product,      % ArtNr
                    text,         % text describing the transaction
                    transaction,  % movement or pick ID
                    references,   % list of tuples to be used by the client application, not used by the myPL kernel
                    created_at
                   }).

% archiviert units, movements und picks
-record(auditbuffer, {id,           % eindeutiger Bezeichner
                      body
                     }).

-export([run_me_once/0, get_articleaudit/1, get_unitaudit/1, get_articlecorrection/1,
         articleaudit/6, articleaudit/5, articleaudit/4,
         unitaudit_mui/2, unitaudit/4, unitaudit/3, unitaudit/2,
         archive/2, spawn_audit_transfer/0, compress_audit/0, compress_audit/1,
         get_from_archive/2, get_recent_from_archive/1]).

%%% we assume all test and initialisation functionality is provided vby other modules

%%%
%%% auditlog - to be called whenever goods enter or leave the warehouse
%%%


run_me_once() ->
    % Tables kept in RAM with disk based backing
    mypl_db:init_table_info(mnesia:create_table(auditbuffer,      [{disc_copies, [node()]}, {attributes, record_info(fields, auditbuffer)}]), auditbuffer),
    % Disk only Tables
    mypl_db:init_table_info(mnesia:create_table(archive,          [{disc_only_copies, [node()]}, {attributes, record_info(fields, archive)}]), archive),
    mnesia:add_table_index(archive, #archive.type),
    mnesia:add_table_index(archive, #archive.body_id),
    mypl_db:init_table_info(mnesia:create_table(articleaudit,     [{disc_only_copies, [node()]}, {attributes, record_info(fields, articleaudit)}]), articleaudit),
    mnesia:add_table_index(articleaudit, #articleaudit.product),
    mypl_db:init_table_info(mnesia:create_table(unitaudit,        [{disc_only_copies, [node()]}, {attributes, record_info(fields, unitaudit)}]), unitaudit),
    mnesia:add_table_index(unitaudit, #unitaudit.mui),
    ok.


%% @doc returns all audit recodes related to an article
get_articleaudit(Product) ->
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(articleaudit),
                                                X#articleaudit.product =:= Product])),
    lists:map(fun(X) -> get_articleaudit_helper(X) end, lists:sort(Records)).
    

%% @private
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
articleaudit(Quantity, Product, Text, Mui, Transaction, References) ->
    Fun = fun() ->
            mnesia:write(#auditbuffer{id=mypl_util:oid(),
                                      body=#articleaudit{id="a" ++ mypl_util:oid(), quantity=Quantity, product=Product,
                                                         text=Text, mui=Mui, transaction=Transaction,
                                                         references=References, created_at=calendar:universal_time()}})
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


unitaudit_mui(Mui, Text) ->
    Fun = fun() ->
            mnesia:write(#auditbuffer{id=mypl_util:oid(),
                                      body=#unitaudit{id="A" ++ mypl_util:oid(),
                                                      mui=Mui, text=Text, created_at=calendar:universal_time()}})
          end,
    mnesia:transaction(Fun).

%% @private
%% @spec unitaudit(unitRecord(), string(), string(), externalReferences()) -> {ok, atomic}
%% @doc to be called whenever Units are moved in the warehouse.
unitaudit(Unit, Text, Transaction, References) ->
    Fun = fun() ->
            mnesia:write(#auditbuffer{id=mypl_util:oid(),
                                      body=#unitaudit{id="A" ++ mypl_util:oid(),
                                                      mui=Unit#unit.mui, quantity=Unit#unit.quantity, product=Unit#unit.product,
                                                      text=Text, transaction=Transaction,
                                                      references=References, created_at=calendar:universal_time()}})
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


%% @doc archives an Unit, Movement, Pick when it is deleted.
archive(Object, Archivaltype) ->
    Fun = fun() ->
        mnesia:write(#auditbuffer{id=mypl_util:oid(),
                                  body=#archive{id="R" ++ mypl_util:oid(),
                                                body=Object, archived_by=Archivaltype,
                                                created_at=mypl_util:timestamp(),
                                                type=element(1, Object),
                                                body_id=element(2, Object)}})
    end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.

%% @doc retrives the entry with type and id from the archive
get_from_archive(Type, Id) ->
    [X#archive.body || X <- mnesia:dirty_match_object(#archive{body_id = Id, type = Type, _ = '_'})].
    

%% doc get all entries if type generated in the last 5 days
get_recent_from_archive(Type) when is_atom(Type) ->
    {Date, _} = calendar:now_to_datetime(erlang:now()),
    {Start, {0,0,0}, 0} = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 5),
    mypl_db_util:do_trans(qlc:q([X#archive.body || X <- mnesia:table(archive),
                                                   X#archive.type =:= Type,
                                                   X#archive.created_at > Start]));
get_recent_from_archive(Type) ->
    get_recent_from_archive(erlang:list_to_atom(Type)).
    

%% TODO: chage from dirty to transaction based
%% @doc transfer data from temporary audit table to it's final destination
transfer_buffers() ->
    transfer_buffers(mnesia:dirty_first(auditbuffer)).

transfer_buffers('$end_of_table') -> ok;
transfer_buffers(Key) ->
    % for set and ordered_set tables this will execute once, for bag tables this could execute many times...
    case mnesia:dirty_read({auditbuffer, Key}) of
        [] ->
            ok;
        [Buffer] ->
            mnesia:dirty_write(Buffer#auditbuffer.body),
            mnesia:dirty_delete(auditbuffer, Buffer#auditbuffer.id),
            timer:sleep(1), % sleep 11 ms to give disk drives / mnesia time to rest
            transfer_buffers(mnesia:dirty_first(auditbuffer))
    end.

% @doc spawn transfer_buffers/0 - but ensure only one is running
spawn_audit_transfer() ->
    % the next line will fail if there already is a audit_transfer_process running, which is fine ...
    mypl_util:spawn_and_register(audit_transfer_process, fun() -> transfer_buffers() end).


%% the following compression functions are meant for compacting the archival tables

compress_articleaudit_helper(Quantity, []) ->
    Quantity;
compress_articleaudit_helper(Quantity, [Record|T]) ->
    NewQuantity = Quantity + Record#articleaudit.quantity,
    ok = mnesia:delete({articleaudit, Record#articleaudit.id}),
    compress_articleaudit_helper(NewQuantity, T).
    
compress_articleaudit(Product, BeforeDate) ->
    BeforeDays = calendar:date_to_gregorian_days(BeforeDate),
    Records = mypl_db_util:do_trans(qlc:q([X || X <- mnesia:table(articleaudit),
                                                X#articleaudit.product =:= Product,
                                                X#articleaudit.text /= "Zusammenfassung vorheriger Einträge",
                                                BeforeDays > calendar:date_to_gregorian_days(element(1, X#articleaudit.created_at))])),
    erlang:display({Product, length(Records)}),
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
    Products = lists:usort(mypl_db_util:do_trans(qlc:q([X#articleaudit.product || X <- mnesia:table(articleaudit)]))),
    % TODO: iterate over records instead of reading them all to memory
    lists:map(fun(Product) -> compress_articleaudit(Product, BeforeDate) end, Products).
    

compress_unitaudit_helper([]) -> ok;
compress_unitaudit_helper([Record|T]) ->
    % TODO: possibly check if the unit stil exists in the warehouse
    ok = mnesia:delete({unitaudit, Record#unitaudit.id}),
    compress_unitaudit_helper(T).

compress_unitaudit(BeforeDate) ->
    BeforeDays = calendar:date_to_gregorian_days(BeforeDate),
    Fun = fun() ->
        % TODO: iterate over records instead of reading them all to memory
        Records = mypl_db_util:do(qlc:q([X|| X <- mnesia:table(unitaudit),
                                             BeforeDays > calendar:date_to_gregorian_days(element(1, X#unitaudit.created_at))])),
        compress_unitaudit_helper(Records)
    end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    

compress_audit(BeforeDate) ->
    % compress_unitaudit(BeforeDate),
    mnesia:add_table_index(articleaudit, #articleaudit.product),
    compress_articleaudit(BeforeDate).

compress_audit() ->
    {Date, _} = calendar:now_to_datetime(erlang:now()),
    BeforeDate = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 180),
    compress_audit(BeforeDate),
    compress_unitaudit(BeforeDate),
    ok.
    

