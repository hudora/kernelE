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
%% After a very short delay the unitaudit data is written to a postresql database
%% and removed from kernelE
%%
%% You need to create the following Tables in PostgreSQL:
%%
%%   CREATE TABLE unitaudit (
%%       id          varchar(128) NOT NULL PRIMARY KEY,
%%       mui         varchar(32) NOT NULL,
%%       quantity    integer NOT NULL,
%%       product     varchar(20) NOT NULL,
%%       description text NOT NULL,
%%       transaction varchar(32) NOT NULL,
%%       ref         text NOT NULL,
%%       created_at  timestamp NOT NULL
%%   );
%%   CREATE INDEX unitaudit_idx1 ON unitaudit ( product );
%%   CREATE INDEX unitaudit_idx2 ON unitaudit ( mui );
%%   GRANT SELECT, INSERT ON unitaudit TO kernele;
%%   
%%   CREATE TABLE articleaudit (
%%       id          varchar(128) NOT NULL PRIMARY KEY,
%%       mui         varchar(32) NOT NULL,
%%       quantity    integer NOT NULL,
%%       product     varchar(20) NOT NULL,
%%       description text NOT NULL,
%%       transaction varchar(32) NOT NULL,
%%       ref         text NOT NULL,
%%       created_at  timestamp NOT NULL
%%   );
%%   CREATE INDEX articleaudit_idx1 ON unitaudit ( product );
%%   GRANT SELECT, INSERT ON articleaudit TO kernele;
%%   
%%   CREATE TABLE pickarchive (
%%       id          varchar(128) NOT NULL PRIMARY KEY,
%%       mui         varchar(32) NOT NULL,
%%       quantity    integer NOT NULL,
%%       product     varchar(20) NOT NULL,
%%       transaction varchar(32) NOT NULL,
%%       ref         text NOT NULL,
%%       created_at   timestamp NOT NULL,
%%       archived_at  timestamp NOT NULL
%%   );
%%   CREATE INDEX pickarchive_idx1 ON pickarchive ( product );
%%   CREATE INDEX pickarchive_idx2 ON pickarchive ( mui );
%%   CREATE INDEX pickarchive_idx3 ON pickarchive ( transaction );
%%   GRANT SELECT, INSERT ON pickarchive TO kernele;
%%   
%%   CREATE TABLE unitarchive (
%%       id           varchar(128) NOT NULL PRIMARY KEY,
%%       mui          varchar(32) NOT NULL,
%%       quantity     integer NOT NULL,
%%       product      varchar(20) NOT NULL,
%%       transaction  varchar(32) NOT NULL,
%%       ref          text NOT NULL,
%%       location     varchar(20) NOT NULL,
%%       height       integer NOT NULL,
%%       created_at   timestamp NOT NULL,
%%       archived_at  timestamp NOT NULL
%%   );
%%   CREATE INDEX unitarchive_idx1 ON unitarchive ( product );
%%   CREATE INDEX unitarchive_idx2 ON unitarchive ( mui );
%%   CREATE INDEX unitarchive_idx3 ON unitarchive ( transaction );
%%   GRANT SELECT, INSERT ON unitarchive TO kernele;
%%   
%%   CREATE TABLE movementarchive (
%%       id          varchar(128) NOT NULL PRIMARY KEY,
%%       mui         varchar(32) NOT NULL,
%%       quantity    integer NOT NULL,
%%       product     varchar(20) NOT NULL,
%%       transaction varchar(32) NOT NULL,
%%       ref         text NOT NULL,
%%       created_at   timestamp NOT NULL,
%%       archived_at  timestamp NOT NULL
%%   );
%%   CREATE INDEX movementarchive_idx1 ON movementarchive ( product );
%%   CREATE INDEX movementarchive_idx2 ON movementarchive ( mui );
%%   CREATE INDEX movementarchive_idx3 ON movementarchive ( transaction );
%%   GRANT SELECT, INSERT ON movementarchive TO kernele;
%%   
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


-export([run_me_once/0, get_articleaudit/1, get_unitaudit/1, get_articlecorrection/1,
         articleaudit/6, articleaudit/5, articleaudit/4,
         unitaudit_mui/2, unitaudit/4, unitaudit/3, unitaudit/2,
         archive/2, 
         transfer_unitaudit/0, transfer_archive/0, transfer_articleaudit/0,
         spawn_audit_transfer/0, compress_audit/0, compress_audit/1,
         get_from_archive/2, get_recent_from_archive/1]).

%%% we assume all test and initialisation functionality is provided vby other modules

%%%
%%% auditlog - to be called whenever goods enter or leave the warehouse
%%%


run_me_once() ->
    % Tables kept in RAM with disk based backing
    mypl_db:init_table_info(mnesia:create_table(articleaudit,     [{disc_copies, [node()]}, {attributes, record_info(fields, articleaudit)}]), articleaudit),
    mnesia:add_table_index(articleaudit, #articleaudit.product),
    mypl_db:init_table_info(mnesia:create_table(archive,          [{disc_copies, [node()]}, {attributes, record_info(fields, archive)}]), archive),
    mnesia:add_table_index(archive, #archive.type),
    mnesia:add_table_index(archive, #archive.body_id),
    mypl_db:init_table_info(mnesia:create_table(unitaudit,        [{disc_copies, [node()]}, {attributes, record_info(fields, unitaudit)}]), unitaudit),
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
            mnesia:dirty_write(#articleaudit{id="a" ++ mypl_util:oid(), quantity=Quantity, product=Product,
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
    

unitaudit_mui(Mui, Text) ->
    Fun = fun() ->
            mnesia:dirty_write(#unitaudit{id="A" ++ mypl_util:oid(),
                                          mui=Mui, text=Text, created_at=calendar:universal_time()})
          end,
    mnesia:transaction(Fun).
    

%% @private
%% @spec unitaudit(unitRecord(), string(), string(), externalReferences()) -> {ok, atomic}
%% @doc to be called whenever Units are moved in the warehouse.
unitaudit(Unit, Text, Transaction, References) ->
    Id = "A" ++ mypl_util:oid(),
    Fun = fun() ->
        mnesia:dirty_write(#unitaudit{id=Id,
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


%% @doc archives an Unit, Movement, Pick when it is deleted.
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
    

%% @doc transfer data from temporary audit table to PostgreSQL
transfer_buffers() ->
    transfer_unitaudit().


% changes all string values in a proplist to binary
proplist_cleanup_binary([]) -> [];
proplist_cleanup_binary(Atom) when is_atom(Atom) -> Atom;
proplist_cleanup_binary([{K, V}|T]) when is_list(V) ->
    [{K, list_to_binary(V)}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([[K, V]|T]) when is_list(V) ->
    [{K, list_to_binary(V)}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([H|T]) ->
    [H|proplist_cleanup_binary(T)].


transfer_archive() ->
    Pg = psql:allocate(),
    transfer_archive(mnesia:dirty_first(archive), Pg),
    psql:free().
    
transfer_archive('$end_of_table', _Pg) -> ok;
transfer_archive(Key, Pg) ->
    case mnesia:dirty_read({archive, Key}) of
        [] ->
            erlang:display(xxx),
            transfer_archive(mnesia:dirty_next(archive, Key), Pg);
        [Record] ->
            erlang:display({Record#archive.type, size(Record#archive.body), Record#archive.id}),
            case {Record#archive.type, size(Record#archive.body)} of
                {pick, 7} ->
                    erlang:display({yyy1, Record}),
                    Body = Record#archive.body,
                    {{PY, PMon, PD}, {PH, PMin, PS}, PMS} = Body#pick.created_at,
                    {{AY, AMon, AD}, {AH, AMin, AS}, AMS} = Record#archive.created_at,
                    Sql = "INSERT INTO pickarchive (id,mui,quantity,product,transaction,ref,created_at,archived_at)" ++
                          " VALUES (" ++ sql_tools:quote(Body#pick.id) ++ "," 
                           ++ sql_tools:quote(Body#pick.from_unit) ++ ","
                           ++ integer_to_list(Body#pick.quantity) ++ ","
                           ++ sql_tools:quote(Body#pick.product)  ++ ","
                           ++ sql_tools:quote(Record#archive.archived_by) ++ ","
                           ++ sql_tools:quote(rfc4627:encode(proplist_cleanup_binary(Body#pick.attributes))) ++ ","
                           ++ "'" ++ integer_to_list(PY) ++ "-"  ++ integer_to_list(PMon) ++ "-" 
                           ++ integer_to_list(PD) ++ " " ++ integer_to_list(PH) ++ ":" 
                           ++ integer_to_list(PMin) ++ ":" ++ integer_to_list(PS)
                           ++ "." ++ integer_to_list(PMS) ++ "'"  
                           ++ ", '" ++ integer_to_list(AY) ++ "-"  ++ integer_to_list(AMon) ++ "-" 
                           ++ integer_to_list(AD) ++ " " ++ integer_to_list(AH) ++ ":" 
                           ++ integer_to_list(AMin) ++ ":" ++ integer_to_list(AS) 
                           ++ "." ++ integer_to_list(AMS) ++ "')",
                    case psql:sql_query(Pg, Sql) of
                        [{<<73,78,83,69,82,84,32,48,32,49,0>>,[]}] -> % == "INSERT 0 1"
                            % all fine - go on.
                            mnesia:dirty_delete(archive, Key),
                            transfer_archive(mnesia:dirty_first(archive), Pg);
                         Error ->
                            erlang:display({error, Error, Sql}),
                            % finish
                            Error,
                            mnesia:dirty_delete(archive, Key),
                            transfer_archive(mnesia:dirty_first(archive), Pg)
                    end;
                {pick, 6} ->
                    erlang:display({zzz, Record}),
                    {pick, I, P, Q, F, C} = Record#archive.body,
                    {{PY, PMon, PD}, {PH, PMin, PS}, PMS} = C,
                    {{AY, AMon, AD}, {AH, AMin, AS}, AMS} = Record#archive.created_at,
                    Sql = "INSERT INTO pickarchive (id,mui,quantity,product,transaction,ref,created_at,archived_at)" ++
                          " VALUES (" ++ sql_tools:quote(I) ++ "," 
                           ++ sql_tools:quote(F) ++ ","
                           ++ integer_to_list(Q) ++ ","
                           ++ sql_tools:quote(P)  ++ ","
                           ++ sql_tools:quote(Record#archive.archived_by) ++ ","
                           ++ sql_tools:quote(rfc4627:encode([])) ++ ","
                           ++ "'" ++ integer_to_list(PY) ++ "-"  ++ integer_to_list(PMon) ++ "-" 
                           ++ integer_to_list(PD) ++ " " ++ integer_to_list(PH) ++ ":" 
                           ++ integer_to_list(PMin) ++ ":" ++ integer_to_list(PS)
                           ++ "." ++ integer_to_list(PMS) ++ "'"  
                           ++ ", '" ++ integer_to_list(AY) ++ "-"  ++ integer_to_list(AMon) ++ "-" 
                           ++ integer_to_list(AD) ++ " " ++ integer_to_list(AH) ++ ":" 
                           ++ integer_to_list(AMin) ++ ":" ++ integer_to_list(AS) 
                           ++ "." ++ integer_to_list(AMS) ++ "')",
                    case psql:sql_query(Pg, Sql) of
                        [{<<73,78,83,69,82,84,32,48,32,49,0>>,[]}] -> % == "INSERT 0 1"
                            % all fine - go on.
                            mnesia:dirty_delete(archive, Key),
                            transfer_archive(mnesia:dirty_first(archive), Pg);
                         Error ->
                            erlang:display({error, Error, Sql}),
                            % finish
                            Error,
                            mnesia:dirty_delete(archive, Key),
                            transfer_archive(mnesia:dirty_first(archive), Pg)
                    end;
                {movement, 7} ->
                    erlang:display({yyy2, Record}),
                    Body = Record#archive.body,
                    {{PY, PMon, PD}, {PH, PMin, PS}, PMS} = Body#movement.created_at,
                    {{AY, AMon, AD}, {AH, AMin, AS}, AMS} = Record#archive.created_at,
                    Sql = "INSERT INTO movementarchive (id,mui,quantity,product,transaction,ref,created_at,archived_at)" ++
                          " VALUES (" ++ sql_tools:quote(Body#movement.id) ++ "," 
                           ++ sql_tools:quote(Body#movement.mui) ++ ","
                           ++ "0,'', "
                           ++ sql_tools:quote(Record#archive.archived_by) ++ ","
                           ++ sql_tools:quote(rfc4627:encode(proplist_cleanup_binary(Body#movement.attributes
                               ++ [{from_location, Body#movement.from_location},
                                   {to_location, Body#movement.to_location}]))) ++ ","
                           ++ "'" ++ integer_to_list(PY) ++ "-"  ++ integer_to_list(PMon) ++ "-" 
                           ++ integer_to_list(PD) ++ " " ++ integer_to_list(PH) ++ ":" 
                           ++ integer_to_list(PMin) ++ ":" ++ integer_to_list(PS)
                           ++ "." ++ integer_to_list(PMS) ++ "'"  
                           ++ ", '" ++ integer_to_list(AY) ++ "-"  ++ integer_to_list(AMon) ++ "-" 
                           ++ integer_to_list(AD) ++ " " ++ integer_to_list(AH) ++ ":" 
                           ++ integer_to_list(AMin) ++ ":" ++ integer_to_list(AS) 
                           ++ "." ++ integer_to_list(AMS) ++ "')",
                    case psql:sql_query(Pg, Sql) of
                        [{<<73,78,83,69,82,84,32,48,32,49,0>>,[]}] -> % == "INSERT 0 1"
                            % all fine - go on.
                            mnesia:dirty_delete(archive, Key),
                            transfer_archive(mnesia:dirty_first(archive), Pg);
                         Error ->
                            erlang:display({error, Error, Sql}),
                            % finish
                            Error,
                            mnesia:dirty_delete(archive, Key),
                            transfer_archive(mnesia:dirty_first(archive), Pg)
                    end;
                {unit, 9} ->
                    erlang:display({yyy3, Record}),
                    Body = Record#archive.body,
                    {{PY, PMon, PD}, {PH, PMin, PS}, PMS} = Body#unit.created_at,
                    {{AY, AMon, AD}, {AH, AMin, AS}, AMS} = Record#archive.created_at,
                    Sql = "INSERT INTO unitarchive (id,mui,quantity,product,transaction,ref,location,height,created_at,archived_at)" ++
                          " VALUES (" ++ sql_tools:quote(Body#unit.mui) ++ "," 
                           ++ sql_tools:quote(Body#unit.mui) ++ ","
                           ++ integer_to_list(Body#unit.quantity) ++ ","
                           ++ sql_tools:quote(Body#unit.product)  ++ ","
                           ++ sql_tools:quote(Record#archive.archived_by) ++ ","
                           ++ sql_tools:quote(rfc4627:encode(proplist_cleanup_binary(Body#unit.attributes))) ++ ","
                           ++ sql_tools:quote(Body#unit.location) ++ ","
                           ++ sql_tools:quote(Body#unit.height) ++ ","
                           ++ "'" ++ integer_to_list(PY) ++ "-"  ++ integer_to_list(PMon) ++ "-" 
                           ++ integer_to_list(PD) ++ " " ++ integer_to_list(PH) ++ ":" 
                           ++ integer_to_list(PMin) ++ ":" ++ integer_to_list(PS)
                           ++ "." ++ integer_to_list(PMS) ++ "'"  
                           ++ ", '" ++ integer_to_list(AY) ++ "-"  ++ integer_to_list(AMon) ++ "-" 
                           ++ integer_to_list(AD) ++ " " ++ integer_to_list(AH) ++ ":" 
                           ++ integer_to_list(AMin) ++ ":" ++ integer_to_list(AS) 
                           ++ "." ++ integer_to_list(AMS) ++ "')",
                    case psql:sql_query(Pg, Sql) of
                        [{<<73,78,83,69,82,84,32,48,32,49,0>>,[]}] -> % == "INSERT 0 1"
                            % all fine - go on.
                            mnesia:dirty_delete(archive, Key),
                            transfer_archive(mnesia:dirty_first(archive), Pg);
                         Error ->
                            erlang:display({error, Error, Sql}),
                            % finish
                            Error,
                            mnesia:dirty_delete(archive, Key),
                            transfer_archive(mnesia:dirty_first(archive), Pg)

                    end;
                {provpipeline, 10} ->
                    erlang:display({yyy4, Record}),
                    Body = Record#archive.body,
                    %{{PY, PMon, PD}, {PH, PMin, PS}, PMS} = Body#provpipeline.created_at,
                    {{AY, AMon, AD}, {AH, AMin, AS}, AMS} = Record#archive.created_at,
                    Sql = "INSERT INTO provpipelinearchive (id,priority,orderlines,weigth,volume,status,tries,provisioninglists,ref)" ++
                          " VALUES (" ++ sql_tools:quote(Body#provpipeline.id) ++ "," 
                           ++ integer_to_list(Body#provpipeline.priority) ++ ","
                           ++ sql_tools:quote(rfc4627:encode(Body#provpipeline.orderlines)) ++ ","
                           ++ integer_to_list(Body#provpipeline.weigth) ++ ","
                           ++ float_to_list(Body#provpipeline.volume) ++ ","
                           ++ sql_tools:quote(Body#provpipeline.status) ++ ","
                           ++ integer_to_list(Body#provpipeline.tries) ++ ","
                           ++ sql_tools:quote(rfc4627:encode(Body#provpipeline.provisioninglists)) ++ ","
                           ++ sql_tools:quote(rfc4627:encode(proplist_cleanup_binary(Body#provpipeline.attributes)))
                           ++ ")",
                    case psql:sql_query(Pg, Sql) of
                        [{<<73,78,83,69,82,84,32,48,32,49,0>>,[]}] -> % == "INSERT 0 1"
                            % all fine - go on.
                            mnesia:dirty_delete(archive, Key),
                            erlang:display({ok}),
                            transfer_archive(mnesia:dirty_first(archive), Pg);
                         Error ->
                            erlang:display({error, Error, Sql}),
                            % finish
                            Error
                            %mnesia:dirty_delete(archive, Key),
                            %transfer_archive(mnesia:dirty_first(archive), Pg)

                    end;
                Other ->
                    erlang:display({vvv, Other, Record}),
                    transfer_archive(mnesia:dirty_next(archive, Key), Pg)
            end
    end.

%% @doc Transfer Unit Audit data to Postgresql.
transfer_unitaudit() ->
    Pg = psql:allocate(),
    transfer_unitaudit(mnesia:dirty_first(unitaudit), Pg),
    psql:free().
    
% This way of traversing the table does not catch all entries in the first
% run but this is no problem since we run several times.
transfer_unitaudit('$end_of_table', _Pg) -> ok;
transfer_unitaudit(Key, Pg) ->
    case mnesia:dirty_read({unitaudit, Key}) of
        [] ->
            ok;
        [Buffer] ->
            {{Y, Mon, D}, {H, Min, S}} = Buffer#unitaudit.created_at,
            Sql = "INSERT INTO unitaudit (id, mui, quantity, product, description, transaction, ref, created_at)" ++
                  " VALUES (" ++ sql_tools:quote(Key) ++ ","
                   ++ sql_tools:quote(Buffer#unitaudit.mui) ++ ","
                   ++ integer_to_list(Buffer#unitaudit.quantity) ++ ","
                   ++ sql_tools:quote(Buffer#unitaudit.product)  ++ ","
                   ++ sql_tools:quote(Buffer#unitaudit.text) ++ ","
                   ++ sql_tools:quote(Buffer#unitaudit.transaction) ++ ","
                   ++ sql_tools:quote(rfc4627:encode_nolist(Buffer#unitaudit.references))
                   ++ ", '" ++ integer_to_list(Y) ++ "-"  ++ integer_to_list(Mon) ++ "-" 
                   ++ integer_to_list(D) ++ " " ++ integer_to_list(H) ++ ":" 
                   ++ integer_to_list(Min) ++ ":" ++ integer_to_list(S) ++ "')",
            case psql:sql_query(Pg, Sql) of
                [{<<73,78,83,69,82,84,32,48,32,49,0>>,[]}] -> % == "INSERT 0 1"
                    % all fine - go on.
                    mnesia:dirty_delete(unitaudit, Key),
                    transfer_unitaudit(mnesia:dirty_first(unitaudit), Pg);
                Error ->
                    erlang:display({error, Error, Sql}),
                    % finish
                    error
           end
    end.


% @doc spawn transfer_buffers/0 - but ensure only one is running
spawn_audit_transfer() ->
    % the next line will fail if there already is a audit_transfer_process running, which is fine ...
    mypl_util:spawn_and_register(audit_transfer_process, fun() -> transfer_buffers() end).


%% the following compression functions are meant for compacting the archival tables

compress_articleaudit_helper(Quantity, [], _Pg) ->
    Quantity;
compress_articleaudit_helper(Quantity, [Record|T], Pg) ->
    {{Y, Mon, D}, {H, Min, S}} = Record#articleaudit.created_at,
    Sql = "INSERT INTO articleaudit (id, mui, quantity, product, description, transaction, ref, created_at)" ++
                  " VALUES (" ++ sql_tools:quote(Record#articleaudit.id) ++ "," ++ sql_tools:quote(Record#articleaudit.mui) ++ ","
                   ++ integer_to_list(Record#articleaudit.quantity) ++ ","
                   ++ sql_tools:quote(Record#articleaudit.product)  ++ ","
                   ++ sql_tools:quote(Record#articleaudit.text) ++ ","
                   ++ sql_tools:quote(Record#articleaudit.transaction) ++ ","
                   ++ sql_tools:quote(rfc4627:encode_nolist(Record#articleaudit.references))
                   ++ ", '" ++ integer_to_list(Y) ++ "-"  ++ integer_to_list(Mon) ++ "-" 
                   ++ integer_to_list(D) ++ " " ++ integer_to_list(H) ++ ":" 
                   ++ integer_to_list(Min) ++ ":" ++ integer_to_list(S) ++ "')",
            erlang:display(Sql),
    case psql:sql_query(Pg, Sql) of
        [{<<73,78,83,69,82,84,32,48,32,49,0>>,[]}] -> % == "INSERT 0 1"
            % all fine - go on.
            ok = mnesia:delete({articleaudit, Record#articleaudit.id}),
            NewQuantity = Quantity + Record#articleaudit.quantity;
        Error ->
            erlang:display({error, Error, Sql}),
            NewQuantity = Quantity
    end,
    compress_articleaudit_helper(NewQuantity, T, Pg).
    
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
            Pg = psql:allocate(),
            Fun = fun() ->
                Quantity = compress_articleaudit_helper(0, Records1, Pg),
                articleaudit(Quantity, Product, "Zusammenfassung vorheriger Einträge", undefined),
                {Quantity, Product}
            end,
            {atomic, Ret} = mnesia:transaction(Fun),
            psql:free(),
            Ret
    end.
    

compress_articleaudit(BeforeDate) ->
    Products = lists:usort(mypl_db_util:do_trans(qlc:q([X#articleaudit.product || X <- mnesia:table(articleaudit)]))),
    % TODO: iterate over records instead of reading them all to memory
    lists:map(fun(Product) -> compress_articleaudit(Product, BeforeDate) end, Products).
    


transfer_articleaudit() ->
    Pg = psql:allocate(),
    transfer_articleaudit(mnesia:dirty_first(articleaudit), Pg),
    psql:free().


transfer_articleaudit('$end_of_table', _Pg) -> ok;
transfer_articleaudit(Key, Pg) ->
    case mnesia:dirty_read({articleaudit, Key}) of
        [] ->
            transfer_articleaudit(mnesia:dirty_next(articleaudit, Key), Pg);
        [Record] ->
            erlang:display({aaa, Record#articleaudit.id}),
            {{Y, Mon, D}, {H, Min, S}} = Record#articleaudit.created_at,
            Sql = "INSERT INTO articleaudit (id, mui, quantity, product, description, transaction, ref, created_at)" ++
                          " VALUES (" ++ sql_tools:quote(Record#articleaudit.id) ++ "," ++ sql_tools:quote(Record#articleaudit.mui) ++ ","
                           ++ integer_to_list(Record#articleaudit.quantity) ++ ","
                           ++ sql_tools:quote(Record#articleaudit.product)  ++ ","
                           ++ sql_tools:quote(Record#articleaudit.text) ++ ","
                           ++ sql_tools:quote(Record#articleaudit.transaction) ++ ","
                           ++ sql_tools:quote(rfc4627:encode_nolist(Record#articleaudit.references))
                           ++ ", '" ++ integer_to_list(Y) ++ "-"  ++ integer_to_list(Mon) ++ "-" 
                           ++ integer_to_list(D) ++ " " ++ integer_to_list(H) ++ ":" 
                           ++ integer_to_list(Min) ++ ":" ++ integer_to_list(S) ++ "')",
            case psql:sql_query(Pg, Sql) of
                [{<<73,78,83,69,82,84,32,48,32,49,0>>,[]}] -> % == "INSERT 0 1"
                    % all fine - go on.
                    ok = mnesia:dirty_delete({articleaudit, Record#articleaudit.id}),
                    transfer_articleaudit(mnesia:dirty_first(articleaudit), Pg);
                Error ->
                    erlang:display({error, Error, Sql}),
                    % ignore errors
                    ok = mnesia:dirty_delete({articleaudit, Record#articleaudit.id}),
                    transfer_articleaudit(mnesia:dirty_first(articleaudit), Pg)
            end
    end.






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
    BeforeDate = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 45),
    compress_audit(BeforeDate),
    compress_unitaudit(BeforeDate),
    ok.
    

