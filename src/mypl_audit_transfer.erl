%%% @author    Maximillian Dornseif <> []
%%% @copyright 2009 Maximillian Dornseif
%%% @doc  transveral of data from mypl_audit into Couchdb
%%% @end  
%%%
%%% @since 2009-01-14 by Maximillian Dornseif
-module(mypl_audit_transfer).
-author('Maximillian Dornseif').

-include("mypl.hrl").

%% API
-export([spawn_audit_transfer/0, transfer_unitaudit/0, transfer_articleaudit/0, transfer_kommiauftragaudit/0,
         transfer_archive/0]).

%%%
%%% The code below is for a process which will transfer data continuesly into CouchDB
%%%

% changes all string values in a proplist to binary
proplist_cleanup_binary([]) ->
    [];
proplist_cleanup_binary([{K, V}|T]) when is_number(V) ->
    [{mypl_util:ensure_binary(K), V}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([[K, V]|T]) when is_number(V) ->
    [{mypl_util:ensure_binary(K), V}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([{K, V}|T]) ->
    [{mypl_util:ensure_binary(K), mypl_util:ensure_binary(V)}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([[K, V]|T]) ->
    [{mypl_util:ensure_binary(K), mypl_util:ensure_binary(V)}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([[K]|T]) ->
    [{mypl_util:ensure_binary(K), true}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([{K}|T]) ->
    [{mypl_util:ensure_binary(K), true}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([K|T]) when is_atom(K) ->
    [{mypl_util:ensure_binary(K), true}|proplist_cleanup_binary(T)];
proplist_cleanup_binary([H|T]) ->
    [H|proplist_cleanup_binary(T)].

save_into_couchdb(DbName, Id, Doc) ->
    case erlang_couchdb:create_document({"couchdb.local.hudora.biz", 5984}, DbName, Id, Doc) of
        {json,{[{<<"ok">>,true}|_]}} -> 
            ok;
        {json,{[{<<"error">>,<<"conflict">>}, {<<"reason">>,<<"Document update conflict.">>}]}} ->
            save_into_couchdb(DbName, Id ++ "_", Doc)
    end.
    

transfer_all() ->
    transfer_articleaudit(),
    transfer_unitaudit(),
    transfer_kommiauftragaudit(),
    transfer_archive(),
    timer:sleep(1000*20).
    

transfer_archive() ->
    transfer_archive(mnesia:dirty_first(archive)).

transfer_archive('$end_of_table') -> ok;
transfer_archive(Key) ->
    NextKey = mnesia:dirty_next(archive, Key),
    case mnesia:dirty_read({archive, Key}) of
        [] ->
            transfer_archive(NextKey);
        [Record] ->
            Body = Record#archive.body,
            ArchivedAt = mypl_util:timestamp2binary(Record#archive.created_at),
            case {Record#archive.type, size(Record#archive.body)} of
                {pick, 7} ->
                    save_pick(Key, Record, Body, ArchivedAt),
                    transfer_archive(NextKey);
                {movement, 7} ->
                    save_movement(Key, Record, Body, ArchivedAt),
                    transfer_archive(NextKey);
                {unit, 9} ->
                    save_unit(Key, Record, Body, ArchivedAt),
                    transfer_archive(NextKey);
                {provpipeline, 10} ->
                    save_provpipeline(Key, Record, Body, ArchivedAt),
                    transfer_archive(NextKey);
                {provisioninglist, 10} ->
                    %save_provisioninglist(Key, Record, Body, ArchivedAt),
                    transfer_archive(NextKey);
                Other ->
                    erlang:display(Record#archive.body),
                    erlang:display({vv1, Other, Record}),
                    erlang:display({vv2, Record}),
                    erlang:display({vv3, Body})
            end
    end.
    

save_pick(Key, Record, Body, ArchivedAt) ->
    save_into_couchdb("mypl_archive",
         Body#pick.id ++ "-" ++ Record#archive.id,
         [{type, <<"pick">>},
          {oid, mypl_util:ensure_binary(Body#pick.id)},
          {mui, mypl_util:ensure_binary(Body#pick.from_unit)},
          {quantity, Body#pick.quantity},
          {product, mypl_util:ensure_binary(Body#pick.product)},
          {attributes, {proplist_cleanup_binary(Body#pick.attributes)}},
          {archived_by, mypl_util:ensure_binary(Record#archive.archived_by)},
          {archived_at, ArchivedAt},
          {created_at, mypl_util:timestamp2binary(Body#pick.created_at)}
         ]),
    ok = mnesia:dirty_delete(archive, Key).
    

save_movement(Key, Record, Body, ArchivedAt) ->
    save_into_couchdb("mypl_archive",
         Body#movement.id ++ "-" ++ Record#archive.id,
         [{type, <<"movement">>},
          {oid, mypl_util:ensure_binary(Body#movement.id)},
          {mui, mypl_util:ensure_binary(Body#movement.mui)},
          {from_location, mypl_util:ensure_binary(Body#movement.from_location)},
          {to_location, mypl_util:ensure_binary(Body#movement.to_location)},
          {attributes, {proplist_cleanup_binary(Body#movement.attributes)}},
          {archived_by, mypl_util:ensure_binary(Record#archive.archived_by)},
          {archived_at, ArchivedAt},
          {created_at, mypl_util:timestamp2binary(Body#movement.created_at)}
         ]),
    ok = mnesia:dirty_delete(archive, Key).
    

save_unit(Key, Record, Body, ArchivedAt) ->
    save_into_couchdb("mypl_archive",
            Body#unit.mui ++ "-" ++ Record#archive.id,
            [{type, <<"unit">>},
             {oid, mypl_util:ensure_binary(Body#unit.mui)},
             {mui, mypl_util:ensure_binary(Body#unit.mui)},
             {quantity, Body#unit.quantity},
             {product, mypl_util:ensure_binary(Body#unit.product)},
             {attributes, {proplist_cleanup_binary(Body#unit.attributes)}},
             {location, mypl_util:ensure_binary(Body#unit.location)},
             {height, Body#unit.height},
             {archived_by, mypl_util:ensure_binary(Record#archive.archived_by)},
             {archived_at, ArchivedAt},
             {created_at, mypl_util:timestamp2binary(Body#unit.created_at)}
            ]),
    ok = mnesia:dirty_delete(archive, Key).
    

save_provpipeline(Key, Record, Body, ArchivedAt) ->
    save_into_couchdb("mypl_archive",
            Body#provpipeline.id ++ "-" ++ Record#archive.id,
            [{type, <<"provpipeline">>},
             {oid, mypl_util:ensure_binary(Body#provpipeline.id)},
             {id, mypl_util:ensure_binary(Body#provpipeline.id)},
             {priority, Body#provpipeline.priority},
             {attributes, {proplist_cleanup_binary(Body#provpipeline.attributes
                                                    ++ [{weigth, Body#provpipeline.weigth},
                                                        {volume, Body#provpipeline.volume}])}},
             {status, mypl_util:ensure_binary(Body#provpipeline.status)},
             {tries, Body#provpipeline.tries},
             {provisioninglists, Body#provpipeline.provisioninglists},
             {archived_by, mypl_util:ensure_binary(Record#archive.archived_by)},
             {archived_at, ArchivedAt},
             {orderlines, lists:map(fun({Quantity, Product, Attributes}) -> 
                                         [Quantity, mypl_util:ensure_binary(Product), {proplist_cleanup_binary(Attributes)}]
                                     end, Body#provpipeline.orderlines)}
             ]),
    ok = mnesia:dirty_delete(archive, Key).
    

save_provisioninglist(Key, Record, Body, ArchivedAt) ->
    save_into_couchdb("mypl_archive",
        Body#provisioninglist.id ++ "-" ++ Record#archive.id,
        [{type, Body#provisioninglist.type},
         {oid, mypl_util:ensure_binary(Body#provisioninglist.id)},
         {provpipeline_id, mypl_util:ensure_binary(Body#provisioninglist.provpipeline_id)},
         {destination, mypl_util:ensure_binary(Body#provisioninglist.destination)},
         {attributes, {proplist_cleanup_binary(Body#provisioninglist.attributes
                       ++ [{parts, Body#provisioninglist.parts}])}},
         {provisionings, lists:map(fun({Id, _, _, _, _, _}) ->
                                       mypl_util:ensure_binary(Id);
                                      (Id) ->
                                       mypl_util:ensure_binary(Id)
                                   end,
                                   Body#provisioninglist.provisionings)},
         {status, mypl_util:ensure_binary(Body#provisioninglist.status)},
         {created_at, mypl_util:ensure_binary(Body#provisioninglist.created_at)},
         {archived_by, mypl_util:ensure_binary(Record#archive.archived_by)},
         {archived_at, ArchivedAt}
         ]),
    ok = mnesia:dirty_delete(archive, Key).

%% @doc Transfer Unit Audit data to Postgresql.
transfer_unitaudit() ->
    transfer_unitaudit(mnesia:dirty_first(unitaudit)).
    
% This way of traversing the table does not catch all entries in the first
% run but this is no problem since we run several times.
transfer_unitaudit('$end_of_table') -> ok;
transfer_unitaudit(Key) ->
    NextKey = mnesia:dirty_next(unitaudit, Key),
    case mnesia:dirty_read({unitaudit, Key}) of
        [] ->
            ok;
        [Buffer] ->
            Archivedata = [{type, "unitaudit"},
                            {mui, Buffer#unitaudit.mui},
                            {quantity, Buffer#unitaudit.quantity},
                            {product, Buffer#unitaudit.product},
                            {description, Buffer#unitaudit.text},
                            {transaction, Buffer#unitaudit.transaction},
                            {ref, Buffer#unitaudit.references},
                            {created_at, mypl_util:timestamp2binary(Buffer#unitaudit.created_at)}
                           ],
            save_into_couchdb("mypl_audit",
                lists:flatten([Buffer#unitaudit.mui, "-", Buffer#unitaudit.id]),
                Archivedata),
            mnesia:dirty_delete(unitaudit, Key)
    end,
    transfer_unitaudit(NextKey).


transfer_articleaudit() ->
    transfer_articleaudit(mnesia:dirty_first(articleaudit)).

transfer_articleaudit('$end_of_table') -> ok;
transfer_articleaudit(Key) ->
    NextKey = mnesia:dirty_next(articleaudit, Key),
    case mnesia:dirty_read({articleaudit, Key}) of
        [] ->
            ok;
        [Record] ->
            save_into_couchdb("mypl_audit",
                Record#articleaudit.product ++ "-" ++ Record#articleaudit.id,
                [{type, "articleaudit"},
                 {mui, Record#articleaudit.mui},
                 {quantity, Record#articleaudit.quantity},
                 {product, Record#articleaudit.product},
                 {description, Record#articleaudit.text},
                 {transaction, Record#articleaudit.transaction},
                 {ref, Record#articleaudit.references},
                 {created_at, mypl_util:timestamp2binary(Record#articleaudit.created_at)}
                ]),
            ok = mnesia:dirty_delete({articleaudit, Key})
    end,
    transfer_articleaudit(NextKey).
    

transfer_kommiauftragaudit() ->
    transfer_kommiauftragaudit(mnesia:dirty_first(kommiauftragaudit)).
    
transfer_kommiauftragaudit('$end_of_table') -> ok;
transfer_kommiauftragaudit(Key) ->
    NextKey = mnesia:dirty_next(kommiauftragaudit, Key),
    case mnesia:dirty_read({kommiauftragaudit, Key}) of
        [] ->
            ok;
        [Record] ->
            save_into_couchdb("mypl_audit",
                lists:flatten([Record#kommiauftragaudit.komminr, "-", Record#kommiauftragaudit.id]),
                [{type, <<"kommiauftragaudit">>},
                 {oid, mypl_util:ensure_binary(Record#kommiauftragaudit.komminr)},
                 {komminr, mypl_util:ensure_binary(Record#kommiauftragaudit.komminr)},
                 {auftrnr, mypl_util:ensure_binary(Record#kommiauftragaudit.auftrnr)},
                 {customer, mypl_util:ensure_binary(Record#kommiauftragaudit.customer)},
                 {description, mypl_util:ensure_binary(Record#kommiauftragaudit.text)},
                 {transaction, mypl_util:ensure_binary(Record#kommiauftragaudit.transaction)},
                 {attributes, {proplist_cleanup_binary(Record#kommiauftragaudit.references)}},
                 {created_at, mypl_util:timestamp2binary(Record#kommiauftragaudit.created_at)}
                ]),
            ok = mnesia:dirty_delete({kommiauftragaudit, Key})
    end,
    transfer_kommiauftragaudit(NextKey).


% @doc spawn start_transfer/0 - but ensure only one is running
spawn_audit_transfer() ->
    mypl_util:spawn_and_register(audit_transfer_process, fun() -> transfer_all() end),
    ok.
