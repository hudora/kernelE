%%% @author    Maximillian Dornseif <> []
%%% @copyright 2009 Maximillian Dornseif
%%% @doc  transveral of data from mypl_audit into Couchdb
%%% @end  
%%%
%%% @since 2009-01-14 by Maximillian Dornseif
-module(mypl_audit_transfer).
-author('Maximillian Dornseif').

-include("mypl.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start_transfer/0, transfer_unitaudit/0, transfer_articleaudit/0, transfer_archive/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, mypl_audit_transfer}, ?MODULE, [], []).

start_transfer() ->
    gen_server:call(mypl_audit_transfer, transfer).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast(transfer, State) ->
    transfer_archive(),
    transfer_articleaudit(),
    transfer_unitaudit(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

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
    

transfer_archive() ->
    transfer_archive(mnesia:dirty_first(archive)).
    
transfer_archive('$end_of_table') -> ok;
transfer_archive(Key) ->
    NextKey = mnesia:dirty_next(archive, Key),
    case mnesia:dirty_read({archive, Key}) of
        [] ->
            transfer_archive(NextKey);
        [Record] ->
            erlang:display({Record#archive.type, size(Record#archive.body), Record#archive.id}),
            Body = Record#archive.body,
            ArchivedAt = mypl_util:timestamp2binary(Record#archive.created_at),
            case {Record#archive.type, size(Record#archive.body)} of
                {pick, 7} ->
                    save_into_couchdb("mypl_archive",
                             Body#pick.id ++ "-" ++ Record#archive.id,
                             [{type, <<"pick">>},
                              {mui, mypl_util:ensure_binary(Body#pick.from_unit)},
                              {quantity, Body#pick.quantity},
                              {product, mypl_util:ensure_binary(Body#pick.product)},
                              {attributes, {proplist_cleanup_binary(Body#pick.attributes)}},
                              {archived_by, mypl_util:ensure_binary(Record#archive.archived_by)},
                              {archived_at, ArchivedAt},
                              {created_at, mypl_util:timestamp2binary(Body#pick.created_at)}
                             ]),
                    ok = mnesia:dirty_delete(archive, Key),
                    transfer_archive(NextKey);
                {movement, 7} ->
                    save_into_couchdb("mypl_archive",
                             Body#movement.id ++ "-" ++ Record#archive.id,
                             [{type, <<"movement">>},
                              {mui, mypl_util:ensure_binary(Body#movement.mui)},
                              {from_location, mypl_util:ensure_binary(Body#movement.from_location)},
                              {to_location, mypl_util:ensure_binary(Body#movement.to_location)},
                              {attributes, {proplist_cleanup_binary(Body#movement.attributes)}},
                              {archived_by, mypl_util:ensure_binary(Record#archive.archived_by)},
                              {archived_at, ArchivedAt},
                              {created_at, mypl_util:timestamp2binary(Body#movement.created_at)}
                             ]),
                    ok = mnesia:dirty_delete(archive, Key),
                    transfer_archive(NextKey);
                {unit, 9} ->
                    save_into_couchdb("mypl_archive",
                            Body#unit.mui ++ "-" ++ Record#archive.id,
                            [{type, <<"unit">>},
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
                    ok = mnesia:dirty_delete(archive, Key),
                    transfer_archive(NextKey);
                {provpipeline, 10} ->
                    erlang:display(Record#archive.body),
                    save_into_couchdb("mypl_archive",
                            Body#provpipeline.id ++ "-" ++ Record#archive.id,
                            [{type, <<"provpipeline">>},
                             {id, mypl_util:ensure_binary(Body#provpipeline.id)},
                             {priority, Body#provpipeline.priority},
                             {attributes, {proplist_cleanup_binary(Body#provpipeline.attributes 
                              ++ [{weigth, Body#provpipeline.weigth}, {volume, Body#provpipeline.volume}])}},
                             {status, mypl_util:ensure_binary(Body#provpipeline.status)},
                             {tries, Body#provpipeline.tries},
                             {provisioninglists, Body#provpipeline.provisioninglists},
                             {archived_by, mypl_util:ensure_binary(Record#archive.archived_by)},
                             {archived_at, ArchivedAt},
                             {orderlines, lists:map(fun({Quantity, Product, Attributes}) -> 
                                                        [Quantity, mypl_util:ensure_binary(Product), {proplist_cleanup_binary(Attributes)}]
                                                    end, Body#provpipeline.orderlines)}
                            ]),
                    ok = mnesia:dirty_delete(archive, Key),
                    transfer_archive(NextKey);
                Other ->
                    erlang:display(Record#archive.body),
                    erlang:display({vv1, Other, Record}),
                    erlang:display({vv2, Record}),
                    erlang:display({vv3, Body})
            end
    end.


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
            save_into_couchdb("mypl_audit",
                Buffer#unitaudit.mui ++ "-" ++ Buffer#unitaudit.id,
                [{type, "unitaudit"},
                 {mui, Buffer#unitaudit.mui},
                 {quantity, Buffer#unitaudit.quantity},
                 {product, Buffer#unitaudit.product},
                 {description, Buffer#unitaudit.text},
                 {transaction, Buffer#unitaudit.transaction},
                 {ref, Buffer#unitaudit.references},
                 {created_at, mypl_util:timestamp2binary(Buffer#unitaudit.created_at)}
                ]),
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
    

