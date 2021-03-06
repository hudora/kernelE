%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is the RabbitMQ Erlang Client.
%%
%%   The Initial Developers of the Original Code are LShift Ltd.,
%%   Cohesive Financial Technologies LLC., and Rabbit Technologies Ltd.
%%
%%   Portions created by LShift Ltd., Cohesive Financial
%%   Technologies LLC., and Rabbit Technologies Ltd. are Copyright (C)
%%   2007 LShift Ltd., Cohesive Financial Technologies LLC., and Rabbit
%%   Technologies Ltd.;
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): Ben Hood <0x6e6562@gmail.com>.
%%

%% @doc This module encapsulates the client's view of an AMQP channel. Each
%% server side channel is represented by an amqp_channel process on the client
%% side. Channel processes are created using the {@link amqp_connection} 
%% module, but channels are respsonsible for closing themselves. Channel
%% processes are linked to the connnection process from which they were
%% created.
-module(amqp_channel).

-include("amqp_client.hrl").

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).
-export([call/2, call/3, cast/2, cast/3]).
-export([subscribe/3]).
-export([close/1, close/3]).
-export([register_return_handler/2]).
-export([register_flow_handler/2]).

%% This diagram shows the interaction between the different component
%% processes in an AMQP client scenario.
%%
%%                             message* / reply*        +-------+
%%                            +----------------------   | queue |
%%                            |                         +-------+
%%                            |
%%                            |                          +-----+
%%                            v                          |     |
%%           request                     reply*          |     v
%% +------+  -------+  +--------------+  <------+  +----------------+
%% | User |         |  | amqp_channel |         |  | direct_channel |
%% +------+  <------+  +--------------+  -------+  +----------------+
%%           response /        |          request
%% cast/call         /         |
%%                  /          | message
%%                 /           v
%% +-------------+/       +----------+
%% | Pending RPC |        | Consumer |
%% +-------------+        +----------+
%%       |
%% [consumer tag --> consumer pid]
%%
%% These notifications are processed asynchronously via
%% handle_info/2 callbacks

%%---------------------------------------------------------------------------
%% Type Definitions
%%---------------------------------------------------------------------------

%% @type amqp_command().
%% This abstract datatype represents the set of commands that comprise the
%% AMQP execution model. As indicated in the overview, the attributes of each 
%% commands in the execution model are described in the protocol
%% documentation. The Erlang record definitions are autogenerated from a
%% parseable version of the specification.

%% @type content() = #'basic.publish'{} | 
%%                   #'basic.deliver'{} |
%%                   #'basic.return'{}.
%% These are the content bearing AMQP commands.

%%---------------------------------------------------------------------------
%% AMQP Channel API methods
%%---------------------------------------------------------------------------

%% @spec (Channel, amqp_command()) -> amqp_command()
%% where
%%      Channel = pid()
%% @doc This is a generic RPC mechanism that sends an AMQP command and
%% receives an AMQP command as a response. This function blocks until the
%% response is returned.
call(Channel, Method) ->
    gen_server:call(Channel, {call, Method, none}, infinity).

%% @spec (Channel, amqp_command(), content()) -> ok | blocked | closing
%% where
%%      Channel = pid()
%% @doc This sends an AMQP command with content and waits for a synchronous
%% response. Generally this is used with the #basic.publish{} command.
%% This will return a blocked atom if either the server has throttled the
%% client for flow control reasons or if the channel is shutting down due to a
%% broker initiated close.
%% It will return a closing atom if the channel is in the process of shutting
%% down.
%% Note that the synchronicity only means that the client has transmitted the
%% command to the broker. It does not imply that the broker has accepted
%% responsibility for the message. To acheive guaranteed delivery, this
%% function would have to be called within the context of a transaction.
call(Channel, Method, Content) ->
    gen_server:call(Channel, {call, Method, Content}, infinity).

%% @spec (Channel, amqp_command()) -> ok
%% @doc Asynchronous variant of {@link call/2}
cast(Channel, Method) ->
    gen_server:cast(Channel, {cast, Method, none}).
    
%% @spec (Channel, amqp_command(), content()) -> ok
%% @doc Asynchronous variant of {@link call/3}
cast(Channel, Method, Content) ->
    gen_server:cast(Channel, {cast, Method, Content}).

%% @spec (Channel) -> ok
%% where
%%      Channel = pid()
%% @doc Closes the channel, invokes close(Channel, 200, &lt;&lt;"Goodbye">>).
close(Channel) ->
    close(Channel, 200, <<"Goodbye">>).

%% @spec (Channel, Code, Text) -> ok
%% where
%%      Channel = pid()
%%      Code = integer()
%%      Text = binary()
%% @doc Closes the channel, allowing the caller to supply a reply code and
%% text.
close(Channel, Code, Text) ->
    Close = #'channel.close'{reply_text =  Text,
                             reply_code = Code,
                             class_id   = 0,
                             method_id  = 0},
    #'channel.close_ok'{} = call(Channel, Close),
    ok.
%%---------------------------------------------------------------------------
%% Consumer registration
%%---------------------------------------------------------------------------

%% @type consume() = #'basic.consume'{}.
%% The AMQP command that is used to  subscribe a consumer to a queue.
%% @spec (Channel, consume(), Consumer) -> amqp_command()
%% where
%%      Channel = pid()
%%      Consumer = pid()
%% @doc Creates a subscription to a queue. This subscribes a consumer pid to 
%% the queue defined in the #'basic.consume'{} command record. Note that both
%% both the process invoking this method and the supplied consumer process
%% receive an acknowledgement of the subscription. The calling process will
%% receive the acknowledgement as the return value of this function, whereas
%% the consumer process will receive the notification asynchronously.
subscribe(Channel, BasicConsume = #'basic.consume'{}, Consumer) ->
    gen_server:call(Channel, {subscribe, BasicConsume, Consumer}, infinity).

%% @spec (Channel, ReturnHandler) -> ok
%% where
%%      Channel = pid()
%%      ReturnHandler = pid()
%% @doc This registers a handler to deal with returned messages. The 
%% registered process will receive #basic.return{} commands.
register_return_handler(Channel, ReturnHandler) ->
    gen_server:cast(Channel, {register_return_handler, ReturnHandler} ).

%% @spec (Channel, FlowHandler) -> ok
%% where
%%      Channel = pid()
%%      FlowHandler = pid()
%% @doc This registers a handler to deal with channel flow notifications.
%% The registered process will receive #channel.flow{} commands.
register_flow_handler(Channel, FlowHandler) ->
    gen_server:cast(Channel, {register_flow_handler, FlowHandler} ).

%%---------------------------------------------------------------------------
%% Internal plumbing
%%---------------------------------------------------------------------------

rpc_top_half(Method, From, State) ->
    rpc_top_half(Method, none, From, State).

rpc_top_half(Method, Content, From, 
             State0 = #channel_state{rpc_requests = RequestQueue}) ->
    % Enqueue the incoming RPC request to serialize RPC dispatching
    State1 = State0#channel_state{
        rpc_requests = queue:in({From, Method, Content}, RequestQueue)},
    IsFirstElement = queue:is_empty(RequestQueue),
    if IsFirstElement -> do_rpc(State1);
       true           -> State1
    end.

rpc_bottom_half(Reply, State0 = #channel_state{rpc_requests = RequestQueue}) ->
    case queue:out(RequestQueue) of
        {empty, _} ->
            exit(empty_rpc_bottom_half);
        {{value, {From, _Method, _Content}}, NewRequestQueue} ->
            gen_server:reply(From, Reply),
            State1 = State0#channel_state{rpc_requests = NewRequestQueue},
            do_rpc(State1)
    end.

do_rpc(State = #channel_state{writer_pid = Writer,
                              rpc_requests = RequestQueue,
                              driver = Driver}) ->
    case queue:peek(RequestQueue) of
        {value, {_From, Method = #'channel.close'{}, Content}} ->
            Driver:do(Writer, Method, Content),
            State#channel_state{closing = true};
        {value, {_From, Method, Content}} ->
            Driver:do(Writer, Method, Content),
            State;
        empty ->
            State
    end.

resolve_consumer(_ConsumerTag, #channel_state{consumers = []}) ->
    exit(no_consumers_registered);

resolve_consumer(ConsumerTag, #channel_state{consumers = Consumers}) ->
    dict:fetch(ConsumerTag, Consumers).

register_consumer(ConsumerTag, Consumer,
                  State = #channel_state{consumers = Consumers0}) ->
    Consumers1 = dict:store(ConsumerTag, Consumer, Consumers0),
    State#channel_state{consumers = Consumers1}.

unregister_consumer(ConsumerTag,
                    State = #channel_state{consumers = Consumers0}) ->
    Consumers1 = dict:erase(ConsumerTag, Consumers0),
    State#channel_state{consumers = Consumers1}.

return_handler(State = #channel_state{return_handler_pid = undefined}) ->
    %% TODO what about trapping exits??
    {ok, ReturnHandler} = gen_event:start_link(),
    gen_event:add_handler(ReturnHandler, amqp_return_handler , [] ),
    {ReturnHandler, State#channel_state{return_handler_pid = ReturnHandler}};

return_handler(State = #channel_state{return_handler_pid = ReturnHandler}) ->
    {ReturnHandler, State}.

amqp_msg(none) ->
    none;
amqp_msg(Content) ->
    {Props, Payload} = rabbit_basic:from_content(Content),
    #amqp_msg{props = Props, payload = Payload}.

build_content(none) ->
    none;
build_content(#amqp_msg{props = Props, payload = Payload}) ->
    rabbit_basic:build_content(Props, Payload).

handle_method(Method, Content,
              #channel_state{closing = Closing,
                             driver = Driver,
                             writer_pid = Writer} = State) ->
    case {Method, Content} of
        %% Handle 'channel.close': send 'channel.close_ok' and stop channel
        {#'channel.close'{reply_code = ReplyCode,
                          reply_text = ReplyText}, none} ->
            Driver:do(Writer, #'channel.close_ok'{}, none),
            {stop, {server_initiated_close, ReplyCode, ReplyText}, State};
        %% Handle 'channel.close_ok': stop channel
        {CloseOk = #'channel.close_ok'{}, none} ->
            {stop, normal, rpc_bottom_half(CloseOk, State)};
        _ ->
            if
                %% Drop all incomming traffic except 'channel.close' and
                %% 'channel.close_ok' when channel is closing (has sent
                %% 'channel.close')
                Closing ->
                    ?LOG_INFO("Channel (~p): dropping method ~p from server "
                              "because channel is closing~n",
                              [self(), {Method, Content}]),
                    {noreply, State};
                %% Standard handling of incoming method
                true ->
                    handle_regular_method(Method, amqp_msg(Content), State)
            end
    end.

handle_regular_method(
        #'basic.consume_ok'{consumer_tag = ConsumerTag} = ConsumeOk, none,
        #channel_state{tagged_sub_requests = Tagged,
                       anon_sub_requests = Anon} = State) ->
    {_From, Consumer, State0} =
        case dict:find(ConsumerTag, Tagged) of
            {ok, {F, C}} ->
                NewTagged = dict:erase(ConsumerTag,Tagged),
                {F, C, State#channel_state{tagged_sub_requests = NewTagged}};
            error ->
                case queue:out(Anon) of
                    {empty, _} ->
                        exit({anonymous_queue_empty, ConsumerTag});
                    {{value, {F, C}}, NewAnon} ->
                        {F, C, State#channel_state{anon_sub_requests = NewAnon}}
                end
        end,
    Consumer ! ConsumeOk,
    State1 = register_consumer(ConsumerTag, Consumer, State0),
    {noreply, rpc_bottom_half(ConsumeOk, State1)};

handle_regular_method(
        #'basic.cancel_ok'{consumer_tag = ConsumerTag} = CancelOk, none,
        #channel_state{} = State) ->
    Consumer = resolve_consumer(ConsumerTag, State),
    Consumer ! CancelOk,
    NewState = unregister_consumer(ConsumerTag, State),
    {noreply, rpc_bottom_half(CancelOk, NewState)};

%% Handle 'channel.flow'
%% If flow_control flag is defined, it informs the flow control handler to
%% suspend submitting any content bearing methods
handle_regular_method(#'channel.flow'{active = Active} = Flow, none,
                      #channel_state{flow_handler_pid = FlowHandler,
                                     driver = Driver,
                                     writer_pid = Writer} = State) ->
    case FlowHandler of
        undefined -> ok;
        _         -> FlowHandler ! Flow
    end,
    Driver:do(Writer, #'channel.flow_ok'{active = Active}, none),
    {noreply, State#channel_state{flow_control = not(Active)}};

handle_regular_method(#'basic.deliver'{consumer_tag = ConsumerTag} = Deliver,
                      AmqpMsg, State) ->
    Consumer = resolve_consumer(ConsumerTag, State),
    Consumer ! {Deliver, AmqpMsg},
    {noreply, State};

handle_regular_method(#'basic.return'{} = BasicReturn, AmqpMsg, State) ->
    {ReturnHandler, NewState} = return_handler(State),
    ReturnHandler ! {BasicReturn, AmqpMsg},
    {noreply, NewState};

handle_regular_method(Method, none, State) ->
    {noreply, rpc_bottom_half(Method, State)};

handle_regular_method(Method, Content, State) ->
    {noreply, rpc_bottom_half({Method, Content}, State)}.

check_drop(_Method, _AmqpMsg, #channel_state{closing = true})      -> closing;
check_drop(_Method, none, #channel_state{})                        -> ok;
check_drop(_Method, _AmqpMsg, #channel_state{flow_control = true}) -> blocked;
check_drop(_Method, _AmqpMsg, #channel_state{})                    -> ok.

handle_subscribe(#'basic.consume'{consumer_tag = Tag} = Method, Consumer, From,
                 #channel_state{anon_sub_requests = Subs} = State)
                 when Tag =:= undefined ; size(Tag) == 0 ->
    NewSubs = queue:in({From,Consumer}, Subs),
    NewState = State#channel_state{anon_sub_requests = NewSubs},
    NewMethod =  Method#'basic.consume'{consumer_tag = <<"">>},
    {noreply, rpc_top_half(NewMethod, From, NewState)};

handle_subscribe(#'basic.consume'{consumer_tag = Tag} = Method, Consumer, From,
                 #channel_state{tagged_sub_requests = Subs} = State)
                 when is_binary(Tag) ->
    %% TODO test whether this tag already exists, either in the pending tagged
    %% request map or in general as already subscribed consumer
    NewSubs = dict:store(Tag,{From,Consumer},Subs),
    NewState = State#channel_state{tagged_sub_requests = NewSubs},
    {noreply, rpc_top_half(Method, From, NewState)}.

%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private
init({ChannelState = #channel_state{driver = Driver, number = ChannelNumber},
      ConnectionState}) ->
    process_flag(trap_exit, true),
    {WriterPid, ReaderPid} = Driver:open_channel(ChannelNumber, ConnectionState),
    {ok, ChannelState#channel_state{writer_pid = WriterPid,
                                    reader_pid = ReaderPid}}.

%% Standard implementation of the call/{2,3} command
%% @private
handle_call({call, Method, AmqpMsg}, From,
            #channel_state{writer_pid = Writer,
                           driver = Driver} = State) ->
    case check_drop(Method, AmqpMsg, State) of
        ok        -> Content = build_content(AmqpMsg),
                     case rabbit_framing:is_method_synchronous(Method) of
                         true ->
                             {noreply, rpc_top_half(Method, Content, From,
                                                    State)};
                         false ->
                             Driver:do(Writer, Method, Content),
                             {reply, ok, State}
                     end;
        DropReply -> {reply, DropReply, State}
    end;

%% Standard implementation of the subscribe/3 command
%% @private
handle_call({subscribe, Method, Consumer}, From, State) ->
    handle_subscribe(Method, Consumer, From, State).

%% Standard implementation of the cast/{2,3} command
%% @private
handle_cast({cast, Method, AmqpMsg} = Cast,
            #channel_state{writer_pid = Writer, driver = Driver} = State) ->
    case check_drop(Method, AmqpMsg, State) of
        ok        -> Driver:do(Writer, Method, build_content(AmqpMsg));
        DropReply -> ?LOG_INFO("Channel (~p): discarding method in cast ~p."
                               "Reason: ~p~n", [self(), Cast, DropReply])
    end,
    {noreply, State};

%% Registers a handler to process return messages
%% @private
handle_cast({register_return_handler, ReturnHandler}, State) ->
    NewState = State#channel_state{return_handler_pid = ReturnHandler},
    {noreply, NewState};

%% Registers a handler to process flow control messages
%% @private
handle_cast({register_flow_handler, FlowHandler}, State) ->
    NewState = State#channel_state{flow_handler_pid = FlowHandler},
    {noreply, NewState};

%% @private
handle_cast({notify_sent, _Peer}, State) ->
    {noreply, State};

%% This callback is invoked when a network channel sends messages
%% to this gen_server instance
%% @private
handle_cast({method, Method, Content}, State) ->
    handle_method(Method, Content, State).

%% These callbacks are invoked when a direct channel sends messages
%% to this gen_server instance.
%% @private
handle_info({send_command, Method}, State) ->
    handle_method(Method, none, State);
%% @private
handle_info({send_command, Method, Content}, State) ->
    handle_method(Method, Content, State);

%% Handles the delivery of a message from a direct channel
%% @private
handle_info({send_command_and_notify, Q, ChPid, Method, Content}, State) ->
    handle_method(Method, Content, State),
    rabbit_amqqueue:notify_sent(Q, ChPid),
    {noreply, State};

%% @private
handle_info(shutdown, State) ->
    {stop, normal, State};


%% Handle writer exit
%% @private
handle_info({'EXIT', WriterPid, Reason},
            State = #channel_state{number = ChannelNumber,
                                   writer_pid = WriterPid}) ->
    ?LOG_WARN("Channel ~p closing: received exit signal from writer. "
              "Reason: ~p~n", [ChannelNumber, Reason]),
    {stop, {writer_died, WriterPid, Reason}, State};

%% Handle reader exit
%% @private
handle_info({'EXIT', ReaderPid, Reason},
            State = #channel_state{number = ChannelNumber,
                                   reader_pid = ReaderPid}) ->
    ?LOG_WARN("Channel ~p closing: received exit signal from reader. "
              "Reason: ~p~n", [ChannelNumber, Reason]),
    {stop, {reader_died, ReaderPid, Reason}, State};

%% Handle other exit
%% @private
handle_info({'EXIT', Pid, Reason},
            State = #channel_state{number = ChannelNumber}) ->
    ?LOG_WARN("Channel ~p closing: received unexpected exit signal from (~p). "
              "Reason: ~p~n", [ChannelNumber, Pid, Reason]),
    {stop, {unexpected_exit_signal, Pid, Reason}, State};

%% This is for a channel exception that is sent by the direct
%% rabbit_channel process - in this case this process needs to tell
%% the connection process whether this is a hard error or not
%% @private
handle_info({channel_exit, _Channel, #amqp_error{name = ErrorName}},
            State = #channel_state{parent_connection = Connection}) ->
    {ConError, Code, Text} = rabbit_framing:lookup_amqp_exception(ErrorName),
    case ConError of
        true  -> Connection ! {connection_level_error, Code, Text};
        false -> ok
    end,
    {stop, {server_initiated_close, Code, Text}, State}.

%%---------------------------------------------------------------------------
%% Rest of the gen_server callbacks
%%---------------------------------------------------------------------------

%% @private
terminate(Reason, #channel_state{driver = Driver,
                                 writer_pid = WriterPid,
                                 reader_pid = ReaderPid}) ->
    Driver:close_channel(Reason, WriterPid, ReaderPid).

%% @private
code_change(_OldVsn, State, _Extra) ->
    State.
