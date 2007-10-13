%%---------------------------------------------------------------------------
%% Copyright (c) 2007 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007 LShift Ltd. <query@lshift.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------

-module(mypl_tcp_session).
%% Somewhat loosely based on rfc 2821.
%% Doesn't even begin to address rfc 2822 in any serious way.

%% FIXME: SMTP AUTH

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-record(session, {socket, mode, reverse_path, forward_path, data_buffer}).

reply_line(Code, Text, false) ->
    [integer_to_list(Code), " ", Text, "\r\n"];
reply_line(Code, Text, true) ->
    [integer_to_list(Code), "-", Text, "\r\n"].

reply(Code, Text, State = #session{socket = Socket}) ->
    gen_tcp:send(Socket, reply_line(Code, Text, false)),
    State.

reply_multi(Code, [], State = #session{socket = Socket}) ->
    gen_tcp:send(Socket, reply_line(Code, "nothing to see here, move along", false)),
    State;
reply_multi(Code, [Text], State = #session{socket = Socket}) ->
    gen_tcp:send(Socket, reply_line(Code, Text, false)),
    State;
reply_multi(Code, [Text | More], State = #session{socket = Socket}) ->
    gen_tcp:send(Socket, reply_line(Code, Text, true)),
    reply_multi(Code, More, State).

reset_buffers(State) ->
    State#session{reverse_path = undefined,
		  forward_path = undefined,
		  data_buffer = []}.

address_to_mailbox(Address) ->
    case regexp:match(Address, "[^@]+@") of
	{match, 1, Length} ->
	    string:substr(Address, 1, Length - 1);
	_ ->
	    Address
    end.

split_path_from_params(Str) ->
    case regexp:match(Str, "<[^>]*>") of
	{match, Start, Length} ->
	    Address = string:substr(Str, Start + 1, Length - 2),
	    Params = string:strip(string:substr(Str, Start + Length), left),
	    {address_to_mailbox(Address), Params};
	_ ->
	    case httpd_util:split(Str, " ", 2) of
		{ok, [Address]} ->
		    {address_to_mailbox(Address), ""};
		{ok, [Address, Params]} ->
		    {address_to_mailbox(Address), Params}
	    end
    end.

parse_path_and_parameters(PrefixRegexp, Data) ->
    case regexp:first_match(Data, PrefixRegexp) of
	nomatch ->
	    unintelligible;
	{match, 1, Length} ->
	    PathAndParams = string:strip(string:substr(Data, Length + 1), left),
	    {Path, Params} = split_path_from_params(PathAndParams),
	    {ok, Path, Params}
    end.

handle_command_line(Line, State) ->
    {Command, Data} = case httpd_util:split(Line, " ", 2) of
			  {ok, [C]} -> {string:to_upper(C), ""};
			  {ok, [C, D]} -> {string:to_upper(C), D}
		      end,
    handle_command(Command, Data, State).

handle_command("QUIT", _ClientDomain, State) ->
    {stop, normal, reply(201, "Goodbye",
        reset_buffers(State))};

handle_command("LOCATIONLIST", _, State) ->
    {noreply, reply(202, rfc4627:encode(mypl_server:location_list()),
     reset_buffers(State))};

handle_command("LOCATIONINFO", Name, State) ->
    {ok, Locinfo} = mypl_server:location_info(Name),
    {noreply, reply(203, rfc4627:encode(Locinfo),
     reset_buffers(State))};

% init_location(Name, Height, Floorlevel, Preference, Attributes) ->
handle_command("INITLOCATION", Parameters, State) ->
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(Parameters, [$,])),
    [Name, HeightStr, FloorlevelStr, PreferenceStr] = Tokens,
    {Height, []} = string:to_integer(HeightStr),
    {Preference, []} = string:to_integer(PreferenceStr),
    Floorlevel = case FloorlevelStr of
        "true" -> true;
        "True" -> true;
        "false" -> false;
        "False" -> false;
        none -> undefined
    end,
    mypl_server:init_location(Name, Height, Floorlevel, Preference, []),
    {noreply, reply(204, "ok", reset_buffers(State))};

% store_at_location(Locname, Mui, Quantity, Product, Height)
handle_command("STOREATLOCATION", Parameters, State) ->
    Tokens = lists:map(fun(X) -> string:strip(X) end, string:tokens(Parameters, [$,])),
    [Name, Mui, QuantityStr, Product, HeightStr] = Tokens,
    {Height, []} = string:to_integer(HeightStr),
    {Quantity, []} = string:to_integer(QuantityStr),
    mypl_server:store_at_location(Name, Mui, Quantity, Product, Height),
    {noreply, reply(205, "ok", reset_buffers(State))};


handle_command("VRFY", _UserOrMailboxPossibly, State) ->
    {noreply, reply(252, "Will not VRFY", State)};

handle_command("EXPN", _MailingListPossibly, State) ->
    {noreply, reply(252, "Will not EXPN", State)};

handle_command("HELP", _MaybeCommand, State) ->
    {noreply, reply(502, "Unimplemented", State)};

handle_command("NOOP", _Junk, State) ->
    {noreply, reply(250, "OK", State)};

handle_command(Command, _Data, State) ->
    {noreply, reply(500, "Unsupported command " ++ Command, State)}.

strip_crlf(S) ->
    lists:reverse(strip_crlf1(lists:reverse(S))).

strip_crlf1([$\n, $\r | S]) -> S;
strip_crlf1([$\n | S]) -> S;
strip_crlf1([$\r | S]) -> S.

deliver(ReversePath, Mailbox, DataLinesRev) ->
    io:format("Delivering ~p -> ~p~n~p~n", [ReversePath, Mailbox, lists:reverse(DataLinesRev)]),
    ok.

%---------------------------------------------------------------------------

init([Sock]) ->
    {ok, reset_buffers(#session{socket = Sock,
				mode = initializing})}.

terminate(_Reason, #session{socket = Sock}) ->
    gen_tcp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call(Request, _From, State) ->
    {stop, {bad_call, Request}, State}.

handle_cast({socket_control_transferred, _Sock}, State = #session{socket = Sock}) ->
    inet:setopts(Sock, [{active, true}]),
    {noreply, reply(200, "Hi there", State#session{mode = command})};

handle_cast(Request, State) ->
    {stop, {bad_cast, Request}, State}.

handle_info({tcp, _Sock, FullLine}, State = #session{mode = command}) ->
    handle_command_line(strip_crlf(FullLine), State);

handle_info({tcp_closed, _Sock}, State) ->
    error_logger:warning_msg("SMTP session closed without warning"),
    {stop, normal, State};

handle_info({tcp_error, _Sock, Reason}, State) ->
    error_logger:warning_msg("SMTP session closed with socket error ~p", [Reason]),
    {stop, normal, State};

handle_info(Message, State) ->
    {stop, {bad_info, Message}, State}.
