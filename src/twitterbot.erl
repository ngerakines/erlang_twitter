%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.1a
%% @doc A Twitter bot template/example.
%% 
%% This module is still in development and should be considered to be in
%% <strong>ALPHA</strong> stage. For your own sake do not put this into
%% production.
%% 
%% <h4>Quick start</h4>
%% <pre><code>
%% 1&gt; inets:start().
%% 2&gt; twitterbot:start("mybot", "mybotpassword").
%% 3&gt; gen_server:call(twitterbot:clean_name("mybot"), {info}).
%% 4&gt; gen_server:call(twitterbot:clean_name("mybot"), {start_dmloop}).
%% 5&gt; gen_server:call(twitterbot:clean_name("mybot"), {start_flwloop}).
%% 6&gt; gen_server:call(twitterbot:clean_name("mybot"), {followers}).
%% </code></pre>
-module(twitterbot).
-behaviour(gen_server).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.1a").

-compile(export_all).

-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

-include("twitter_client.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(UNIQUEPREFIX,"twitterbot_").

-record(state, {login, password, usertable, datatable, dmloop, flwloop, lastcheck, checkinterval}).
-record(bucket, {id, name, date, total, count}).

clean_name(Name) -> list_to_atom(lists:concat([?UNIQUEPREFIX, Name])).

start(Login, Password) ->
    twitter_client:start(Login, Password),
    gen_server:start_link({local, twitterbot:clean_name(Login)}, ?MODULE, [Login, Password], []).

call(Client, Method) ->
    twitter_client:call(Client, Method, []).

call(Client, Method, Args) ->
    gen_server:call(twitterbot:clean_name(Client), {Method, Args}).

followers_loop(Name) ->
    io:format("Checking for new followers.~n", []),
    
    Followers = twitter_client:call("treasury", collect_user_followers),
    [begin
        gen_server:call(twitterbot:clean_name(Name), {follower, Follower}, infinity)
    end || Follower <- Followers],

    SleepTime = gen_server:call(twitterbot:clean_name(Name), {checkinterval}, infinity),
    timer:sleep(SleepTime),
    twitterbot:followers_loop(Name).

direct_message_loop(Name) ->
    LowId = gen_server:call(twitterbot:clean_name(Name), {lastcheck}, infinity),
    io:format("Checking for new messages: lowid ~p~n", [LowId]),
    
    NewMessages = twitter_client:call(Name, collect_direct_messages, LowId),
    case length(NewMessages) of
        0 -> ok;
        _ ->
            [LastId | _] = lists:usort([Status#status.id || Status <- NewMessages]),
            gen_server:call(twitterbot:clean_name(Name), {lastcheck, list_to_integer(LastId)}, infinity),
            twitterbot:process_direct_messages(NewMessages, Name)
    end,
    
    SleepTime = gen_server:call(twitterbot:clean_name(Name), {checkinterval}, infinity),
    timer:sleep(SleepTime),
    twitterbot:direct_message_loop(Name).

process_direct_messages([], _) -> ok;
process_direct_messages([Message | Messages], Name) ->
    MessageUser = Message#status.user,
    twitterbot:parse_direct_message(Name, MessageUser#user.screen_name, Message#status.text),
    twitterbot:process_direct_messages(Messages, Name).

parse_direct_message(BotName, Name, "i " ++ Message) ->
    TableName = gen_server:call(twitterbot:clean_name(BotName), {datatable}, infinity),
    {Date, _} = calendar:universal_time(),
    NewBucket = lists:foldl(
        fun ("$" ++ X, OldRec) -> OldRec#bucket{ total = clean_price(X) };
            ([X | Y], OldRec) when X > 47, X < 58 ->
                 OldRec#bucket{ total = clean_price([X | Y]) };
            ("price:$" ++ X, OldRec) -> OldRec#bucket{ total = clean_price(X) };
            ("price:" ++ X, OldRec) -> OldRec#bucket{ total = clean_price(X) };
            ("bucket:" ++ X, OldRec) -> OldRec#bucket{ name = X };
            ("date:" ++ X, OldRec) -> OldRec#bucket{ date = clean_date(X) };
            (Bucket, OldRec) -> OldRec#bucket{ name = Bucket }
        end,
        #bucket{ date = Date, count = 0, name = "default"},
        string:tokens(Message, " ")
    ),
    BucketId = bucket_key(Name, NewBucket#bucket.date),
    case dets:lookup(TableName, BucketId) of 
        [_] ->
            dets:update_counter(TableName, BucketId, {4, NewBucket#bucket.total}),
            dets:update_counter(TableName, BucketId, {5, 1}),
            ok;
        _ ->
            dets:insert(TableName, [{BucketId, NewBucket#bucket.name, NewBucket#bucket.date, NewBucket#bucket.total, 1}]),
            ok
    end;
parse_direct_message(_, _, Message) ->
    io:format("Direct message (no match): ~p~n", [Message]),
    ok.

bucket_key(Name, {YY, MM, DD}) ->
    lists:flatten(io_lib:format("~s-~w-~w-~w",[Name, YY, MM, DD])).

clean_date(Str) ->
    case string:tokens(Str, "/") of 
        [A, B, C] -> list_to_tuple([ list_to_integer(X) || X <- [A, B, C]]);
        _ -> {Date, _} = calendar:universal_time(), Date
    end.

clean_price(Str) when is_integer(Str) -> Str;
clean_price(Str) when is_list(Str) ->
    case string:chr(Str, $.) of
        0 -> list_to_integer(Str) + 0.0;
        _ -> list_to_float(Str)
    end.

%% -
%% gen_server functions

%% todo: Add the process to a pg2 pool
init([Login, Password]) ->
    {ok, UserTable} = dets:open_file(list_to_atom(Login ++ "_users"), []),
    {ok, DataTable} = dets:open_file(list_to_atom(Login ++ "_data"), []),
    State = #state{
        login = Login,
        password = Password,
        usertable = UserTable,
        datatable = DataTable,
        checkinterval = 60000 * 5,
        lastcheck = 5000
    },
    {ok, State}.

handle_call({info}, _From, State) -> {reply, State, State};

handle_call({checkinterval}, _From, State) -> {reply, State#state.checkinterval, State};

handle_call({checkinterval, X}, _From, State) -> {reply, ok, State#state{ checkinterval = X }};

handle_call({datatable}, _From, State) -> {reply, State#state.datatable, State};

handle_call({lastcheck}, _From, State) -> {reply, State#state.lastcheck, State};

handle_call({lastcheck, X}, _From, State) -> {reply, ok, State#state{ lastcheck = X }};

handle_call({follower, User}, _From, State) ->
    io:format("Follow: ~p~n", [User]),
    dets:insert(State#state.usertable, [{User#user.id, User#user.name, User#user.screen_name}]),
    {reply, ok, State};

handle_call({followers}, _From, State) ->
    Response = dets:foldl(fun(R, Acc) -> [R | Acc] end, [], State#state.usertable),
    {reply, Response, State};

%% dmloop, flwloop
handle_call({check_dmloop}, _From, State) ->
    Response = case erlang:is_pid(State#state.dmloop) of
        false -> false;
        true -> erlang:is_process_alive(State#state.dmloop)
    end,
    {reply, Response, State};

handle_call({start_dmloop}, _From, State) ->
    case erlang:is_pid(State#state.dmloop) of
        false -> ok;
        true ->
            case erlang:is_process_alive(State#state.dmloop) of
                false -> ok;
                true -> erlang:exit(State#state.dmloop, kill)
            end
    end,
    NewState = State#state{
        dmloop = spawn(twitterbot, direct_message_loop, [State#state.login])
    },
    {reply, ok, NewState};

handle_call({check_flwloop}, _From, State) ->
    Response = case erlang:is_pid(State#state.flwloop) of
        false -> false;
        true -> erlang:is_process_alive(State#state.flwloop)
    end,
    {reply, Response, State};

handle_call({start_flwloop}, _From, State) ->
    case erlang:is_pid(State#state.flwloop) of
        false -> ok;
        true ->
            case erlang:is_process_alive(State#state.flwloop) of
                false -> ok;
                true -> erlang:exit(State#state.flwloop, kill)
            end
    end,
    NewState = State#state{
        flwloop = spawn(twitterbot, followers_loop, [State#state.login])
    },
    {reply, ok, NewState};

handle_call({stop}, _From, State) -> {stop, normalStop, State};

handle_call(stop, _From, State) -> {stop, normalStop, State};

handle_call(_, _From, State) -> {noreply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
