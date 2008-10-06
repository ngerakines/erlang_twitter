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
%% 
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.3
%% @doc Provides access to the Twitter web service. Mostly through the
%% clever use of REST-like requests and XML parsing.
%% 
%% This module attempts to provide complete access to the Twitter API. In
%% addition, it provides a simple gen_server process template to allow calls
%% to be made on behalf of a named user without having to send a username
%% and password each time.
%% 
%% When the gen_server feature is used to make Twitter API calls for a user,
%% a gen_server process is spawned locally and its name is prefixed to
%% prevent named process collision.
%% 
%% <strong>Make sure you start inets (<code>inets:start().</code>) before you do
%% anything.</strong>
%% 
%% <h4>Quick start</h4>
%% <pre><code>
%% 1&gt; inets:start().
%% 2&gt; twitter_client:start("myname", "pass").
%% 3&gt; twitter_client:account_verify_credentials("myname", "pass", []).
%%   OR
%% 3&gt; twitter_client:call("myname", account_verify_credentials).
%% 4&gt; twitter_client:call("myname", user_timeline).
%% 5&gt; twitter_client:call("myname", status_update, [{"status", "Testing the erlang_twitter twitter_client.erl library."}]).
%% 6&gt; twitter_client:call("myname", user_timeline).
%% </code></pre>
-module(twitter_client).
-behaviour(gen_server).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.3").

-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

-export([account_archive/4, account_end_session/4, account_rate_limit_status/4,
account_update_delivery_device/4, account_update_location/4, account_verify_credentials/4,
add_session/2, block_create/4, block_destroy/4, build_url/2, call/2, call/3,
collect_account_archive/6, collect_direct_messages/6, collect_favorites/5,
collect_user_followers/5, direct_destroy/4, direct_messages/4, direct_new/4,
direct_sent/4, favorites_create/4, favorites_destroy/4, favorites_favorites/4,
friendship_create/4, friendship_destroy/4, friendship_exists/4, headers/2, help_test/4,
info/0, notification_follow/4, notification_leave/4, parse_status/1, parse_statuses/1,
parse_user/1, parse_users/1, request_url/5, session_from_client/2, set/2, start/0,
status_destroy/4, status_friends_timeline/4, status_public_timeline/4, status_replies/4,
status_show/4, status_update/4, status_user_timeline/4, text_or_default/3, user_featured/4,
user_followers/4, user_friends/4, user_show/4, delay/0]).

-include("twitter_client.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-record(erlang_twitter, {sessions, base_url, delay, lastcall}).

%% @spec start() -> Result
%% where 
%%       Result = {ok, pid()} | Error
%% @doc Start a twitter_client gen_server process for a Twitter user.
start() ->
    inets:start(),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% @spec add_session(Login, Password) -> ok
%% where 
%%       Login = string()
%%       Password = string()
%% @doc Start a twitter_client gen_server process for a Twitter user.
add_session(Login, Password) ->
    gen_server:call({global, ?MODULE}, {add_session, Login, Password}, infinity).

set(base_url, Value) ->
    gen_server:call({global, ?MODULE}, {base_url, Value}, infinity);

set(delay, Value) ->
    gen_server:call({global, ?MODULE}, {delay, Value}, infinity).

info() ->
    gen_server:call({global, ?MODULE}, {info}, infinity).

delay() ->
    gen_server:call({global, ?MODULE}, {should_wait}, infinity).    


%% @equiv call(Client, Method, [])
call(Client, Method) ->
    twitter_client:call(Client, Method, []).

%% @spec call(Client, Method, Args) -> Result
%% where 
%%       Client = string() | atom()
%%       Method = atom()
%%       Args = [{any(), any()}]
%%       Result = any()
%% @doc Make a request to a twitter_client gen_server process for a user.
%% This function attempts to call the named gen_server process for the given
%% client (usern). The method called maps directly to the available methods
%% provided by this module. Please refer to the specific methods for their
%% required and optional arguments. In most (all) cases the arguments
%% defined in the Twitter API documentation can be passed in directly as
%% string/string tuples.
%% 
%% Calling this method does not verify that the given gen_server process
%% exists or is running.
call(Client, Method, Args) ->
    gen_server:call({global, ?MODULE}, {Client, Method, Args}, infinity).

%% % -
%% % gen_server functions

%% @private
init(_) ->
    {ok, #erlang_twitter{
        sessions = gb_trees:empty(),
        base_url = "http://twitter.com/",
        delay = 0,
        lastcall = calendar:datetime_to_gregorian_seconds(erlang:universaltime())
    }}.

%% @private
session_from_client(State, Client) ->
    case gb_trees:is_defined(Client, State#erlang_twitter.sessions) of
        false -> {error, invalid_client};
        true -> gb_trees:get(Client, State#erlang_twitter.sessions)
    end.

handle_call({base_url, BaseUrl}, _From, State) ->
    {reply, ok, State#erlang_twitter{ base_url = BaseUrl }};

handle_call({delay, Delay}, _From, State) ->
    {reply, ok, State#erlang_twitter{ delay = Delay }};

%% Should work .. I think
handle_call({should_wait}, _From, State) ->
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    Delay = case State#erlang_twitter.delay of
        0 -> 0;
        Time when Time + State#erlang_twitter.delay < Now -> 0;
        _ -> State#erlang_twitter.delay
    end,
    {reply, Delay, State};

handle_call({add_session, Login, Password}, _From, State) ->
    NewTree =  case gb_trees:is_defined(Login, State#erlang_twitter.sessions) of
        true -> State#erlang_twitter.sessions;
        false -> gb_trees:insert(Login, {Login, Password}, State#erlang_twitter.sessions)
    end,
    {reply, ok, State#erlang_twitter{ sessions = NewTree }};

handle_call({remove_session, Login}, _From, State) ->
    NewTree =  case gb_trees:is_defined(Login, State#erlang_twitter.sessions) of
        true -> gb_trees:delete(Login, State#erlang_twitter.sessions);
        false -> State#erlang_twitter.sessions
    end,
    {reply, ok, State#erlang_twitter{ sessions = NewTree }};

handle_call({info}, _From, State) ->
    {reply, State, State};

handle_call({Client, Method, Args}, _From, State) ->
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    Response = case session_from_client(State, Client) of
        {error, Reason} -> {error, Reason};
        {Login, Password} ->
            try apply(twitter_client, Method, [State#erlang_twitter.base_url, Login, Password, Args])
            catch
                X:Y ->
                    io:format("error: ~p:~p~n", [X, Y]),
                    {error, unsupported_method}
            end;
        _ -> {error, unknown}
    end,
    {reply, Response, State#erlang_twitter{ lastcall = Now }};

handle_call(stop, _From, State) -> {stop, normalStop, State};

handle_call(_, _From, State) -> {noreply, ok, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% % -
%% % Status API methods

%% @doc Return a list of the most recent tweets as per the public timeline.
%% This API call ignores the login and password strings given.
status_public_timeline(RootUrl, _Login, _Password, Args) ->
    Url = build_url(RootUrl ++ "statuses/public_timeline.xml", Args),
    Body = request_url(get, Url, nil, nil, nil),
    parse_statuses(Body).

status_friends_timeline(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "statuses/friends_timeline",
    Url = case lists:keytake("id", 1, Args) of 
        false ->
            build_url(UrlBase ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} ->
            build_url(UrlBase ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

status_user_timeline(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "statuses/user_timeline",
    Url = case lists:keytake("id", 1, Args) of 
        false ->
            build_url(UrlBase ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} ->
            build_url(UrlBase ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

status_show(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "statuses/show/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

status_update(RootUrl, Login, Password, Args) ->
    Url = RootUrl ++ "statuses/update.xml",
    Body = request_url(post, Url, Login, Password, Args),
    parse_status(Body).

status_replies(RootUrl, Login, Password, Args) ->
    Url = build_url(RootUrl ++ "statuses/replies.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

status_destroy(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "statuses/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

%% % -
%% % Account API methods

account_verify_credentials(RootUrl, Login, Password, _) ->
    Url = build_url(RootUrl ++ "account/verify_credentials.xml", []),
    case request_url(get, Url, Login, Password, nil) of
        "<authorized>true</authorized>" -> true;
        _ -> false
    end.

account_end_session(RootUrl, Login, Password, _) ->
    Url = build_url(RootUrl ++ "account/end_session", []),
    request_url(get, Url, Login, Password, nil).

account_archive(RootUrl, Login, Password, Args) ->
    Url = build_url(RootUrl ++ "account/archive.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

collect_account_archive(RootUrl, Login, Password, Page, Args, Acc) ->
    NArgs = [{"page", integer_to_list(Page)} ] ++ Args,
    Messages = twitter_client:account_archive(RootUrl, Login, Password, NArgs),
    case length(Messages) of
        80 -> collect_account_archive(RootUrl, Login, Password, Page + 1, Args, [Messages | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Messages | Acc])
    end.

account_update_location(RootUrl, Login, Password, Args) ->
    Url = build_url(RootUrl ++ "account/update_location.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_user(Body).

account_update_delivery_device(RootUrl, Login, Password, Args) ->
    Url = build_url(RootUrl ++ "account/update_delivery_device.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_user(Body).

account_rate_limit_status(RootUrl, Login, Password, Args) ->
    Url = build_url(RootUrl ++ "account/rate_limit_status.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    Body.

%% % -
%% % Direct Message API methods

direct_messages(RootUrl, Login, Password, Args) ->
    Url = build_url(RootUrl ++ "direct_messages.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

collect_direct_messages(RootUrl, Login, Password, Page, LowID, Acc) ->
    Args = [{"page", integer_to_list(Page)}, {"since_id", integer_to_list(LowID)}],
    Messages = twitter_client:direct_messages(RootUrl, Login, Password, Args),
    case length(Messages) of
        20 -> collect_direct_messages(RootUrl, Login, Password, Page + 1, LowID, [Messages | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Messages | Acc])
    end.

direct_new(RootUrl, Login, Password, Args) ->
    Url = RootUrl ++ "direct_messages/new.xml",
    Body = request_url(post, Url, Login, Password, Args),
    parse_status(Body).

direct_sent(RootUrl, Login, Password, Args) ->
    Url = build_url(RootUrl ++ "direct_messages/sent.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

direct_destroy(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "direct_messages/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

%% % -
%% % Favorites API methods

favorites_favorites(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "favorites",
    Url = case lists:keytake("id", 1, Args) of 
        false ->
            build_url(UrlBase ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} ->
            build_url(UrlBase ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

collect_favorites(RootUrl, Login, Password, Page, Acc) ->
    Args = [{"page", integer_to_list(Page)}],
    Messages = twitter_client:favorites_favorites(RootUrl, Login, Password, Args),
    case length(Messages) of
        20 -> collect_favorites(RootUrl, Login, Password, Page + 1, [Messages | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Messages | Acc])
    end.

favorites_create(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "favorites/create/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

favorites_destroy(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "favorites/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

%% % -
%% % Friendship API methods

friendship_exists(RootUrl, Login, Password, Args) ->
    Url = build_url(RootUrl ++ "friendships/exists.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    Body == "<friends>true</friends>".

friendship_create(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "friendships/create/",
    case Args of
        [{"id", Id}] ->
            Url = UrlBase ++ Id ++ ".xml",
            Body = request_url(post, Url, Login, Password, Args),
            parse_user(Body);
        _ -> {error}
    end.

friendship_destroy(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "friendships/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_user(Body);
        _ -> {error}
    end.
%% % -
%% % User API methods

user_friends(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "statuses/friends",
    Url = case lists:keytake("id", 1, Args) of 
        false ->
            build_url(UrlBase ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} ->
            build_url(UrlBase ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    Body = request_url(get, Url, Login, Password, nil),
    parse_users(Body).

user_followers(RootUrl, Login, Password, Args) ->
    Url = build_url(RootUrl ++ "statuses/followers.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_users(Body).

collect_user_followers(RootUrl, Login, Password, Page, Acc) ->
    Followers = twitter_client:user_followers(RootUrl, Login, Password, [{"page", integer_to_list(Page)}, {"lite", "true"}]),
    case length(Followers) of
        100 -> collect_user_followers(RootUrl, Login, Password, Page + 1, [Followers | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Followers | Acc])
    end.

user_featured(RootUrl, _, _, _) ->
    Url = build_url(RootUrl ++ "statuses/featured.xml", []),
    Body = request_url(get, Url, nil, nil, nil),
    parse_users(Body).

user_show(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "users/show",
    Url = case lists:keytake("id", 1, Args) of 
        false ->
            build_url(UrlBase ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} ->
            build_url(UrlBase ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    Body = request_url(get, Url, Login, Password, nil),
    parse_user(Body).

%% % -
%% % Notification API methods

notification_follow(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "notifications/follow/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end;
        _ -> {error}
    end.

notification_leave(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "notifications/leave/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end;
        _ -> {error}
    end.

%% % -
%% % Block API methods

block_create(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "blocks/create/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end;
        _ -> {error}
    end.

block_destroy(RootUrl, Login, Password, Args) ->
    UrlBase = RootUrl ++ "blocks/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end;
        _ -> {error}
    end.

%% % -
%% % Help API methods

help_test(RootUrl, _, _, _) ->
    Url = build_url(RootUrl ++ "help/test.xml", []),
    Body = request_url(get, Url, nil, nil, nil),
    Body == "<ok>true</ok>".

%% % -
%% %  Internal request functions

%% @private
build_url(Url, []) -> Url;
build_url(Url, Args) ->
    ArgStr = lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec];
                (Rec, Ac) -> [Rec, "&" | Ac]
            end,
            [],
            [K ++ "=" ++ twitter_client_utils:url_encode(V) || {K, V} <- Args]
        )
    ),
    Url ++ "?" ++ ArgStr.

%% @private
request_url(get, Url, Login, Pass, _) ->
    HTTPResult = http:request(get, {Url, headers(Login, Pass)}, [], []),
    case HTTPResult of
        {ok, {_Status, _Headers, Body}} -> Body;
        _ -> {error}
    end;

request_url(post, Url, Login, Pass, Args) ->
    Body = lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
            [],
            [K ++ "=" ++ twitter_client_utils:url_encode(V) || {K, V} <- Args]
        )
    ),
    HTTPResult = http:request(post, {Url, headers(Login, Pass), "application/x-www-form-urlencoded", Body} , [], []),
    case HTTPResult of
        {ok, {_, _, Res}} -> Res;
        _ -> {error}
    end.

%% @private
headers(nil, nil) -> [{"User-Agent", "ErlangTwitterClient/0.1"}];
headers(User, Pass) when is_binary(User) ->
    headers(binary_to_list(User), Pass);
headers(User, Pass) when is_binary(Pass) ->
    headers(User, binary_to_list(Pass));
headers(User, Pass) -> 
    UP = base64:encode(User ++ ":" ++ Pass),
    Basic = lists:flatten(io_lib:fwrite("Basic ~s", [UP])),
    [{"User-Agent", "ErlangTwitterClient/0.1"}, {"Authorization", Basic}].

%% % -
%% % Response parsing functions

%% @private
parse_statuses(Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', _} -> {error};
        {error, _} -> {error};
        Result ->
            {Xml, _Rest} = Result,
            [parse_status(Node) || Node <- lists:flatten([xmerl_xpath:string("/statuses/status", Xml), xmerl_xpath:string("/direct-messages/direct_message", Xml)])]
    end.

%% @private
parse_status(Node) when is_tuple(Node) ->
    Status = #status{
        created_at = text_or_default(Node, ["/status/created_at/text()", "/direct_message/created_at/text()"], ""),
        id = text_or_default(Node, ["/status/id/text()", "/direct_message/id/text()"], ""),
        text = text_or_default(Node, ["/status/text/text()", "/direct_message/text/text()"], ""),
        source = text_or_default(Node, ["/status/source/text()", "/direct_message/source/text()"], ""),
        truncated = text_or_default(Node, ["/status/truncated/text()", "/direct_message/truncated/text()"], ""),
        in_reply_to_status_id = text_or_default(Node, ["/status/in_reply_to_status_id/text()", "/direct_message/in_reply_to_status_id/text()"], ""),
        in_reply_to_user_id = text_or_default(Node, ["/status/in_reply_to_user_id/text()", "/direct_message/in_reply_to_user_id/text()"], ""),
        favorited = text_or_default(Node, ["/status/favorited/text()", "/direct_message/favorited/text()"], "")
    },
    case xmerl_xpath:string("/status/user|/direct_message/sender", Node) of
        [] -> Status;
        [UserNode] -> Status#status{ user = parse_user(UserNode) }
    end;

%% @private
parse_status(Body) when is_list(Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', _} -> {error, Body};
        {error, _} -> {error, Body};
        Result ->
            {Xml, _Rest} = Result,
            [parse_status(Node) || Node <- xmerl_xpath:string("/status", Xml)]
    end.

%% @private
parse_users(Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', _} -> {error, Body};
        {error, _} -> {error, Body};
        Result ->
            {Xml, _Rest} = Result,
            [parse_user(Node) || Node <- xmerl_xpath:string("/users/user", Xml)]
    end.

%% @private
parse_user(Node) when is_tuple(Node) ->
    UserRec = #user{
        id = text_or_default(Node, ["/user/id/text()", "/sender/id/text()"], ""),
        name = text_or_default(Node, ["/user/name/text()", "/sender/name/text()"], ""),
        screen_name = text_or_default(Node, ["/user/screen_name/text()", "/sender/screen_name/text()"], ""),
        location = text_or_default(Node, ["/user/location/text()", "/sender/location/text()"], ""),
        description = text_or_default(Node, ["/user/description/text()", "/sender/description/text()"], ""),
        profile_image_url = text_or_default(Node, ["/user/profile_image_url/text()", "/sender/profile_image_url/text()"], ""),
        url = text_or_default(Node, ["/user/url/text()", "/sender/url/text()"], ""),
        protected = text_or_default(Node, ["/user/protected/text()", "/sender/protected/text()"], ""),
        followers_count = text_or_default(Node, ["/user/followers_count/text()", "/sender/followers_count/text()"], ""),
        profile_background_color = text_or_default(Node, ["/user/profile_background_color/text()"], ""),
        profile_text_color = text_or_default(Node, ["/user/profile_text_color/text()"], ""),
        profile_link_color = text_or_default(Node, ["/user/profile_link_color/text()"], ""),
        profile_sidebar_fill_color = text_or_default(Node, ["/user/profile_sidebar_fill_color/text()"], ""),
        profile_sidebar_border_color = text_or_default(Node, ["/user/profile_sidebar_border_color/text()"], ""),
        friends_count = text_or_default(Node, ["/user/friends_count/text()"], ""),
        created_at = text_or_default(Node, ["/user/created_at/text()"], ""),
        favourites_count = text_or_default(Node, ["/user/favourites_count/text()"], ""),
        utc_offset = text_or_default(Node, ["/user/utc_offset/text()"], ""),
        time_zone = text_or_default(Node, ["/user/time_zone/text()"], ""),
        following = text_or_default(Node, ["/user/following/text()"], ""),
        notifications = text_or_default(Node, ["/user/notifications/text()"], ""),
        statuses_count = text_or_default(Node, ["/user/statuses_count/text()"], "")
    },
    case xmerl_xpath:string("/user/status", Node) of
        [] -> UserRec;
        [StatusNode] -> UserRec#user{ status = parse_status(StatusNode) }
    end;

%% @private
parse_user(Body) when is_list(Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', _} -> {error, Body};
        {error, _} -> {error, Body};
        Result ->
            {Xml, _Rest} = Result,
            [parse_user(Node) || Node <- xmerl_xpath:string("/user", Xml)]
    end.

%% @private
text_or_default(_, [], Default) -> Default;
text_or_default(Xml, [Xpath | Tail], Default) ->
    Res = lists:foldr(
        fun(#xmlText{value = Val}, Acc) -> lists:append(Val, Acc);
           (_, Acc) -> Acc
        end,
        Default,
        xmerl_xpath:string(Xpath, Xml)
    ),
    text_or_default(Xml, Tail, Res).
