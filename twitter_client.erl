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
%% @version 0.2
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
-version("0.2").

-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

-export([account_archive/3, account_end_session/3,
    account_update_delivery_device/3, account_update_location/3,
    account_verify_credentials/3, block_create/3, block_destroy/3,
    build_url/2, call/2, call/3, clean_name/1, collect_account_archive/5,
    collect_direct_messages/5, collect_favorites/4, collect_user_followers/4,
    direct_destroy/3, direct_messages/3, direct_new/3, direct_sent/3,
    favorites_create/3, favorites_destroy/3, favorites_favorites/3,
    friendship_create/3, friendship_destroy/3, friendship_exists/3,
    headers/2, help_test/3, notification_follow/3, notification_leave/3,
    parse_status/1, parse_statuses/1, parse_user/1, parse_users/1,
    request_url/5, start/2, status_destroy/3, status_friends_timeline/3,
    status_public_timeline/3, status_replies/3, status_show/3,
    status_update/3, status_user_timeline/3, text_or_default/3,
    user_featured/3, user_followers/3, user_friends/3, user_show/3
]).

-include("twitter_client.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(UNIQUEPREFIX,"twitterclient_").

-record(state, {login, password}).

%% @spec start(Login, Password) -> Result
%% where 
%%       Login = string()
%%       Password = string()
%%       Result = {ok, pid()} | Error
%% @doc Start a twitter_client gen_server process for a Twitter user.
start(Login, Password) ->
    gen_server:start_link({local, clean_name(Login)}, ?MODULE, [Login, Password], []).

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
    gen_server:call(clean_name(Client), {Method, Args}, infinity).

%% @private
clean_name(Name) when is_list(Name) ->
    list_to_atom(lists:concat([?UNIQUEPREFIX, Name]));

%% @private
clean_name(Name) when is_atom(Name) ->
    list_to_atom(lists:concat([?UNIQUEPREFIX, atom_to_list(Name)])).

%% % -
%% % gen_server functions

%% @private
init([Login, Password]) ->
    {ok, #state{login = Login, password = Password }}.

%% @private
handle_call({collect_direct_messages, LowID}, _From, State) ->
    Messages = twitter_client:collect_direct_messages(State#state.login, State#state.password, 0, LowID, []),
    {reply, Messages, State};

handle_call({collect_user_followers, _}, _From, State) ->
    Followers = twitter_client:collect_user_followers(State#state.login, State#state.password, 0, []),
    {reply, Followers, State};

handle_call({Method, Args}, _From, State) ->
    Response = case erlang:function_exported(twitter_client, Method, 3) of 
        false -> {error, unsupported_method};
        _ -> apply(twitter_client, Method, [State#state.login, State#state.password, Args])
    end,
    {reply, Response, State};

handle_call(stop, _From, State) -> {stop, normalStop, State};

handle_call(_, _From, State) -> {noreply, ok, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% % -
%% % Status API methods

%% @doc Return a list of the most recent tweets as per the public timeline.
%% This API call ignores the login and password strings given.
status_public_timeline(_Login, _Password, Args) ->
    Url = build_url("http://twitter.com/statuses/public_timeline.xml", Args),
    Body = request_url(get, Url, nil, nil, nil),
    parse_statuses(Body).

status_friends_timeline(Login, Password, Args) ->
    UrlBase = "http://twitter.com/statuses/friends_timeline",
    Url = case lists:keytake("id", 1, Args) of 
        false ->
            build_url(UrlBase ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} ->
            build_url(UrlBase ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

status_user_timeline(Login, Password, Args) ->
    UrlBase = "http://twitter.com/statuses/user_timeline",
    Url = case lists:keytake("id", 1, Args) of 
        false ->
            build_url(UrlBase ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} ->
            build_url(UrlBase ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

status_show(Login, Password, Args) ->
    UrlBase = "http://twitter.com/statuses/show/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

status_update(Login, Password, Args) ->
    Url = "http://twitter.com/statuses/update.xml",
    Body = request_url(post, Url, Login, Password, Args),
    parse_status(Body).

status_replies(Login, Password, Args) ->
    Url = build_url("http://twitter.com/statuses/replies.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

status_destroy(Login, Password, Args) ->
    UrlBase = "http://twitter.com/statuses/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

%% % -
%% % Account API methods

account_verify_credentials(Login, Password, _) ->
    Url = build_url("http://twitter.com/account/verify_credentials.xml", []),
    case request_url(get, Url, Login, Password, nil) of
        "<authorized>true</authorized>" -> true;
        _ -> false
    end.

account_end_session(Login, Password, _) ->
    Url = build_url("http://twitter.com/account/end_session", []),
    request_url(get, Url, Login, Password, nil).

account_archive(Login, Password, Args) ->
    Url = build_url("http://twitter.com/account/archive.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

collect_account_archive(Login, Password, Page, Args, Acc) ->
    NArgs = [{"page", integer_to_list(Page)} ] ++ Args,
    Messages = twitter_client:account_archive(Login, Password, NArgs),
    case length(Messages) of
        80 -> collect_account_archive(Login, Password, Page + 1, Args, [Messages | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Messages | Acc])
    end.

account_update_location(Login, Password, Args) ->
    Url = build_url("http://twitter.com/account/update_location.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_user(Body).

account_update_delivery_device(Login, Password, Args) ->
    Url = build_url("http://twitter.com/account/update_delivery_device.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_user(Body).

%% % -
%% % Direct Message API methods

direct_messages(Login, Password, Args) ->
    Url = build_url("http://twitter.com/direct_messages.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

collect_direct_messages(Login, Password, Page, LowID, Acc) ->
    Args = [{"page", integer_to_list(Page)}, {"since_id", integer_to_list(LowID)}],
    Messages = twitter_client:direct_messages(Login, Password, Args),
    case length(Messages) of
        20 -> collect_direct_messages(Login, Password, Page + 1, LowID, [Messages | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Messages | Acc])
    end.

direct_new(Login, Password, Args) ->
    Url = "http://twitter.com/direct_messages/new.xml",
    Body = request_url(post, Url, Login, Password, Args),
    parse_status(Body).

direct_sent(Login, Password, Args) ->
    Url = build_url("http://twitter.com/direct_messages/sent.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

direct_destroy(Login, Password, Args) ->
    UrlBase = "http://twitter.com/direct_messages/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

%% % -
%% % Favorites API methods

favorites_favorites(Login, Password, Args) ->
    UrlBase = "http://twitter.com/favorites",
    Url = case lists:keytake("id", 1, Args) of 
        false ->
            build_url(UrlBase ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} ->
            build_url(UrlBase ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    Body = request_url(get, Url, Login, Password, nil),
    parse_statuses(Body).

collect_favorites(Login, Password, Page, Acc) ->
    Args = [{"page", integer_to_list(Page)}],
    Messages = twitter_client:favorites_favorites(Login, Password, Args),
    case length(Messages) of
        20 -> collect_favorites(Login, Password, Page + 1, [Messages | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Messages | Acc])
    end.

favorites_create(Login, Password, Args) ->
    UrlBase = "http://twitter.com/favorites/create/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

favorites_destroy(Login, Password, Args) ->
    UrlBase = "http://twitter.com/favorites/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_status(Body);
        _ -> {error}
    end.

%% % -
%% % Friendship API methods

friendship_exists(Login, Password, Args) ->
    Url = build_url("http://twitter.com/friendships/exists.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    Body == "<friends>true</friends>".

friendship_create(Login, Password, Args) ->
    UrlBase = "http://twitter.com/friendships/create/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_user(Body);
        _ -> {error}
    end.

friendship_destroy(Login, Password, Args) ->
    UrlBase = "http://twitter.com/friendships/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            parse_user(Body);
        _ -> {error}
    end.
%% % -
%% % User API methods

user_friends(Login, Password, Args) ->
    UrlBase = "http://twitter.com/statuses/friends",
    Url = case lists:keytake("id", 1, Args) of 
        false ->
            build_url(UrlBase ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} ->
            build_url(UrlBase ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    Body = request_url(get, Url, Login, Password, nil),
    parse_users(Body).

user_followers(Login, Password, Args) ->
    Url = build_url("http://twitter.com/statuses/followers.xml", Args),
    Body = request_url(get, Url, Login, Password, nil),
    parse_users(Body).

collect_user_followers(Login, Password, Page, Acc) ->
    Followers = twitter_client:user_followers(Login, Password, [{"page", integer_to_list(Page)}, {"lite", "true"}]),
    case length(Followers) of
        100 -> collect_user_followers(Login, Password, Page + 1, [Followers | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Followers | Acc])
    end.

user_featured(_, _, _) ->
    Url = build_url("http://twitter.com/statuses/featured.xml", []),
    Body = request_url(get, Url, nil, nil, nil),
    parse_users(Body).

user_show(Login, Password, Args) ->
    UrlBase = "http://twitter.com/users/show",
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

notification_follow(Login, Password, Args) ->
    UrlBase = "http://twitter.com/notifications/follow/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end;
        _ -> {error}
    end.

notification_leave(Login, Password, Args) ->
    UrlBase = "http://twitter.com/notifications/leave/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end;
        _ -> {error}
    end.

%% % -
%% % Block API methods

block_create(Login, Password, Args) ->
    UrlBase = "http://twitter.com/blocks/create/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end;
        _ -> {error}
    end.

block_destroy(Login, Password, Args) ->
    UrlBase = "http://twitter.com/blocks/destroy/",
    case Args of
        [{"id", Id}] ->
            Url = build_url(UrlBase ++ Id ++ ".xml", []),
            Body = request_url(get, Url, Login, Password, nil),
            case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end;
        _ -> {error}
    end.

%% % -
%% % Help API methods

help_test(_, _, _) ->
    Url = build_url("http://twitter.com/help/test.xml", []),
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
            [K ++ "=" ++ yaws_api:url_encode(V) || {K, V} <- Args]
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
            [K ++ "=" ++ yaws_api:url_encode(V) || {K, V} <- Args]
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
    case xmerl_xpath:string(Xpath, Xml) of
        [ #xmlText{value = Val} ] -> Val;
        _ -> text_or_default(Xml, Tail, Default)
    end.
