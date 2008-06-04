-module(twitter_client).
-behaviour(gen_server).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.1").

-compile(export_all).

-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

-include("twitter_client.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-record(state, {login, password}).

%% todo: Do some checking to see if the process already exists.
start(Login, Password) ->
    gen_server:start_link({local, list_to_atom(Login)}, ?MODULE, [Login, Password], []).

call(Client, Method) ->
    twitter_client:call(Client, Method, []).

call(Client, Method, Args) when is_atom(Client) ->
    gen_server:call(Client, {Method, Args}, infinity).

%% -
%% gen_server functions

%% todo: Add the process to a pg2 pool
init([Login, Password]) ->
    {ok, #state{login = Login, password = Password }}.

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

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% -
%% Status API methods

status_public_timeline(_, _, Args) ->
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

%% -
%% Account API methods

account_verify_credentials(Login, Password, _) ->
    Url = build_url("http://twitter.com/account/verify_credentials.xml", []),
    case request_url(get, Url, Login, Password, nil) of
        "<authorized>true</authorized>" -> true;
        _ -> false
    end.

account_end_session(Login, Password, _) ->
    Url = build_url("http://twitter.com/account/end_session", []),
    request_url(get, Url, Login, Password, nil).

%% -
%% Direct Message API methods

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

%% -
%% User API methods

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
    parse_statuses(Body).

%% -
%% Internal request functions

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

headers(nil, nil) -> [{"User-Agent", "ErlangTwitterClient/0.1"}];
headers(User, Pass) -> 
    UP = base64:encode(User ++ ":" ++ Pass),
    Basic = lists:flatten(io_lib:fwrite("Basic ~s", [UP])),
    [{"User-Agent", "ErlangTwitterClient/0.1"}, {"Authorization", Basic}].

%% -
%% Response parsing functions

parse_statuses(Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', _} -> {error};
        {error, _} -> {error};
        Result ->
            {Xml, _Rest} = Result,
            [parse_status(Node) || Node <- xmerl_xpath:string("/statuses/status|/direct-messages/direct_message", Xml)]
    end.

%% todo: Find a better way to do this.
parse_status(Node) when is_tuple(Node) ->
    Status = #status{
        created_at = text_or_default(Node, "/status/created_at/text()|/direct_message/created_at/text()", ""),
        id = text_or_default(Node, "/status/id/text()|/direct_message/id/text()", ""),
        text = text_or_default(Node, "/status/text/text()|/direct_message/text/text()", ""),
        source = text_or_default(Node, "/status/source/text()|/direct_message/source/text()", ""),
        truncated = text_or_default(Node, "/status/truncated/text()|/direct_message/truncated/text()", ""),
        in_reply_to_status_id = text_or_default(Node, "/status/in_reply_to_status_id/text()|/direct_message/in_reply_to_status_id/text()", ""),
        in_reply_to_user_id = text_or_default(Node, "/status/in_reply_to_user_id/text()|/direct_message/in_reply_to_user_id/text()", ""),
        favorited = text_or_default(Node, "/status/favorited/text()|/direct_message/favorited/text()", "")
    },
    case xmerl_xpath:string("/status/user|/direct_message/user", Node) of
        [] -> Status;
        [UserNode] -> Status#status{ user = parse_user(UserNode) }
    end;

parse_status(Body) when is_list(Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', _} -> {error, Body};
        {error, _} -> {error, Body};
        Result ->
            {Xml, _Rest} = Result,
            [parse_status(Node) || Node <- xmerl_xpath:string("/status", Xml)]
    end.

parse_users(Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', _} -> {error, Body};
        {error, _} -> {error, Body};
        Result ->
            {Xml, _Rest} = Result,
            [parse_user(Node) || Node <- xmerl_xpath:string("/users/user", Xml)]
    end.

parse_user(Node) when is_tuple(Node) ->
    UserRec = #user{
        id = text_or_default(Node, "/user/id/text()", ""),
        name = text_or_default(Node, "/user/name/text()", ""),
        screen_name = text_or_default(Node, "/user/screen_name/text()", ""),
        location = text_or_default(Node, "/user/location/text()", ""),
        description = text_or_default(Node, "/user/description/text()", ""),
        profile_image_url = text_or_default(Node, "/user/profile_image_url/text()", ""),
        url = text_or_default(Node, "/user/url/text()", ""),
        protected = text_or_default(Node, "/user/protected/text()", ""),
        followers_count = text_or_default(Node, "/user/followers_count/text()", ""),
        profile_background_color = text_or_default(Node, "/user/profile_background_color/text()", ""),
        profile_text_color = text_or_default(Node, "/user/profile_text_color/text()", ""),
        profile_link_color = text_or_default(Node, "/user/profile_link_color/text()", ""),
        profile_sidebar_fill_color = text_or_default(Node, "/user/profile_sidebar_fill_color/text()", ""),
        profile_sidebar_border_color = text_or_default(Node, "/user/profile_sidebar_border_color/text()", ""),
        friends_count = text_or_default(Node, "/user/friends_count/text()", ""),
        created_at = text_or_default(Node, "/user/created_at/text()", ""),
        favourites_count = text_or_default(Node, "/user/favourites_count/text()", ""),
        utc_offset = text_or_default(Node, "/user/utc_offset/text()", ""),
        time_zone = text_or_default(Node, "/user/time_zone/text()", ""),
        following = text_or_default(Node, "/user/following/text()", ""),
        notifications = text_or_default(Node, "/user/notifications/text()", ""),
        statuses_count = text_or_default(Node, "/user/statuses_count/text()", "")
    },
    case xmerl_xpath:string("/user/status", Node) of
        [] -> UserRec;
        [StatusNode] -> UserRec#user{ status = parse_status(StatusNode) }
    end;

parse_user(Body) when is_list(Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', _} -> {error, Body};
        {error, _} -> {error, Body};
        Result ->
            {Xml, _Rest} = Result,
            [parse_user(Node) || Node <- xmerl_xpath:string("/user", Xml)]
    end.

text_or_default(Xml, Xpath, Default) ->
    case xmerl_xpath:string(Xpath, Xml) of
        [ #xmlText{value = Val} ] -> Val;
        _ -> Default
    end.
