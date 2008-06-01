%% twitter_client:start("mybot", "mybotpass").
%% gen_server:call(mybot, verify).
%% gen_server:call(mybot, user_timeline).
-module(twitter_client).
-behaviour(gen_server).

-compile(export_all).

-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

-define(USER_AGENT, "ErlangTwitterClient/0.1").
-define(ACCEPT_CHARSET, "utf-8").

-include_lib("xmerl/include/xmerl.hrl").

-record(state, {login, password}).
-record(status, {created_at, id, text, source, truncated, in_reply_to_status_id, in_reply_to_user_id, favorited, user}).
-record(user, {id, name, screen_name, location, description, profile_image_url, url, protected, followers_count}).

start(Login, Password) ->
    gen_server:start_link({local, list_to_atom(Login)}, ?MODULE, [Login, Password], []).

init([Login, Password]) ->
    {ok, #state{login = Login, password = Password }}.

handle_call(verify, _From, State) ->
    Response = twitter_client:verify_credentials(State#state.login, State#state.password),
    {reply, Response, State};

handle_call(user_timeline, _From, State) ->
    Response = twitter_client:user_timeline(State#state.login, State#state.password),
    {reply, Response, State};

handle_call(stop, _From, State) -> {stop, normalStop, State};

handle_call(_, _From, State) -> {noreply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% pragma mark -
%% pragma mark Twitter API functions

verify_credentials(Login, Password) ->
    Url = "http://twitter.com/account/verify_credentials.xml",
    Body = request_url(Url, Login, Password),
    case Body of
        "<authorized>true</authorized>" -> true;
        _ -> false
    end.

public_timeline() ->
    Url = "http://twitter.com/statuses/public_timeline.xml",
    Body = request_url(Url, nil, nil),
    parse_statuses(Body).

friends_timeline(Login, Password) ->
    friends_timeline(Login, Password, nil, nil).

friends_timeline(Login, Password, Friend, Since) ->
    ExtOne = case Friend of
        nil -> "friends_timeline.xml";
        _ -> "friends_timeline/" ++ Friend ++ ".xml"
    end,
    ExtTwo = case Since of
        nil -> "";
        _ -> "?since=" ++ Since
    end,
    Url = "http://twitter.com/statuses/" ++ ExtOne ++ ExtTwo,
    Body = request_url(Url, Login, Password),
    parse_statuses(Body).

favorites(Login, Password) ->
    Url = "http://twitter.com/favorites.xml",
    Body = request_url(Url, Login, Password),
    parse_statuses(Body).

user_timeline(Login, Password) ->
    Url = "http://twitter.com/statuses/user_timeline.xml",
    Body = request_url(Url, Login, Password),
    parse_statuses(Body).

%% pragma mark -
%% pragma mark Internal request functions

request_url(Url, Login, Pass) ->
    HTTPResult = http:request(get, {Url, headers(Login, Pass)}, [], []),
    case HTTPResult of
        {ok, {_Status, _Headers, Body}} -> Body;
        _ -> {error}
    end.

headers(nil, nil) -> [{"User-Agent", ?USER_AGENT}];
headers(User, Pass) -> 
    UP = base64:encode(User ++ ":" ++ Pass),
    Basic = lists:flatten(io_lib:fwrite("Basic ~s", [UP])),
    [{"User-Agent", ?USER_AGENT}, {"Authorization", Basic}].

%% pragma mark -
%% pragma mark Response parsing functions

parse_statuses(Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', _} -> {error};
        {error, _} -> {error};
        Result ->
            {Xml, _Rest} = Result,
            [begin
                parse_status(Node)
            end || Node <- xmerl_xpath:string("/statuses/status", Xml)]
    end.

%% todo: Find a better way to do this.
parse_status(Node) ->
    #status{
        created_at = text_or_default(Node, "//created_at/text()", ""),
        id = text_or_default(Node, "//id/text()", ""),
        text = text_or_default(Node, "//text/text()", ""),
        source = text_or_default(Node, "//source/text()", ""),
        truncated = text_or_default(Node, "//truncated/text()", ""),
        in_reply_to_status_id = text_or_default(Node, "//in_reply_to_status_id/text()", ""),
        in_reply_to_user_id = text_or_default(Node, "//in_reply_to_user_id/text()", ""),
        favorited = text_or_default(Node, "//favorited/text()", ""),
        user = #user{
            id = text_or_default(Node, "//user/id/text()", ""),
            name = text_or_default(Node, "//user/name/text()", ""),
            screen_name = text_or_default(Node, "//user/screen_name/text()", ""),
            location = text_or_default(Node, "//user/location/text()", ""),
            description = text_or_default(Node, "//user/description/text()", ""),
            profile_image_url = text_or_default(Node, "//user/profile_image_url/text()", ""),
            url = text_or_default(Node, "//user/url/text()", ""),
            protected = text_or_default(Node, "//user/protected/text()", ""),
            followers_count = text_or_default(Node, "//user/followers_count/text()", "")
        }
    }.

text_or_default(Xml, Xpath, Default) ->
    case xmerl_xpath:string(Xpath, Xml) of
        [ #xmlText{value = Val} ] -> Val;
        _ -> Default
    end.
