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
%% @copyright 2008-2009 Nick Gerakines
%% @version 0.4
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

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.5").

-export([
    status_friends_timeline/2,
    status_home_timeline/2,
    status_user_timeline/2,
    status_mentions/2,
    status_show/2,
    status_update/2,
    status_replies/2,
    status_destroy/2,
    account_archive/2, collect_account_archive/4,
    account_update_location/2,
    account_update_delivery_device/2,
    account_rate_limit_status/2,
    direct_messages/2, collect_direct_messages/4,
    direct_new/2,
    direct_sent/2,
    direct_destroy/2,
    favorites/2, collect_favorites/3,
    favorites_create/2,
    favorites_destroy/2,
    friendship_create/2,
    friendship_destroy/2,
    user_followers/2, collect_user_followers/3,
    user_friends/2, collect_user_friends/3,
    user_featured/2,
    user_show/2,
    user_list_memberships/2,
    user_list_subscriptions/2,
    list/2,
    lists/2,
    list_statuses/2,
    list_members/2,
    notification_follow/2,
    notification_leave/2,
    block_create/2,
    block_destroy/2,
    help_test/2,
    social_graph_follower_ids/2,
    social_graph_friend_ids/2,
    friendship_exists/2,
    account_end_session/2,
    account_verify_credentials/2, 
    headers/2,
    parse_status/1, parse_statuses/1, parse_user/1, parse_users/1, request_url/5,
    text_or_default/3,
    build_url/2
]).

-include("twitter_client.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(BASE_URL(X), "http://www.twitter.com/" ++ X).

status_home_timeline(Auth, Args) when is_tuple(Auth), is_list(Args) ->
    Url = build_url("statuses/home_timeline.xml", Args),
    request_url(get, Url, Auth, [], fun(X) -> parse_statuses(X) end).

status_friends_timeline(Auth, Args) when is_tuple(Auth), is_list(Args) ->
    Url = case lists:keytake("id", 1, Args) of 
        false -> build_url("statuses/friends_timeline" ++ ".xml", Args);
        {_, {"id", Id}, RetArgs} -> build_url("statuses/friends_timeline" ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(X) -> parse_statuses(X) end).

status_user_timeline(Auth, Args) ->
    Url = case lists:keytake("id", 1, Args) of 
        false -> build_url("statuses/user_timeline" ++ ".xml", Args);
        {_, {"id", Id}, RetArgs} -> build_url("statuses/user_timeline" ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(X) -> parse_statuses(X) end).

status_mentions(Auth, Args) ->
    Url = build_url("statuses/mentions.xml", Args),
    request_url(get, Url, Auth, [], fun(X) -> parse_statuses(X) end).

status_show(Auth, [{"id", Id}]) ->
    Url = build_url("statuses/show/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(X) -> parse_status(X) end).

status_update(Auth, Args) ->
    request_url(post, "statuses/update.xml", Auth, Args, fun(X) -> parse_status(X) end).

status_replies(Auth, Args) ->
    Url = build_url("statuses/replies.xml", Args),
    request_url(get, Url, Auth, [], fun(X) -> parse_statuses(X) end).

status_destroy(Auth, [{"id", Id}]) ->
    Url = build_url("statuses/destroy/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(X) -> parse_statuses(X) end).

account_end_session(Auth, _) ->
    Url = build_url("account/end_session", []),
    request_url(get, Url, Auth, [], fun(_) -> ok end).

account_archive(Auth, Args) ->
    Url = build_url("account/archive.xml", Args),
    request_url(get, Url, Auth, [], fun(X) -> parse_statuses(X) end).

collect_account_archive(Auth, Page, Args, Acc) ->
    NArgs = [{"page", integer_to_list(Page)} ] ++ Args,
    Messages = twitter_client:account_archive(Auth, NArgs),
    %% NKG: Assert that `Messages` is a list?
    case length(Messages) of
        80 -> collect_account_archive(Auth, Page + 1, Args, [Messages | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Messages | Acc])
    end.

account_update_location(Auth, Args) ->
    Url = build_url("account/update_location.xml", Args),
    request_url(get, Url, Auth, [], fun(X) -> parse_user(X) end).

account_update_delivery_device(Auth, Args) ->
    Url = build_url("account/update_delivery_device.xml", Args),
    request_url(get, Url, Auth, [], fun(X) -> parse_user(X) end).

account_rate_limit_status(Auth, Args) ->
    Url = build_url("account/rate_limit_status.xml", Args),
    request_url(get, Url, Auth, [], fun(X) -> parse_rate_limit(X) end).

direct_messages(Auth, Args) ->
    Url = build_url("direct_messages.xml", Args),
    request_url(get, Url, Auth, [], fun(X) -> parse_messages(X) end).

collect_direct_messages(Auth, Page, LowID, Acc) ->
    Args = [{"page", integer_to_list(Page)}, {"since_id", integer_to_list(LowID)}],
    Messages = twitter_client:direct_messages(Auth, Args),
    %% NKG: Assert that `Messages` is a list?
    case length(Messages) of
        20 -> collect_direct_messages(Auth, Page + 1, LowID, [Messages | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Messages | Acc])
    end.

direct_new(Auth, Args) ->
    request_url(post, "direct_messages/new.xml", Auth, Args, fun(Body) -> parse_message(Body) end).

direct_sent(Auth, Args) ->
    Url = build_url("direct_messages/sent.xml", Args),
    request_url(get, Url, Auth, [], fun(Body) -> parse_messages(Body) end).

direct_destroy(Auth, [{"id", Id}]) ->
    Url = build_url("direct_messages/destroy/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) -> parse_status(Body) end).

favorites(Auth, Args) ->
    Url = case lists:keytake("id", 1, Args) of 
        false -> build_url("favorites" ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} -> build_url("favorites" ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(Body) -> parse_statuses(Body) end).

collect_favorites(Auth, Page, Acc) ->
    Messages = favorites(Auth, [{"page", integer_to_list(Page)}]),
    case length(Messages) of
        20 -> collect_favorites(Auth, Page + 1, [Messages | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Messages | Acc])
    end.

favorites_create(Auth, [{"id", Id}]) ->
    Url = build_url("favorites/create/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) -> parse_status(Body) end).

favorites_destroy(Auth, [{"id", Id}]) ->
    Url = build_url("favorites/destroy/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) -> parse_status(Body) end).

friendship_exists(Auth, Args) ->
    Url = build_url("friendships/exists.xml", Args),
    request_url(get, Url, Auth, [], fun(Body) -> Body == "<friends>true</friends>" end).

friendship_create(Auth, [{"id", Id}]) ->
    Url = "friendships/create/" ++ Id ++ ".xml",
    request_url(post, Url, Auth, [], fun(Body) -> parse_user(Body) end).

friendship_destroy(Auth, [{"id", Id}]) ->
    Url = build_url("friendships/destroy/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) -> parse_user(Body) end).

user_friends(Auth, Args) ->
    Url = case lists:keytake("id", 1, Args) of 
        false -> build_url("statuses/friends" ++ ".xml", Args);
        {_, {"id", Id}, RetArgs} -> build_url("statuses/friends" ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(Body) -> parse_users(Body) end).

collect_user_friends(Auth, Page, Acc) ->
    Friends = user_friends(Auth, [{"page", integer_to_list(Page)}, {"lite", "true"}]),
    case length(Friends) of
      100 -> collect_user_friends(Auth, Page + 1, [Friends | Acc]);
      0 -> lists:flatten(Acc);
      _ -> lists:flatten([Friends | Acc])
    end.

user_followers(Auth, Args) ->
    Url = build_url("statuses/followers.xml", Args),
    request_url(get, Url, Auth, [], fun(Body) -> parse_users(Body) end).

collect_user_followers(Auth, Page, Acc) ->
    Followers = user_followers(Auth, [{"page", integer_to_list(Page)}, {"lite", "true"}]),
    case length(Followers) of
        100 -> collect_user_followers(Auth, Page + 1, [Followers | Acc]);
        0 -> lists:flatten(Acc);
        _ -> lists:flatten([Followers | Acc])
    end.

user_featured(_, _) ->
    Url = build_url("statuses/featured.xml", []),
    request_url(get, Url, {nil, nil}, [], fun(Body) -> parse_users(Body) end).

user_show(Auth, Args) ->
    Url = case lists:keytake("id", 1, Args) of 
        false -> build_url("users/show" ++ ".xml", Args);
        {value, {"id", Id}, RetArgs} -> build_url("users/show" ++ "/" ++ Id ++ ".xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(Body) -> parse_user(Body) end).

user_list_memberships(Auth, Args) ->
    Login = case Auth of {X, _} -> X; {X, _, _, _} -> X end,
    Url = case lists:keytake("id", 1, Args) of 
	false -> build_url("/" ++ Login ++ "/lists/memberships.xml", []);
	{_, {"id", Id}, RetArgs} -> build_url("/" ++ Id ++ "/lists/memberships.xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(Body) -> parse_lists(Body) end).    

user_list_subscriptions(Auth, Args) ->
    Login = case Auth of {X, _} -> X; {X, _, _, _} -> X end,
    Url = case lists:keytake("id", 1, Args) of 
	false -> build_url("/" ++ Login ++ "/lists/subscriptions.xml", []);
	{_, {"id", Id}, RetArgs} -> build_url("/" ++ Id ++ "/lists/subscriptions.xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(Body) -> parse_lists(Body) end). 

list(Auth, Args) ->
    Login = case lists:keytake("id", 1, Args) of
	false -> case Auth of {X, _} -> X; {X, _, _, _} -> X end;
	{_, {"id", Id}, _} -> Id end, 
    Url = case lists:keytake("listid", 1, Args) of 
        {_, {"listid", ListId}, RetArgs} -> build_url("/" ++ Login ++ "/lists/" ++ ListId ++  ".xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(Body) -> parse_list(Body) end).

lists(Auth, Args) ->
    Login = case Auth of {X, _} -> X; {X, _, _, _} -> X end,
    Url = case lists:keytake("id", 1, Args) of 
	false -> build_url("/" ++ Login ++ "/lists.xml", []);
	{_, {"id", Id}, RetArgs} -> build_url("/" ++ Id ++ "/lists.xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(Body) -> parse_lists(Body) end).

list_statuses(Auth, Args) ->
    Login = case lists:keytake("id", 1, Args) of
	false -> case Auth of {X, _} -> X; {X, _, _, _} -> X end;
	{_, {"id", Id}, _} -> Id end, 
    Url = case lists:keytake("listid", 1, Args) of 
        {_, {"listid", ListId}, RetArgs} -> build_url("/" ++ Login ++ "/lists/" ++ ListId ++  "/statuses.xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(Body) -> parse_statuses(Body) end).
    
list_members(Auth, Args) -> 
    Login = case lists:keytake("id", 1, Args) of
	false -> case Auth of {X, _} -> X; {X, _, _, _} -> X end;
	{_, {"id", Id}, _} -> Id end, 
    Url = case lists:keytake("listid", 1, Args) of 
        {_, {"listid", ListId}, RetArgs} -> build_url("/" ++ Login ++ "/" ++ ListId ++  "/members.xml", RetArgs)
    end,
    request_url(get, Url, Auth, [], fun(Body) -> parse_list_users(Body) end).

notification_follow(Auth, [{"id", Id}]) ->
    Url = build_url("notifications/follow/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) ->
        case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end
    end).

notification_leave(Auth, [{"id", Id}]) ->
    Url = build_url("notifications/leave/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) ->
        case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end
    end).

block_create(Auth, [{"id", Id}]) ->
    Url = build_url("blocks/create/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) ->
        case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end
    end).

block_destroy(Auth, [{"id", Id}]) ->
    Url = build_url("blocks/destroy/" ++ Id ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) ->
        case parse_user(Body) of [#user{ screen_name = Id }] -> true; _ -> false end
    end).

help_test(_, _) ->
    Url = build_url("help/test.xml", []),
    request_url(get, Url, {nil, nil}, [], fun(Body) -> Body == "<ok>true</ok>" end).

social_graph_friend_ids(Auth, _) ->
    Login = case Auth of {X, _} -> X; {X, _, _, _} -> X end,
    Url = build_url("friends/ids/" ++ twitter_client_utils:url_encode(Login) ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) -> parse_ids(Body) end).

social_graph_follower_ids(Auth, _) ->
    Login = case Auth of {X, _} -> X; {X, _, _, _} -> X end,
    Url = build_url("followers/ids/" ++ twitter_client_utils:url_encode(Login) ++ ".xml", []),
    request_url(get, Url, Auth, [], fun(Body) -> parse_ids(Body) end).

account_verify_credentials({Login, Password}, _) ->
    Url = build_url("account/verify_credentials.xml", []),
    case httpc:request(get, {Url, headers(Login, Password)}, [], []) of
        {ok, {{_HTTPVersion, 200, _Text}, _Headers, _Body}} -> true;
        {ok, {{_HTTPVersion, 401, _Text}, _Headers, _Body}} -> false;
        _ -> {error}
    end;
account_verify_credentials({Consumer, Token, Secret}, _) ->
    Url = build_url("account/verify_credentials.xml", []),
    case oauth:get(Url, [], Consumer, Token, Secret) of
        {ok, {{_HTTPVersion, 200, _Text}, _Headers, _Body}} -> true;
        {ok, {{_HTTPVersion, 401, _Text}, _Headers, _Body}} -> false;
        _ -> {error}
    end.

build_url(Url, []) -> Url;
build_url(Url, Args) ->
    Url ++ "?" ++ lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end, [],
            [K ++ "=" ++ twitter_client_utils:url_encode(V) || {K, V} <- Args]
        )
    ).

request_url(get, Url, {Login, Pass}, _, Fun) ->
    case httpc:request(get, {?BASE_URL(Url), headers(Login, Pass)}, [{timeout, 6000}], []) of
        {ok, {_, _, Body}} -> Fun(Body);
        Other -> {error, Other}
    end;
request_url(post, Url, {Login, Pass}, Args, Fun) ->
    Body = twitter_client_utils:compose_body(Args),
    case httpc:request(post, {?BASE_URL(Url), headers(Login, Pass), "application/x-www-form-urlencoded", Body} , [{timeout, 6000}], []) of
        {ok, {_, _, Body2}} -> Fun(Body2);
        Other -> {error, Other}
    end;
request_url(get, Url, {Consumer, Token, Secret}, Args, Fun) ->
    case oauth:get(?BASE_URL(Url), Args, Consumer, Token, Secret) of
        {ok, {_, _, "Failed to validate oauth signature or token"}} -> {oauth_error, "Failed to validate oauth signature or token"};
        {ok, {_, _, Body}} -> Fun(Body);
        Other -> Other
    end;
request_url(post, Url, {Consumer, Token, Secret}, Args, Fun) ->
    case oauth:post(?BASE_URL(Url), Args, Consumer, Token, Secret) of
        {ok, {_, _, "Failed to validate oauth signature or token"}} -> {oauth_error, "Failed to validate oauth signature or token"};
        {ok, {_, _, Body}} -> Fun(Body);
        Other -> Other
    end.

headers(nil, nil) -> [{"User-Agent", "ErlangTwitterClient/0.1"}];
headers(User, Pass) when is_binary(User) ->
    headers(binary_to_list(User), Pass);
headers(User, Pass) when is_binary(Pass) ->
    headers(User, binary_to_list(Pass));
headers(User, Pass) ->
    Basic = "Basic " ++ binary_to_list(base64:encode(User ++ ":" ++ Pass)),
    [{"User-Agent", "ErlangTwitterClient/0.1"}, {"Authorization", Basic}, {"Host", "twitter.com"}].

parse_statuses(Body) ->
    parse_generic(Body, fun(Xml) ->
        [status_rec(Node) || Node <- lists:flatten([xmerl_xpath:string("/statuses/status", Xml), xmerl_xpath:string("/direct-messages/direct_message", Xml)])]
    end).

parse_ids(Body) ->
    parse_generic(Body, fun(Xml) ->
        [parse_id(Node) || Node <- xmerl_xpath:string("/ids/id", Xml)]
    end).

parse_status(Body) when is_list(Body) ->
    parse_generic(Body, fun(Xml) ->
        [status_rec(Node) || Node <- xmerl_xpath:string("/status", Xml)]
    end).

parse_messages(Body) ->
    parse_generic(Body, fun(Xml) ->
        [message_rec(Node) || Node <- lists:flatten([xmerl_xpath:string("/direct-messages/direct_message", Xml)])]
    end).

parse_message(Body) when is_list(Body) ->
    parse_generic(Body, fun(Xml) ->
        [message_rec(Node) || Node <- xmerl_xpath:string("/direct_message", Xml)]
    end).

parse_users(Body) ->
    parse_generic(Body, fun(Xml) ->
        [user_rec(Node) || Node <- xmerl_xpath:string("/users/user", Xml)]
    end).

parse_user(Body) when is_list(Body) ->
    parse_generic(Body, fun(Xml) -> [user_rec(Node) || Node <- xmerl_xpath:string("/user", Xml)] end).

parse_list(Body) ->
    parse_generic(Body, fun(Xml) -> [list_rec(Node) || Node <- xmerl_xpath:string("/list", Xml)] end).

parse_lists(Body) ->
    parse_generic(Body, fun(Xml) ->
	[list_rec(Node) || Node <- xmerl_xpath:string("/lists_list/lists/list", Xml)]
    end).

parse_list_users(Body) ->
    parse_generic(Body, fun(Xml) ->
	[user_rec(Node) || Node <- xmerl_xpath:string("/users_list/users/user", Xml)]
    end).	

status_rec(Node) when is_tuple(Node) ->
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
        [UserNode] -> Status#status{ user = user_rec(UserNode) }
    end.

message_rec(Node) when is_tuple(Node) ->
    #message{
        created_at = text_or_default(Node, ["/direct_message/created_at/text()"], ""), 
        id = text_or_default(Node, ["/direct_message/id/text()"], ""),
        text = text_or_default(Node, ["/direct_message/text/text()"], ""),
        sender_id = text_or_default(Node, ["/direct_message/sender_id/text()"], ""), 
        recipient_id = text_or_default(Node, ["/direct_message/recipient_id/text()"], ""), 
        sender_screen_name = text_or_default(Node, ["/direct_message/sender_screen_name/text()"], ""), 
        recipient_screen_name = text_or_default(Node, ["/direct_message/recipient_screen_name/text()"], ""), 
        sender = case xmerl_xpath:string("/direct_message/sender", Node) of
            [] -> "";
            [SenderNode] -> user_rec(SenderNode)
        end,
        recipient = case xmerl_xpath:string("/direct_message/recipient", Node) of
            [] -> "";
            [RecipientNode] -> user_rec(RecipientNode)
        end
    }.

user_rec(Node) when is_tuple(Node) ->
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
        [StatusNode] -> UserRec#user{ status = status_rec(StatusNode) }
    end.

list_rec(Node) when is_tuple(Node) ->
    ListRec = #list{
	id = text_or_default(Node, ["id/text()"], ""),
	name = text_or_default(Node, ["name/text()"], ""),
	full_name = text_or_default(Node, ["full_name/text()"], ""),
	slug = text_or_default(Node, ["slug/text()"], ""),
	description = text_or_default(Node, ["description/text()"], ""),
	subscriber_count = text_or_default(Node, ["subscriber_count/text()"], ""),
	member_count = text_or_default(Node, ["member_count/text()"], ""),
	uri = text_or_default(Node, ["uri/text()"], ""),
	mode = text_or_default(Node, ["name/text()"], "")
    },
    case xmerl_xpath:string("/list/user", Node) of
	[] -> ListRec;
	[UserNode] -> ListRec#list{ user = user_rec(UserNode) }
    end.

parse_rate_limit(Node) when is_tuple(Node) ->
    #rate_limit{
        reset_time = text_or_default(Node, ["/hash/reset-time/text()"], ""),
        reset_time_in_seconds = int_or_default(Node, ["/hash/reset-time-in-seconds/text()"], ""),
        remaining_hits = int_or_default(Node, ["/hash/remaining-hits/text()"], ""),
        hourly_limit = int_or_default(Node, ["/hash/hourly-limit/text()"], "")
    };

parse_rate_limit(Body) when is_list(Body) ->
    parse_generic(Body, fun(Xml) -> [parse_rate_limit(Node) || Node <- xmerl_xpath:string("/hash", Xml)] end).

parse_generic(Body, Fun) ->
    try xmerl_scan:string(Body, [{quiet, true}]) of
        Result ->
            {Xml, _Rest} = Result,
            Fun(Xml)
    catch _:_ -> {error, Body} end.

parse_id(Node) ->
    Text = text_or_default(Node, ["/id/text()"], "0"),
    twitter_client_utils:string_to_int(Text).

text_or_default(_, [], Default) -> Default;
text_or_default(Xml, [Xpath | Tail], Default) ->
    Res = lists:foldr(
        fun (#xmlText{value = Val}, Acc) -> lists:append(Val, Acc); (_, Acc) -> Acc end,
        Default,
        xmerl_xpath:string(Xpath, Xml)
    ),
    text_or_default(Xml, Tail, Res).

int_or_default(_Xml, [], Default) -> Default;
int_or_default(Xml, Xpath, Default) ->
    twitter_client_utils:string_to_int(text_or_default(Xml, Xpath, Default)).

