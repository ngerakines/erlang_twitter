%%% -*- mode:erlang -*-
{application, erlang_twitter, [
    {description, "An Erlang-native Twitter client."},
    {vsn, "0.5"},
    {modules, [twitter_client, twitter_client_utils]},
    {registered, []},
    {applications, [kernel, stdlib, inets]},
    {env, []}
]}.
