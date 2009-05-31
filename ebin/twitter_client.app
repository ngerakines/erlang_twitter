%%% -*- mode:erlang -*-
{application, erlang_protobuffs, [
    {description, "An Erlang-native Twitter client."},
    {vsn, "0.4.3"},
    {modules, [twitter_client, twitter_client_utils]},
    {registered, []},
    {applications, [kernel, stdlib, inets]},
    {env, []}
]}.
