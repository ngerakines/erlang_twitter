#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(2),
    etap_can:loaded_ok(twitter_client, "module 'twitter_client' loaded"),
    etap_can:loaded_ok(twitter_client, "module 'twitter_client_utils' loaded"),
    etap:end_tests().
