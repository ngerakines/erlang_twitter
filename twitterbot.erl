-module(twitterbot).
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

-record(state, {login, password, usertable, updateproc, lastcheck, checkinterval}).

clean_name(Name) -> list_to_atom(lists:concat(["twitterbot_", Name])).

start(Login, Password) ->
    twitter_client:start(Login, Password),
    gen_server:start_link({local, clean_name(Login)}, ?MODULE, [Login, Password], []).

call(Client, Method) ->
    twitter_client:call(Client, Method, []).

call(Client, Method, Args) ->
    gen_server:call(clean_name(Client), {Method, Args}).
 
update_loop(Name) ->
    LowId = gen_server:call(twitterbot:clean_name(Name), {lastcheck}),
    io:format("Checking for new messages: lowid ~p~n", [LowId]),
    
    NewMessages = twitter_client:call(list_to_atom(Name), collect_direct_messages, LowId),
    case length(NewMessages) of
        0 -> ok;
        _ ->
            [LastId | _] = lists:usort([Status#status.id || Status <- NewMessages]),
            gen_server:call(twitterbot:clean_name(Name), {lastcheck, LastId})
    end,
    
    SleepTime = gen_server:call(twitterbot:clean_name(Name), {checkinterval}),
    timer:sleep(SleepTime),
    twitterbot:update_loop(Name).

%% -
%% gen_server functions

%% todo: Add the process to a pg2 pool
init([Login, Password]) ->
    UserTable = ets:new(list_to_atom(Login ++ "_users"), [protected, set, named_table]),
    State = #state{
        login = Login,
        password = Password,
        usertable = UserTable,
        checkinterval = 60000 * 5,
        lastcheck = 5000
    },
    {ok, State}.

handle_call({info}, _From, State) -> {reply, State, State};

handle_call({checkinterval}, _From, State) -> {reply, State#state.checkinterval, State};

handle_call({checkinterval, X}, _From, State) -> {reply, ok, State#state{ checkinterval = X }};

handle_call({lastcheck}, _From, State) -> {reply, State#state.lastcheck, State};

handle_call({lastcheck, X}, _From, State) -> {reply, ok, State#state{ lastcheck = X }};

handle_call({check_updater}, _From, State) ->
    Response = case erlang:is_pid(State#state.updateproc) of
        false -> false;
        true -> erlang:is_process_alive(State#state.updateproc)
    end,
    {reply, Response, State};

handle_call({start_updater}, _From, State) ->
    case erlang:is_pid(State#state.updateproc) of
        false -> ok;
        true ->
            case erlang:is_process_alive(State#state.updateproc) of
                false -> ok;
                true -> erlang:exit(State#state.updateproc, kill)
            end
    end,
    NewState = State#state{
        updateproc = spawn(?MODULE, update_loop, [State#state.login])
    },
    {reply, ok, NewState};

handle_call({stop}, _From, State) -> {stop, normalStop, State};

handle_call(stop, _From, State) -> {stop, normalStop, State};

handle_call(_, _From, State) -> {noreply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% -
%% Misc utility functions
