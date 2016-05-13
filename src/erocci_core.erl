%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2013-2016 Jean Parpaillon
%%% 
%%% This file is provided to you under the license described
%%% in the file LICENSE at the root of the project.
%%%
%%% You can also download the LICENSE file from the following URL:
%%% https://github.com/erocci/erocci/blob/master/LICENSE
%%% 
%%% @doc
%%%
%%% @end
%%% Created :  6 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(erocci_core).

-include("erocci_log.hrl").

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, 
	 start_phase/3,
	 stop/1]).

%% @doc Start the erocci_core application
%% @end
-spec start() -> {ok, [atom()]} | {error, term()}.
start() ->
    applicaton:ensure_all_started(erocci_core).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(normal, _Args) ->
    erocci_core_sup:start_link();

start(_StartType, _StartArgs) ->
    {error, badarg}.


%% @doc Start phase `config' start listeners and backends, once
%% supervisors have been started
%% @end
-spec start_phase(Phase :: atom(), Type :: atom(), Args :: term()) -> ok | {error, term()}.
start_phase(listeners, normal, _Args) ->
    start_listeners(erocci_config:get(listeners));

start_phase(backends, normal, _Args) ->
    start_backends(erocci_config:get(backends), false);

start_phase(_, _, _) ->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.


%%%
%%% Priv
%%%
start_listeners([]) ->
    ok;

start_listeners([ L | Tail ]) ->
    case erocci_listeners:add(L) of
	{ok, Pid} ->
	    ?info("Started listener ~p: ~p", [erocci_listener:id(L), Pid]),
	    start_listeners(Tail);
	{error, _}=Err ->
	    Err
    end.


start_backends([], true) ->
    ok;

start_backends([], false) ->
    ?info("No root backend, mount default one"),
    %%start_backends2(erocci_backend:default(), [], true);
    ok;

start_backends([ B | Tail ], true) ->
    case erocci_backend:is_root(B) of
	true ->
	    {error, dulicate_root_backend};
	false ->
	    start_backends2(B, Tail, true)
    end;

start_backends([ B | Tail ], false) ->
    start_backends2(B, Tail, erocci_backend:is_root(B)).


start_backends2(Backend, Others, Root) ->
    case erocci_backends:mount(Backend) of
	{ok, _Pid} ->
	    ?info("Mounted backend ~p on ~s", [erocci_backend:id(Backend), erocci_backend:path(Backend)]),
	    start_backends(Others, Root);
	{error, _}=Err ->
	    Err
    end.
