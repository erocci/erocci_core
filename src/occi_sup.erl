%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2013-2016 Jean Parpaillon
%%% 
%%% This file is provided to you under the license described
%%% in the file LICENSE at the root of the project.
%%%
%%% You can also download the LICENSE file from the following URL:
%%% https://github.com/erocci/erocci/blob/master/LICENSE
%%% 

%% @doc Supervisor for the occi core application.

-module(occi_sup).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%% ChildSpec = {Id, StartFunc, Restart, Shutdown, Type, Modules}
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init(_) ->
    TableMgr = {occi_table_mgr,
		{occi_table_mgr, start_link, []},
		permanent,
		infinity,
		worker,[occi_table_mgr]},
    Config = {occi_config,
	     {occi_config, start_link, []},
	     permanent,
	     infinity,
	     supervisor,
	     [occi_config]},
    Store = {occi_store,
	     {occi_store, start_link, []},
	     permanent,
	     infinity,
	     supervisor,
	     [occi_store]},
    Listener = {occi_listener,
		{occi_listener, start_link, []},
		permanent,
		infinity,
		supervisor,
		[occi_listener]},
    Hook = {occi_hook,
	    {occi_hook, start_link, []},
	    permanent,
	    infinity,
	    worker,
	    [occi_hook]},
    Children = [TableMgr, Config, Store, Listener, Hook],
    {ok, {{one_for_one, 10, 10}, Children}}.
