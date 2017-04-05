%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2013-2016 Jean Parpaillon
%%%
%%% This file is provided to you under the license described
%%% in the file LICENSE at the root of the project.
%%%
%%% You can also download the LICENSE file from the following URL:
%%% https://github.com/erocci/erocci/blob/master/LICENSE
%%%

%% @doc Supervisor for the erocci_core application.
%% @end

-module(erocci_core_sup).
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
  Acls = #{ id => erocci_acls,
            start => {erocci_acls, start_link, []},
            type => worker },
  Listeners = #{ id => erocci_listeners,
                 start => {erocci_listeners, start_link, []},
                 type => supervisor
               },
  Backends = #{ id => erocci_backends,
                start => {erocci_backends, start_link, []},
                type => supervisor
              },
  Children = [Acls, Listeners, Backends],
  {ok, {{one_for_one, 10, 10}, Children}}.
