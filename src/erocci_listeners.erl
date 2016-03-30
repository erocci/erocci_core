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
%%% @doc Sueprvisor for listeners (protocol handlers)
%%%
%%% @end
%%% Created :  6 Aug 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(erocci_listeners).

-behaviour(supervisor).

-include("erocci.hrl").

%% API
-export([start_link/0, 
	 add/1]).

-type opts() :: [{atom(), any()}].
-callback start_link(atom(), opts()) -> ok | {error, atom()}.
-callback terminate(atom(), term()) -> ok.

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).


%% @doc Add a new listener
%% @end
-spec add(occi_listener:t()) -> {ok, pid()} | {error, term()}.
add(L) ->
    supervisor:start_child(?SUPERVISOR, occi_listener:spec(L)).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    ?info("Starting erocci listeners manager"),
    {ok, {{one_for_one, 1000, 6000}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
