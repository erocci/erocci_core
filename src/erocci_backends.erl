%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_backends).

-behaviour(supervisor).

-include("erocci.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0,
	 mount/1,
	 umount/1,
	 by_path/1,
	 by_category_id/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).
-define(BY_PATH_TID, path_tid).

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


%% @doc Mount a backend
%% @end
-spec mount(erocci_backend:t()) -> ok | {error, term()}.
mount(Backend) ->
    case supervisor:start_child(?SUPERVISOR, erocci_backend:spec(Backend)) of
	{ok, _} -> add_backend(Backend);
	{ok, _, _} -> add_backend(Backend);
	{error, _}=Err -> Err
    end.


%% @doc Stop backend
%% @end
-spec umount(erocci_backend:t()) -> ok | {error, not_found}.
umount(Backend) ->
    case supervisor:stop_child(?SUPERVISOR, erocci_backend:id(Backend)) of
	ok -> rm_backend(Backend);
	{error, not_found}=Err -> Err
    end.


%% @doc Find backend attached to mountpoint
%% @end
-spec by_path(binary()) -> erocci_backend:t().
by_path(Path) when is_binary(Path) ->
    SplittedPath = binary:split(Path, [<<$/>>], [global, trim_all]),
    find2(SplittedPath, ets:last(?BY_PATH_TID)).


%% @doc Find backends handling category id
%% So far, returns all backends
%% @todo
%% @end
-spec by_category_id(occi_category:id()) -> erocci_backend:t().
by_category_id(Id) ->
    find_category_id(Id, ets:first(?BY_PATH_TID)).

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
    ?info("Starting erocci backends manager"),
    ?BY_PATH_TID = ets:new(?BY_PATH_TID, [ordered_set,
					  {read_concurrency, true},
					  public,
					  named_table
					 ]),
    {ok, {{one_for_one, 1000, 6000}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_backend(Backend) ->
    Depth = erocci_backend:depth(Backend),
    SameDepthBackends = case ets:lookup(?BY_PATH_TID, Depth) of
			    [] -> #{};
			    [{_, Backends}] -> Backends
			end,
    true = ets:insert(?BY_PATH_TID, {Depth, SameDepthBackends#{ erocci_backend:mountpoint(Backend) => Backend }}),
    ok.


rm_backend(Backend) ->
    Depth = erocci_backend:depth(Backend),
    case ets:lookup(?BY_PATH_TID, Depth) of
	[] -> 
	    {error, not_found};
	[{_, SameDepthBackends}] ->
	    true = ets:insert(?BY_PATH_TID, {Depth, maps:remove(erocci_backend:mountpoint(Backend), SameDepthBackends)}),
	    ok
    end.


find_category_id(Id, '$end_of_table') ->
    throw({unhandled_category, Id});

find_category_id(Id, Key) ->
    [Map] = ets:lookup(?BY_PATH_TID, Key),
    case find_category_id2(Id, maps:keys(Map), Map) of
	undefined ->
	    find_category_id(Id, ets:next(?BY_PATH_TID, Key));
	Backend ->
	    Backend
    end.
		 

find_category_id2(_Id, [], _Map) ->
    undefined;

find_category_id2(Id, [ Key | Tail ], Map) ->
    Backend = maps:get(Key, Map),
    case erocci_backend:has_category(Id, Backend) of
	true -> Backend;
	false -> find_category_id2(Id, Tail, Map)
    end.


find2(_, '$end_of_table') ->
    throw({backend, no_root});

find2(_Path, 0) ->
    #{ [] := Root } = ets:lookup_element(?BY_PATH_TID, 0, 2),
    Root;

find2(Path, Depth) ->
    case ets:lookup(?BY_PATH_TID, Depth) of
	[] ->
	    find2(Path, ets:prev(?BY_PATH_TID, Depth));
	[{_, Backends}] ->
	    case lookup(lists:sublist(Path, Depth), maps:keys(Backends), Backends) of
		false -> find2(Path, ets:prev(?BY_PATH_TID, Depth));
		Backend -> Backend
	    end
    end.


lookup(_, [], _) ->
    false;

lookup(Path, [ Path | _Tail ], Backends) ->
    maps:get(Path, Backends);

lookup(Path, [ _Mounpoint | Tail], Backends) ->
    lookup(Path, Tail, Backends).

%%%
%%% eunit
%%%
-ifdef(TEST).
find_test_() ->
    application:ensure_all_started(erocci_core),
    add_backend(erocci_backend:new({root, dummy, [], <<"/">>})),
    add_backend(erocci_backend:new({un, dummy, [], <<"/un">>})),
    add_backend(erocci_backend:new({undeux, dummy, [], <<"/un/deux">>})),
    add_backend(erocci_backend:new({undeuxtrois, dummy, [], <<"/un/deux/trois">>})),
    add_backend(erocci_backend:new({deux, dummy, [], <<"/deux">>})),
    [
     ?_assertMatch(root,        erocci_backend:id(by_path(<<"/">>))),
     ?_assertMatch(root,        erocci_backend:id(by_path(<<"/trois">>))),
     ?_assertMatch(un,          erocci_backend:id(by_path(<<"/un">>))),
     ?_assertMatch(un,          erocci_backend:id(by_path(<<"/un/">>))),
     ?_assertMatch(un,          erocci_backend:id(by_path(<<"/un//">>))),
     ?_assertMatch(un,          erocci_backend:id(by_path(<<"/un/un">>))),
     ?_assertMatch(undeux,      erocci_backend:id(by_path(<<"/un/deux/quatre">>))),
     ?_assertMatch(undeuxtrois, erocci_backend:id(by_path(<<"/un/deux/trois/quatre">>))),
     ?_assertMatch(deux,        erocci_backend:id(by_path(<<"/deux/un">>)))
    ].
-endif.
