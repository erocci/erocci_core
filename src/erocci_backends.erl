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

-record(backends_path,      { depth     :: integer(), 
			      backends  :: maps:map() }).
-record(backends_category,  { id        :: occi_category:id(),
			      backends  :: [occi_backend:id()] }).
-record(backends,           { id        :: occi_backend:id(),
			      backend   :: occi_backend:t() }).

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
    find2(SplittedPath, mnesia:dirty_last(backends_path)).


%% @doc Find first backend handling category id
%% @todo Handle smarter choice when multiple backends handle the category
%% @end
-spec by_category_id(occi_category:id()) -> erocci_backend:t().
by_category_id(Id) ->
    case mnesia:dirty_read(backends_category, Id) of
	[] -> 
	    throw({not_found, Id});
	[#backends_category{ backends=[ BackendId | _ ]}] ->
	    by_id(BackendId)
    end.


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
    case init_tables([{backends_path, ordered_set, record_info(fields, backends_path)},
		      {backends_category, set, record_info(fields, backends_category)},
		      {backends, set, record_info(fields, backends)}]) of
	ok -> {ok, {{one_for_one, 1000, 6000}, []}};
	{error, _}=Err -> Err
    end.


init_tables([]) ->
    ok;

init_tables([ {Table, Type, Fields} | Tail ]) ->
    Opts = [{ram_copies, nodes()},
	    {attributes, Fields},
	    {type, Type},
	    {storage_properties, [{ets, [{read_concurrency, true}]}]}],
    case mnesia:create_table(Table, Opts) of
	{atomic, ok} ->
	    init_tables(Tail);
	{aborted, {already_exists, Table}} ->
	    init_tables(Tail);
	{aborted, _}=Err ->
	    {error, Err}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_backend(Backend) ->
    case mnesia:transaction(fun () -> add_backend_t(Backend) end) of
	{atomic, ok} -> ok;
	{aborted, Err} -> Err
    end.


add_backend_t(Backend) ->
    register_categories(Backend).


register_categories(Backend) ->
    Ext = erocci_backend:model(Backend),
    case occi_models:import(Ext) of
	{ok, Categories} ->
	    ok = lists:foreach(fun (Category) ->
				       register_category(Backend, Category)
			       end, Categories),
	    register_path(Backend);
	{error, _}=Err ->
	    Err
    end.


register_category(Backend, Category) ->
    CatId = occi_category:id(Category),
    BackendId = erocci_backend:id(Backend),
    Backends = case mnesia:read(backends_category, CatId) of
		   [] -> [];
		   [#backends_category{backends=Val}] -> Val
	       end,
    case lists:member(BackendId, Backends) of
	true -> 
	    ok;
	false ->
	    Record = #backends_category{ id=CatId, backends=Backends ++ [BackendId]},
	    mnesia:write(Record)
    end.


register_path(Backend) ->
    Depth = erocci_backend:depth(Backend),
    SameDepthBackends = case mnesia:read(backends_path, Depth) of
			    [] -> #{};
			    [#backends_path{backends=Backends}] -> Backends
			end,
    BackendId = erocci_backend:id(Backend),
    Record = #backends_path{ depth=Depth, 
			     backends=SameDepthBackends#{ erocci_backend:mountpoint(Backend) => BackendId } },
    ok = mnesia:write(Record),
    register_backend(Backend).


register_backend(Backend) ->      
    mnesia:write(#backends{ id=erocci_backend:id(Backend), backend=Backend }).


rm_backend(Backend) ->
    case mnesia:transaction(fun () -> rm_backend_t(Backend) end) of
	{atomic, ok} -> ok;
	{aborted, Err} -> Err
    end.


rm_backend_t(Backend) ->
    unregister_categories(Backend).


unregister_categories(Backend) ->
    BackendId = erocci_backend:id(Backend),
    Fun = fun (#backends_category{id=CatId, backends=Backends}, ok) ->
		  case lists:member(BackendId, Backends) of
		      true ->
			  case lists:delete(BackendId, Backends) of
			      [] ->
				  mnesia:delete({backends_category, CatId});
			      Backends2 ->
				  Record = #backends_category{ id=CatId, backends=Backends2 },
				  mnesia:write(Record)
			  end;
		      false ->
			  ok
		  end;
	      (_, Err) ->
		  {error, Err}
	  end,
    case mnesia:foldl(Fun, ok, backends_category) of
	ok -> unregister_path(Backend);
	Err -> Err
    end.


unregister_path(Backend) ->
    Depth = erocci_backend:depth(Backend),
    case mnesia:read(backends_path, Depth) of
	[] -> 
	    {error, not_found};
	[{_, SameDepthBackends}] ->
	    Record = #backends_path{ depth=Depth, 
				     backends=maps:remove(erocci_backend:mountpoint(Backend), SameDepthBackends) },
	    case mnesia:write(Record) of
		ok -> unregister_backend(Backend);
		Err -> {error, Err}
	    end
    end.


unregister_backend(Backend) ->
    mnesia:delete({backends, erocci_backend:id(Backend)}).


find2(_, '$end_of_table') ->
    throw({backend, no_root});

find2(_Path, 0) ->
    [#backends_path{ backends=#{ [] := Root }}] = mnesia:read(backends_path, 0),
    Root;

find2(Path, Depth) ->
    case mnesia:read(backends_path, Depth) of
	[] ->
	    find2(Path, mnesia:prev(backends_path, Depth));
	[{_, Backends}] ->
	    case lookup(lists:sublist(Path, Depth), maps:keys(Backends), Backends) of
		false -> 
		    find2(Path, mnesia:prev(backends_path, Depth));
		BackendId -> 
		    by_id(BackendId)
	    end
    end.


lookup(_, [], _) ->
    false;

lookup(Path, [ Path | _Tail ], Backends) ->
    maps:get(Path, Backends);

lookup(Path, [ _Mounpoint | Tail], Backends) ->
    lookup(Path, Tail, Backends).


by_id(BackendId) ->
    case mnesia:dirty_read(backends, BackendId) of
	[] -> throw(not_found);
	[#backends{ backend=Backend }] -> Backend
    end.

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
