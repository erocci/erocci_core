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
%%% Created :  1 Jul 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(erocci_backend).
-behaviour(gen_server).

-include_lib("occi/include/occi_types.hrl").

-include("erocci_log.hrl").

%% API
-export([new/1,
	 default/0,
	 spec/1,
	 id/1,
	 mountpoint/1,
	 path/1,
	 depth/1,
	 is_root/1,
	 mnesia_disc_copies/1,
	 start_link/1]).

%% Callbacks wrappers
-export([models/1,
	 get/2,
	 create/4,
	 update/3,
	 link/4,
	 action/3,
	 delete/2,
	 mixin/4,
	 unmixin/3,
	 collection/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type capability() :: undefined.

-define(TIMEOUT, 10000).
%%-define(TIMEOUT, infinity).

-record(state, {ref             :: atom(),
                mod             :: atom(),
		capabilities    :: [capability()],
		model           :: occi_extension:t(),
                state           :: term()}).

-type id() :: term().
-record(backend, { id             :: id(),
		   handler        :: atom(),
		   opts           :: term(),
		   mountpoint     :: [binary()],
		   raw_mountpoint :: binary(),
		   depth          :: integer()
		 }).
-type t() :: #backend{}.

-type backend_error() :: not_found
		       | conflict
		       | {internal, _}.
-type error() :: backend_error()
	       | occi_rendering:error().

-export_type([t/0,
	      capability/0,
	      error/0]).

%%%
%%% Callbacks
%%%
-callback mnesia_disc_copies(Opts :: term()) ->
    [node()].
-optional_callbacks([mnesia_disc_copies/1]).


-callback init(Opts :: term()) ->
    {ok, Caps :: [capability()], State :: term()} |
    {error, Reason :: term()}.


-callback terminate(State :: term()) -> ok.


-callback models(State :: term()) -> 
    {{ok, [occi_extension:t()]}
     | {error, error()}, NewState :: term()}.


-callback get(Location :: binary(), State :: term()) ->
    {{ok, occi_collection:t() | occi_entity:t(), erocci_creds:user(), erocci_creds:group(), erocci_node:serial()} 
     | {error, error()}, NewState :: term()}.


-callback create(Location :: binary(), Entity :: occi_entity:t(), Owner :: erocci_creds:user(), Group :: erocci_creds:group(), State :: term()) ->
    {{ok, occi_entity:t(), erocci_node:serial()}
     | {error, error()}, NewState :: term()}.


-callback create(Entity :: occi_entity:t(), Owner :: erocci_creds:user(), Group :: erocci_creds:group(), State :: term()) ->
    {{ok, occi_uri:url(), occi_entity:t(), erocci_node:serial()} 
     | {error, error()}, NewState :: term()}.


-callback update(Location :: occi_uri:url(), Attributes :: maps:map(), State :: term()) ->
    {{ok, Entity2 :: occi_entity:t(), erocci_node:serial()}
     | {error, error()}, NewState :: term()}.


-callback link(Location :: occi_uri:url(), 
	       Type :: source | target, LinkId :: occi_link:id(), State :: term()) ->
    {ok | {error, error()}, NewState :: term()}.


-callback action(Location :: occi_uri:url(), ActionId :: occi_action:id(), Attributes :: maps:map(), State :: term()) ->
    {{ok, occi_entity:t(), erocci_node:serial()}
     | {error, error()}, NewState :: term()}.


-callback delete(Id :: binary(), State :: term()) ->
    {ok | {error, error()}, NewState :: term()}.


-callback mixin(Location :: occi_uri:url(), MixinId :: occi_mixin:t(), Attributes :: maps:map(), State :: term()) ->
    {{ok, occi_entity:t(), erocci_node:serial()}
     | {error, error()}, NewState :: term()}.


-callback unmixin(Location :: occi_uri:url(), MixinId :: occi_mixin:t(), State :: term()) ->
    {{ok, occi_entity:t(), erocci_node:serial()}
     | {error, error()}, NewState :: term()}.


-callback collection(Id :: occi_category:id() | binary(),
		     Filter :: erocci_filter:t(),
		     Start :: integer(), Number :: integer() | undefined,
		     State :: term()) ->
    {{ok, [{occi_entity:t(), erocci_creds:user(), erocci_creds:group()}], erocci_node:serial()}
     | {error, error()}, NewState :: term()}.


%% @doc Creates new backend entry
%% @throws {backend, term()}
%% @end
-spec new({Id :: term(), Mod :: atom(), Opts :: term(), Mountpoint :: binary() | string()}) -> t().
new({Id, Mod, Opts, Mountpoint}) when is_list(Mountpoint) ->
    new({Id, Mod, Opts, list_to_binary(Mountpoint)});

new({Id, Mod, Opts, << $/, _/binary >> = Path}) when is_atom(Mod) ->
    Mountpoint = binary:split(Path, [<<$/>>], [global, trim_all]),
    #backend{ id=Id, handler=Mod, opts=Opts, 
	      raw_mountpoint=occi_utils:normalize(Path),
	      depth=length(Mountpoint),
	      mountpoint=Mountpoint };

new({_Id, Mod, _Opts, Mountpoint}) when is_binary(Mountpoint), is_atom(Mod) ->
    throw({backend, {invalid_mountpoint, Mountpoint}});

new(Config) ->
    throw({backend, {invalid_config, Config}}).


%% @doc Default backend
%% @end
default() ->
    new({default, erocci_backend_mnesia, [], <<"/">>}).


%% @doc 
%% @end
-spec id(t()) -> id().
id(#backend{ id=Id }) ->
    Id.


%% @doc
%% @end
-spec mountpoint(t()) -> [binary()].
mountpoint(#backend{ mountpoint=Mountpoint }) ->
    Mountpoint.


%% @doc
%% @end
-spec path(t()) -> binary().
path(#backend{ raw_mountpoint=Raw }) ->
    Raw.


%% @doc Return mountpoint length
%% @end
-spec depth(t()) -> integer().
depth(#backend{ depth=Depth }) ->
    Depth.


%% @doc is root backend ?
%% @end
-spec is_root(t()) -> boolean().
is_root(#backend{ depth=Depth }) ->
    0 =:= Depth.


%% @doc 
%% @end
-spec spec(t()) -> supervisor:child_spec().
spec(#backend{ id=Id, handler=Mod}=B) ->
    #{ id => Id,
       start => {?MODULE, start_link, [B]},
       modules => [?MODULE, Mod] 
     }.


%% @doc Get nodes on which a schema must exists
%% @end
-spec mnesia_disc_copies(t()) -> [node()].
mnesia_disc_copies(#backend{ handler=Mod, opts=Opts }) ->
    _ = code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, mnesia_disc_copies, 1) of
    	true ->
    	    Mod:mnesia_disc_copies(Opts);
    	false ->
    	    []
    end.


%% @doc Start backend
%% @end
-spec start_link(t()) -> {ok, pid()} | ignore | {error, term()}.
start_link(#backend{id=Id, handler=Mod}=Backend) ->
    ?info("Starting storage backend ~p (~s)", [Id, Mod]),
    gen_server:start_link({local, Id}, ?MODULE, Backend, []).


%%% 
%%% Callback wrappers
%%%

%% @doc Get backend models
%% @end
-spec models(t()) -> [occi_extension:t()].
models(#backend{ id=B }) ->
    case gen_server:call(B, {models, []}, ?TIMEOUT) of
	{ok, Extensions} ->
	    Extensions;
	{error, Err} ->
	    throw(Err)
    end.


%% @doc Lookup for a node at Path
%% @end
-spec get(t(), binary()) -> {ok, erocci_node:t()} | {error, error()}.
get(#backend{ id=B, raw_mountpoint=Prefix }, Location) ->
    case gen_server:call(B, {get, [occi_uri:change_prefix(rm, Prefix, Location)]}, ?TIMEOUT) of
	{ok, Entity, Owner, Group, Serial} ->
	    Entity2 = occi_entity:change_prefix(add, Prefix, Entity),
	    {ok, erocci_node:entity(Entity2, Owner, Group, Serial)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Create a new entity
%% @end
-spec create(t(), occi_entity:t(), erocci_creds:user(), erocci_creds:group()) -> {ok, erocci_entity:t()} | {error, error()}.
create(B, Entity, Owner, Group) ->
    case occi_entity:location(Entity) of
	undefined ->
	    create_and_gen_location(B, Entity, Owner, Group);
	Id ->
	    create_with_location(B, Id, Entity, Owner, Group)
    end.


%% @doc Update an entity
%% @end
-spec update(t(), Entity :: occi_entity:t(), Attributes :: maps:map()) -> ok | {error, error()}.
update(#backend{ id=B, raw_mountpoint=Prefix }, Entity, Attributes) ->
    Location = occi_uri:change_prefix(rm, Prefix, occi_entity:location(Entity)),
    case gen_server:call(B, {update, [Location, Attributes]}, ?TIMEOUT) of
	{ok, Entity2, Serial} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2), Serial};
	{error, _}=Err ->
	    Err
    end.


%% @doc Creates a link of type `Type' between resource and link id.
%% @end
-spec link(t(), Resource :: occi_resource:t(), Type :: source | target, LinkId :: occi_link:id()) -> 
		  ok | {error, error()}.
link(#backend{ id=B, raw_mountpoint=Prefix }, Resource, Type, LinkId) ->
    Location = occi_uri:change_prefix(rm, Prefix, occi_entity:location(Resource)),
    gen_server:call(B, {link, [Location, Type, LinkId]}, ?TIMEOUT).


%% @doc Invoke an action on an existing entity
%% @end
-spec action(t(), occi_invoke:t(), occi_entity:t()) -> {ok, occi_entity:t(), erocci_node:serial()} | {error, error()}.
action(#backend{ id=B, raw_mountpoint=Prefix }, Invoke, Entity) ->
    Location = occi_uri:change_prefix(rm, Prefix, occi_entity:location(Entity)),
    ActionId = occi_invoke:id(Invoke),
    Attributes = occi_invoke:attributes(Invoke),
    case gen_server:call(B, {action, [Location, ActionId, Attributes]}, ?TIMEOUT) of
	{ok, Entity2, Serial} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2), Serial};
	{error, _}=Err ->
	    Err
    end.


%% @doc Delete an entity
%% @end
-spec delete(t(), Id :: binary()) -> ok | {error, error()}.
delete(#backend{ id=B, raw_mountpoint=Prefix }, Id) when is_binary(Id) ->
    gen_server:call(B, {delete, [occi_uri:change_prefix(rm, Prefix, Id)]}, ?TIMEOUT).


%% @doc Add a mixin to an existing entity
%% @end
-spec mixin(t(), occi_entity:t(), occi_mixin:t(), maps:map()) -> {ok, occi_entity:t()} | {error, error()}.
mixin(#backend{ id=B, raw_mountpoint=Prefix }, Entity, Mixin, Attributes) ->
    Location = occi_uri:change_prefix(rm, Prefix, occi_entity:location(Entity)),
    case gen_server:call(B, {mixin, [Location, Mixin, Attributes]}, ?TIMEOUT) of
	{ok, Entity2, Serial} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2), Serial};
	{error, _}=Err ->
	    Err
    end.


%% @doc Remove mixin from existing entity
%% @end
-spec unmixin(t(), occi_entity:t(), occi_mixin:t()) -> {ok, occi_entity:t()} | {error, error()}.
unmixin(#backend{ id=B, raw_mountpoint=Prefix }, Entity, Mixin) ->
    Location = occi_uri:change_prefix(rm, Prefix, occi_entity:location(Entity)),
    case gen_server:call(B, {unmixin, [Location, Mixin]}, ?TIMEOUT) of
	{ok, Entity2, Serial} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2), Serial};
	{error, _}=Err ->
	    Err
    end.


%% @doc Retrieve a list of entities
%% @end
-spec collection(t(),
		 Id :: occi_category:id() | binary(),
		 Filter :: erocci_filter:t(),
		 Start :: integer(), Number :: integer() | undefined) ->
			{ok, [erocci_node:t()], erocci_node:serial()} | {error, error()}.
collection(#backend{ id=B, raw_mountpoint=Prefix }, Id, Filter, Start, Number) ->
    Id2 = case Id of
	      Path when is_binary(Path) -> occi_uri:change_prefix(rm, Prefix, Path);
	      CatId when ?is_category_id(CatId) -> CatId
	  end,
    case gen_server:call(B, {collection, [Id2, Filter, Start, Number]}, ?TIMEOUT) of
	{ok, Items, Serial} ->
	    Nodes = lists:map(fun ({Entity, Owner, Group}) -> 
				       erocci_node:entity(occi_entity:change_prefix(add, Prefix, Entity), Owner, Group)
			       end, Items),
	    {ok, Nodes, Serial};
	{error, _}=Err ->
	    Err
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(t()) -> {ok, term()} | {stop, term()} | ignore.
init(#backend{id=Id, handler=Mod, opts=Opts}) ->
    try Mod:init(Opts) of
        {ok, Capabilities, BackendState} ->
	    S = #state{ref=Id, mod=Mod, capabilities=Capabilities, state=BackendState},
	    {ok, S};
        {error, Err} ->
            {stop, Err}
    catch _:Err ->
            {stop, Err}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({Cmd, Args}, _From, #state{mod=Mod, state=BState}=State) ->
    try erlang:apply(Mod, Cmd, Args ++ [BState]) of
	{Reply, BState2} ->
	    {reply, Reply, State#state{state=BState2}}
    catch Cls:Err ->
	    erlang:display(erlang:get_stacktrace()),
	    {reply, {error, {internal, {Cls, Err}}}, State}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{mod=Mod, state=State}) ->
    Mod:terminate(State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%
%%% Priv
%%%
create_and_gen_location(#backend{ id=B, raw_mountpoint=Prefix }, Entity, Owner, Group) ->
    Entity1 = occi_entity:change_prefix(rm, Prefix, Entity),
    case gen_server:call(B, {create, [Entity1, Owner, Group]}, ?TIMEOUT) of
	{ok, Location, Entity2, Serial} ->
	    Location1 = occi_uri:change_prefix(add, Prefix, Location),
	    Entity3 = occi_entity:change_prefix(add, Prefix, occi_entity:location(Location1, Entity2)),
	    {ok, Entity3, Serial};
	{error, _}=Err -> 
	    Err
    end.


create_with_location(#backend{ id=B, raw_mountpoint=Prefix }, Id, Entity, Owner, Group) ->
    Id1 = occi_uri:change_prefix(rm, Prefix, Id),
    Entity1 = occi_entity:change_prefix(rm, Prefix, Entity),
    case gen_server:call(B, {create, [Id1, Entity1, Owner, Group]}, ?TIMEOUT) of
	{ok, Entity2, Serial} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2), Serial};
	{error, _}=Err -> 
	    Err
    end.
