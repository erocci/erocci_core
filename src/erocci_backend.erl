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
	 start_link/1]).

%% Callbacks wrappers
-export([model/1,
	 get/2,
	 create/4,
	 update/3,
	 action/3,
	 delete/2,
	 mixin/3,
	 unmixin/3,
	 collection/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type capability() :: undefined.

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

-type backend_error() :: not_found.
-type error() :: backend_error()
	       | occi_rendering:error().

-export_type([t/0,
	      capability/0,
	      error/0]).

%%%
%%% Callbacks
%%%
-callback init(Opts :: term()) ->
    {ok, Caps :: [capability()], State :: term()} |
    {error, Reason :: term()}.


-callback terminate(State :: term()) -> ok.


-callback model(State :: term()) -> 
    {{ok, occi_extension:t()}
     | {error, error()}, NewState :: term()}.


-callback get(Id :: binary(), State :: term()) ->
    {{ok, occi_collection:t() | occi_entity:t(), erocci_creds:user(), erocci_creds:group(), erocci_node:serial()} 
     | {error, error()}, NewState :: term()}.


-callback create(Id :: binary(), Entity :: occi_entity:t(), Owner :: erocci_creds:user(), Group :: erocci_creds:group(), State :: term()) ->
    {{ok, occi_entity:t()}
     | {error, error()}, NewState :: term()}.


-callback create(Entity :: occi_entity:t(), Owner :: erocci_creds:user(), Group :: erocci_creds:group(), State :: term()) ->
    {{ok, occi_entity:id(), occi_entity:t()} 
     | {error, error()}, NewState :: term()}.


-callback update(Actual :: occi_entity:t(), Attributes :: maps:map(), State :: term()) ->
    {{ok, Entity2 :: occi_entity:t()}
     | {error, error()}, NewState :: term()}.


-callback action(Invoke :: occi_invoke:t(), Entity :: occi_entity:t(), State :: term()) ->
    {{ok, occi_entity:t()}
     | {error, error()}, NewState :: term()}.


-callback delete(Id :: binary(), State :: term()) ->
    {ok
     | {error, error()}, NewState :: term()}.


-callback mixin(Mixin :: occi_mixin:t(), Entity :: occi_entity:t(), State :: term()) ->
    {{ok, occi_entity:t()}
     | {error, error()}, NewState :: term()}.


-callback unmixin(Mixin :: occi_mixin:t(), Entity :: occi_entity:t(), State :: term()) ->
    {{ok, occi_entity:t()}
     | {error, error()}, NewState :: term()}.


-callback collection(Id :: occi_category:id() | binary(),
		     Filter :: erocci_filter:t(),
		     Start :: integer(), Number :: integer() | undefined,
		     State :: term()) ->
    {{ok, [{occi_entity:t(), erocci_creds:user(), erocci_creds:group()}], erocci_node:serial()}
     | {error, error()}, NewState :: term()}.


%% @doc Creates new backend entry
%% @throw {backend, term()}
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


%% @doc Start backend
%% @end
-spec start_link(t()) -> {ok, pid()} | ignore | {error, term()}.
start_link(#backend{id=Id, handler=Mod}=Backend) ->
    ?info("Starting storage backend ~p (~s)", [Id, Mod]),
    gen_server:start_link({local, Id}, ?MODULE, Backend, []).


%%% 
%%% Callback wrappers
%%%

%% @doc Get backend model
%% @end
-spec model(t()) -> occi_extension:t().
model(#backend{ id=B }) ->
    case gen_server:call(B, {model, []}) of
	{ok, Ext} ->
	    Ext;
	{error, Err} ->
	    throw(Err)
    end.


%% @doc Lookup for a node at Path
%% @end
-spec get(t(), binary()) -> {ok, erocci_node:t()} | {error, error()}.
get(#backend{ id=B, raw_mountpoint=Prefix }, Id) ->
    case gen_server:call(B, {get, [occi_uri:change_prefix(rm, Prefix, Id)]}) of
	{ok, Entity, Owner, Group, Serial} ->
	    {ok, erocci_node:entity(Entity, Owner, Group, Serial)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Create a new entity
%% @end
-spec create(t(), occi_entity:t(), erocci_creds:user(), erocci_creds:group()) -> {ok, erocci_entity:t()} | {error, error()}.
create(#backend{ id=B, raw_mountpoint=Prefix }, Entity, Owner, Group) ->
    Args = case occi_entity:id(Entity) of
	       undefined ->
		   [occi_entity:change_prefix(rm, Prefix, Entity), Owner, Group];
	       Id ->
		   [occi_uri:change_prefix(rm, Prefix, Id), occi_entity:change_prefix(rm, Prefix, Entity), Owner, Group]
	   end,
    case gen_server:call(B, {create, Args}) of
	{ok, Entity2} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2)};
	{ok, Id2, Entity2} ->
	    Id3 = occi_uri:change_prefix(add, Prefix, Id2),
	    {ok, occi_entity:change_prefix(add, Prefix, occi_entity:id(Id3, Entity2))};
	{error, _}=Err -> 
	    Err
    end.


%% @doc Update an entity
%% @end
-spec update(t(), Entity :: occi_entity:t(), Attributes :: maps:map()) -> ok | {error, error()}.
update(#backend{ id=B, raw_mountpoint=Prefix }, Entity, Attributes) ->
    case gen_server:call(B, {update, [occi_entity:change_prefix(rm, Prefix, Entity), Attributes]}) of
	{ok, Entity2} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Invoke an action on an existing entity
%% @end
-spec action(t(), occi_invoke:t(), occi_entity:t()) -> {ok, occi_entity:t()} | {error, error()}.
action(#backend{ id=B, raw_mountpoint=Prefix }, Invoke, Entity) ->
    case gen_server:call(B, {action, [Invoke, occi_entity:change_prefix(rm, Prefix, Entity)]}) of
	{ok, Entity2} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Delete an entity
%% @end
-spec delete(t(), Id :: binary()) -> ok | {error, error()}.
delete(#backend{ id=B, raw_mountpoint=Prefix }, Id) when is_binary(Id) ->
    gen_server:call(B, {delete, [occi_uri:change_prefix(rm, Prefix, Id)]}).


%% @doc Add a mixin to an existing entity
%% @end
-spec mixin(t(), occi_mixin:t(), occi_entity:t()) -> {ok, occi_entity:t()} | {error, error()}.
mixin(#backend{ id=B, raw_mountpoint=Prefix }, Mixin, Entity) ->
    case gen_server:call(B, {mixin, [Mixin, occi_entity:change_prefix(rm, Prefix, Entity)]}) of
	{ok, Entity2} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Remove mixin from existing entity
%% @end
-spec unmixin(t(), occi_mixin:t(), occi_entity:t()) -> {ok, occi_entity:t()} | {error, error()}.
unmixin(#backend{ id=B, raw_mountpoint=Prefix }, Mixin, Entity) ->
    case gen_server:call(B, {unmixin, [Mixin, occi_entity:change_prefix(rm, Prefix, Entity)]}) of
	{ok, Entity2} ->
	    {ok, occi_entity:change_prefix(add, Prefix, Entity2)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Retrieve a list of entities
%% @end
-spec collection(t(),
		 Id :: occi_category:id() | binary(),
		 Filter :: erocci_filter:t(),
		 Start :: integer(), Number :: integer() | undefined) ->
			{ok, [erocci_node:t()]} | {error, error()}.
collection(#backend{ id=B, raw_mountpoint=Prefix }, Id, Filter, Start, Number) ->
    Id2 = case Id of
	      Path when is_binary(Path) -> occi_uri:change_prefix(rm, Prefix, Path);
	      CatId when ?is_category_id(CatId) -> CatId
	  end,	      
    case gen_server:call(B, {collection, [Id2, Filter, Start, Number]}) of
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
    {Reply, BState2} = erlang:apply(Mod, Cmd, Args ++ [BState]),
    {reply, Reply, State#state{mod=Mod, state=BState2}}.


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
