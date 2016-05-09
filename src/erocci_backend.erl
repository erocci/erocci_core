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
	 create/2,
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

-type backend_errors() :: not_found.
-type errors() :: backend_errors()
		| occi_rendering:errors().

-export_type([t/0,
	      capability/0,
	      errors/0]).

%%%
%%% Callbacks
%%%
-callback init(Opts :: term()) ->
    {ok, Caps :: [capability()], State :: term()} |
    {error, Reason :: term()}.


-callback terminate(State :: term()) -> ok.


-callback model(State :: term()) -> 
    {{ok, occi_extension:t()} | {error, errors()}, NewState :: term()}.


-callback get(Id :: binary(), State :: term()) ->
    {{ok, erocci_node:t()} | {error, errors()}, NewState :: term()}.


-callback create(Id :: binary(), Entity :: occi_entity:t(), State :: term()) ->
    {{ok, occi_entity:t()} | {error, errors()}, NewState :: term()}.


-callback create(Entity :: occi_entity:t(), State :: term()) ->
    {{ok, occi_entity:id(), occi_entity:t()} | {error, errors()}, NewState :: term()}.


-callback update(Actual :: occi_entity:t(), Attributes :: maps:map(), State :: term()) ->
    {{ok, Entity2 :: occi_entity:t()} | {error, errors()}, NewState :: term()}.


-callback action(Invoke :: occi_invoke:t(), Entity :: occi_entity:t(), State :: term()) ->
    {{ok, occi_entity:t()} | {error, errors()}, NewState :: term()}.


-callback delete(Id :: binary(), State :: term()) ->
    {ok | {error, errors()}, NewState :: term()}.


-callback mixin(Mixin :: occi_mixin:t(), Entity :: occi_entity:t(), State :: term()) ->
    {{ok, occi_entity:t()} | {error, errors()}, NewState :: term()}.


-callback unmixin(Mixin :: occi_mixin:t(), Entity :: occi_entity:t(), State :: term()) ->
    {{ok, occi_entity:t()} | {error, errors()}, NewState :: term()}.


-callback collection(Id :: occi_category:id() | binary(),
		     Filter :: erocci_filter:t(),
		     Page :: integer(), Number :: integer() | undefined,
		     State :: term()) ->
    {{ok, [erocci_node:t()]} | {error, errors()}, NewState :: term()}.


%% @doc Creates new backend entry
%% @throw {backend, term()}
%% @end
-spec new({Id :: term(), Mod :: atom(), Opts :: term(), Mountpoint :: binary() | string()}) -> t().
new({Id, Mod, Opts, Mountpoint}) when is_list(Mountpoint) ->
    new({Id, Mod, Opts, list_to_binary(Mountpoint)});

new({Id, Mod, Opts, << $/, _/binary >> = Path}) when is_atom(Mod) ->
    Mountpoint = binary:split(Path, [<<$/>>], [global, trim_all]),
    #backend{ id=Id, handler=Mod, opts=Opts, 
	      raw_mountpoint=Path,
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
-spec get(t(), binary()) -> {ok, erocci_node:t()} | {error, errors()}.
get(#backend{ id=B, raw_mountpoint=Prefix }, Id) ->
    case gen_server:call(B, {get, [occi_uri:rm_prefix(Prefix, Id)]}) of
	{ok, Node} ->
	    {ok, erocci_node:add_prefix(Prefix, Node)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Create a new entity
%% @end
-spec create(t(), occi_entity:t()) -> {ok, erocci_entity:t()} | {error, errors()}.
create(#backend{ id=B, raw_mountpoint=Prefix }, Entity) ->
    Args = case occi_entity:id(Entity) of
	       undefined ->
		   [occi_entity:rm_prefix(Prefix, Entity)];
	       Id ->
		   [occi_uri:rm_prefix(Prefix, Id), occi_entity:rm_prefix(Prefix, Entity)]
	   end,
    case gen_server:call(B, {create, Args}) of
	{ok, Entity2} ->
	    {ok, occi_entity:add_prefix(Entity2, Prefix)};
	{ok, Id2, Entity2} ->
	    {ok, occi_uri:add_prefix(Prefix, Id2), occi_entity:add_prefix(Entity2, Prefix)};
	{error, _}=Err -> 
	    Err
    end.


%% @doc Update an entity
%% @end
-spec update(t(), Entity :: occi_entity:t(), Attributes :: maps:map()) -> ok | {error, errors()}.
update(#backend{ id=B, raw_mountpoint=Prefix }, Entity, Attributes) ->
    Attributes2 = maps:map(fun (_K, V) -> occi_attribute:rm_prefix(Prefix, V) end, Attributes),
    case gen_server:call(B, {update, [occi_entity:rm_prefix(Prefix, Entity), Attributes2]}) of
	{ok, Entity2} ->
	    {ok, occi_entity:add_prefix(Prefix, Entity2)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Invoke an action on an existing entity
%% @end
-spec action(t(), occi_invoke:t(), occi_entity:t()) -> {ok, occi_entity:t()} | {error, errors()}.
action(#backend{ id=B, raw_mountpoint=Prefix }, Invoke, Entity) ->
    case gen_server:call(B, {action, [occi_invoke:rm_prefix(Prefix, Invoke), occi_entity:rm_prefix(Prefix, Entity)]}) of
	{ok, Entity2} ->
	    {ok, occi_entity:add_prefix(Prefix, Entity2)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Delete an entity
%% @end
-spec delete(t(), Id :: binary()) -> ok | {error, errors()}.
delete(#backend{ id=B, raw_mountpoint=Prefix }, Id) when is_binary(Id) ->
    gen_server:call(B, {delete, [occi_uri:rm_prefix(Prefix, Id)]}).


%% @doc Add a mixin to an existing entity
%% @end
-spec mixin(t(), occi_mixin:t(), occi_entity:t()) -> {ok, occi_entity:t()} | {error, errors()}.
mixin(#backend{ id=B, raw_mountpoint=Prefix }, Mixin, Entity) ->
    case gen_server:call(B, {mixin, [Mixin, occi_entity:rm_prefix(Prefix, Entity)]}) of
	{ok, Entity2} ->
	    {ok, occi_entity:add_prefix(Prefix, Entity2)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Remove mixin from existing entity
%% @end
-spec unmixin(t(), occi_mixin:t(), occi_entity:t()) -> {ok, occi_entity:t()} | {error, errors()}.
unmixin(#backend{ id=B, raw_mountpoint=Prefix }, Mixin, Entity) ->
    case gen_server:call(B, {unmixin, [Mixin, occi_entity:rm_prefix(Prefix, Entity)]}) of
	{ok, Entity2} ->
	    {ok, occi_entity:add_prefix(Prefix, Entity2)};
	{error, _}=Err ->
	    Err
    end.


%% @doc Retrieve a list of nodes
%% @end
-spec collection(t(),
		 Id :: occi_category:id() | binary(),
		 Filter :: erocci_filter:t(),
		 Page :: integer(), Number :: integer() | undefined) ->
			{ok, [occi_node:t()]} | {error, errors()}.
collection(#backend{ id=B, raw_mountpoint=Prefix }, Id, Filter, Page, Number) ->
    Id2 = case Id of
	      Path when is_binary(Path) -> occi_uri:rm_prefix(Prefix, Path);
	      CatId when ?is_category_id(CatId) -> CatId
	  end,	      
    case gen_server:call(B, {collection, [Id2, Filter, Page, Number]}) of
	{ok, Nodes} ->
	    {ok, lists:map(fun (N) -> occi_node:add_prefix(N) end, Nodes)};
	{error, _}=Err ->
	    Err
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(t()) -> {ok, term()} | {error, term()} | ignore.
init(#backend{id=Id, handler=Mod}=Backend) ->
    try Mod:init(Backend) of
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
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
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
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
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
%% @spec terminate(Reason, State) -> void()
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
