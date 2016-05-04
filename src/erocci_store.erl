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
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(erocci_store).

-include("erocci_types.hrl").
-include_lib("occi/include/occi_types.hrl").

-export([collections/0,
	 capabilities/2,
	 delete_mixin/2,
	 new_mixin/2,
	 collection/5,
	 delete_all/2,
	 append_mixin/3,
	 set_mixin/3,
	 remove_mixin/3,
	 create/3,
	 get/5,
	 delete/2,
	 action/4,
	 update/3]).

%%-record(tag, { id       :: occi_category:id(),
%%	         creds    :: erocci_creds:t() }).

%%-type tag() :: #tag{}.

-type store_error() :: not_found
		     | method_not_allowed
		     | forbidden
		     | conflict
		     | {unauthorized, binary()}.

-type errors() :: store_error() 
		| erocci_backend:errors()
		| occi_rendering:parse_error().

-export_type([errors/0]).

%% @doc Return map of location -> bounded collections
%% @end
-spec collections() -> maps:map().
collections() ->
    lists:foldl(fun (Category, Acc) ->
			case occi_category:class(Category) of
			    action ->
				Acc;
			    _ ->
				Acc#{ occi_category:location(Category) => Category }
			end
		end, #{}, occi_models:categories()).


%% @doc Retrieve capabilities node
%% @end
-spec capabilities(erocci_creds:t(), erocci_filter:t() | undefined) -> 
			  {ok, [occi_category:t()]} | {error, errors()}.
capabilities(Creds, Filter) when ?is_creds(Creds), ?is_filter(Filter) ->
    Categories = lists:filter(fun (E) -> erocci_filter:match(E, Filter) end, occi_models:categories()),
    Node = occi_node:capabilities(Categories),
    auth(read, Creds, Node, fun () -> {ok, Categories} end).


%% @doc Add a user-defined mixin
%% @end
-spec new_mixin(maps:map(), erocci_creds:t()) -> {ok, occi_mixin:t()} | {error, errors()}.
new_mixin(Map, Creds) when is_map(Map), ?is_creds(Creds) ->
    try occi_mixin:from_map(Map) of
	Mixin ->
	    Node = erocci_node:capabilities([Mixin]),
	    Success = fun() ->
			      new_mixin2(Mixin)
		      end,
	    auth(create, Creds, Node, Success)
    catch throw:Err ->
	    Err
    end.


%% @doc Delete a user-defined mixin
%% @end
-spec delete_mixin(maps:map(), erocci_creds:t()) -> ok | {error, errors()}.
delete_mixin(Map, Creds) when is_map(Map), ?is_creds(Creds) ->
    try occi_category:id_from_map(Map) of
	Id ->
	    Node = erocci_node:capabilities([Id]),
	    Success = fun () ->
			      delete_mixin2(Id)
		      end,
	    auth(delete, Creds, Node, Success)
    catch throw:Err ->
	    Err
    end.


%% @doc Retrieve a bounded collection
%% `page' is an indexed integer that refers to a sub-collection of the requested.
%% `number' is an integer of items that SHOULD be contained in the collection. 
%% If `number = undefined', retrieve whole collection.
%% @end
-spec collection(occi_category:t(), erocci_creds:t(), erocci_filter:t(), integer(), integer() | undefined) ->
			{ok, occi_collection:t()} | {error, errors()}.
collection(Category, Creds, Filter, Page, Number) ->
    collection(Category, Creds, read, Filter, Page, Number).	


%% @doc Delete all entities from bounded collection
%% @end
-spec delete_all(occi_category:t(), erocci_creds:t()) -> ok | {error, errors()}.
delete_all(Category, Creds) ->
    case collection(Category, Creds, delete) of
	{ok, Coll} ->
	    delete_all2(sets:to_list(occi_collection:ids(Coll)), ok);
	{error, _}=Err ->
	    Err
    end.


%% @doc Associate entities to the given mixin
%% @end
-spec append_mixin(occi_category:t(), maps:map(), erocci_creds:t()) -> {ok, occi_collection:t()} | {error, errors()}.
append_mixin(Mixin, Obj, Creds) ->
    MixinId = occi_collection:id(Mixin),
    try occi_collection:from_map(MixinId, Obj) of
	Coll ->
	    Ret = apply_collection(Coll, Creds, fun (Backend, Category, Entity) ->
							erocci_backend:mixin(Backend, Category, Entity)
						end),
	    case Ret of
		ok -> collection(Mixin, Creds, read);
		{error, _}=Err -> Err
	    end
    catch throw:Err ->
	    Err
    end.


%% @doc Replace collection of entities associated to mixin
%% @end
-spec set_mixin(occi_category:t(), maps:map(), erocci_creds:t()) -> {ok, occi_collection:t()} | {error, errors()}.
set_mixin(Mixin, Obj, Creds) ->
    MixinId = occi_collection:id(Mixin),
    try occi_collection:from_map(MixinId, Obj) of
	New ->
	    Actual = collection(Mixin, Creds, update),
	    ToDelete = sets:subtract(occi_collection:ids(Actual), occi_collection:ids(New)),
	    ToAdd = sets:subtract(occi_collection:ids(New), occi_collection:ids(Actual)),
	    Ret = apply_collection(occi_collection:new(MixinId, ToDelete),
				   Creds, fun (Backend, Category, Entity) ->
						  erocci_backend:unmixin(Backend, Category, Entity)
					  end),
	    Ret2 = apply_collection(occi_collection:new(MixinId, ToAdd),
				    Creds, fun (Backend, Category, Entity) ->
						   erocci_backend:mixin(Backend, Category, Entity)
					   end, Ret),
	    case Ret2 of
		ok -> collection(Mixin, Creds, read);
		{error, _}=Err -> Err
	    end
    catch throw:Err ->
	    Err
    end.


%% @doc Disassociate entities from the given mixin
%% @end
-spec remove_mixin(occi_category:t(), maps:map(), erocci_creds:t()) -> ok | {error, errors()}.
remove_mixin(Mixin, Obj, Creds) ->
    MixinId = occi_collection:id(Mixin),
    try occi_collection:from_map(MixinId, Obj) of
	Coll ->
	    Coll2 = case occi_collection:size(Coll) of
			0 -> collection(Mixin, Creds, update);
			_ -> Coll
		    end,
	    Ret = apply_collection(Coll2, Creds, fun (Backend, Category, Entity) ->
							 erocci_backend:unmixin(Backend, Category, Entity)
						 end),
	    case Ret of
		ok -> collection(Mixin, Creds, read);
		{error, _}=Err -> Err
	    end
    catch throw:Err ->
	    Err
    end.


%% @doc Creates new entity
%% @end
-spec create(occi_category:t() | binary(), maps:map(), erocci_creds:t()) -> 
		    {ok, occi_entity:t()} | {error, errors()}.
create(Path, Obj, Creds) when is_binary(Path) ->
    create(Path, Obj, Creds, fun () -> erocci_backends:by_path(Path) end);

create(Category, Obj, Creds) when ?is_category(Category) ->
    Id = occi_category:id(Category),
    create(Category, Obj, Creds, fun () -> erocci_backends:by_category_id(Id) end).


%% @doc Retrieve an entity or unbounded collection
%% @todo Implement unbounded collection (?)
%% @end
-spec get(binary(), erocci_creds:t(), erocci_filter:t(), integer(), integer() | undefined) -> 
		 {ok, occi_entity:t() | occi_collection:t()} | {error, errors()}.
get(Path, Creds, Filter, Page, Number) when is_binary(Path),
					    ?is_filter(Filter),
					    is_integer(Page),
					    Number =:= undefined orelse is_integer(Number) ->
    entity(Path, Creds, read).


%% @doc Delete entity
%% @end
-spec delete(binary(), erocci_creds:t()) -> ok | {error, errors()}.
delete(Path, Creds) ->
    Success = fun () ->
		      Backend = erocci_backends:by_path(Path),
		      erocci_backend:delete(Backend, Path)
	      end,
    auth(delete, Creds, erocci_node:entity(Path), Success).


%% @doc Execute an action on the given entity or collection
%% @end
-spec action(binary() | occi_category:t(), binary(), maps:map(), erocci_creds:t()) ->
		    {ok, erocci_type:t()} | {error, errors()}.
action(Path, ActionTerm, Obj, Creds) when is_binary(Path),
					  is_binary(ActionTerm),
					  is_map(Obj),
					  ?is_creds(Creds) ->
    try occi_invoke:from_map(ActionTerm, Obj) of
	Invoke ->
	    case entity(Path, Creds, {action, occi_invoke:id(Invoke)}) of
		{ok, Entity} ->
		    action2(Invoke, Entity);
		{error, _}=Err ->
		    Err
	    end
    catch throw:Err ->
	    Err
    end.


%% @doc Update entity
%% @end
-spec update(binary(), maps:map(), erocci_creds:t()) -> {ok, occi_entity:t()} | {error, errors()}.
update(Path, Obj, Creds) when is_binary(Path),
			      is_map(Obj),
			      ?is_creds(Creds) ->
    case entity(Path, Creds, update) of
	{ok, Entity}->
	    update2(Entity, Obj);
	{error, _}=Err ->
	    Err
    end.


%%%
%%% Priv
%%%
entity(Path, Creds, Op) ->
    Backend = erocci_backends:by_path(Path),
    case erocci_backend:get(Backend, Path) of
	{ok, Node} ->
	    Success = fun () ->
			      {ok, erocci_node:data(Node)}
		      end,
	    auth(Op, Creds, Node, Success);
	{error, _}=Err ->
	    Err
    end.


usermixin(Id) ->
    occi_category:category(Id).


new_mixin2(Mixin) ->
    case usermixin(occi_category:id(Mixin)) of
	undefined ->
	    case occi_models:add_category(Mixin) of
		ok -> {ok, Mixin};
		{error, _}=Err -> Err
	    end;
	Mixin2 ->
	    %% Ignore duplicate mixins
	    {ok, Mixin2}
    end.


delete_mixin2(Id) ->
    case usermixin(Id) of
	undefined ->
	    {error, not_found};
	_Mixin ->
	    occi_models:rm_category(Id)
    end.
    

action2(Invoke, Entity) ->
    Backend = erocci_backends:by_path(occi_entity:id(Entity)),
    erocci_backend:action(Backend, Invoke, Entity).


update2(Entity, Obj) ->
    try occi_entity:update_from_map(Obj, Entity) of
	_Entity2 ->
	    Backend = erocci_backends:by_path(occi_entity:id(Entity)),
	    erocci_backend:update(Backend, Entity, maps:get(attributes, Obj), #{})
    catch throw:Err ->
	    Err
    end.


%% Get full collection
collection(Category, Creds, Op) ->
    collection(Category, Creds, Op, [], 0, undefined).


collection(Category, Creds, Op, Filter, Page, Number) ->
    Backends = erocci_backends:by_category_id(occi_category:id(Category)),
    Id = occi_category:id(Category),
    collection2(erocci_backend:collection(hd(Backends), Id, Filter, Page, Number),
		Creds, Filter, Page, Number, Backends, 
		occi_collection:new(Id), Op).


%% Creds=undefined for internal purpose (credentials will be checked later)
collection2([], _Creds, _Op, _Filter, _Page, _Number, [], Acc) ->
    %% No more entity, no more backends
    {ok, Acc};

collection2([], Creds, Op, Filter, Page, Number, [ _Backend | Backends ], Acc) ->
    %% No more entity in this backend
    Id = occi_collection:id(Acc),
    Page2 = case occi_category:size(Acc) of
		0 -> Page;   %% still no element, apply shift on next backends
		_ -> 0       %% got first elements, do not shift requests on next backends
	    end,
    collection2(erocci_backend:collection(hd(Backends), Id, Filter, Page2, Number),
		Creds, Op, Filter, Page2, Number, Backends, Acc);

collection2(Nodes, Creds, Op, Filter, Page, Number, [ Backend | Backends ], Acc) ->
    case collection_auth(Nodes, Creds, Op, Number, Acc) of
	{error, _}=Err ->
	    %% Authorization required
	    Err;
	{0, Acc2} ->
	    {ok, Acc2};
	{Left, Acc2} ->
	    %% Still trying same backend
	    Id = occi_collection:id(Acc),
	    collection2(erocci_backend:collection(Backend, Id, Filter, Page+1, Left),
			Creds, Op, Filter, 0, Left, Backends, Acc2)
    end.


collection_auth(_, _, _, _, {error, {unauthorized, _}}=Err) ->
    Err;

collection_auth([], _Creds, _Op, Left, Acc) ->
    {Left, Acc};

collection_auth(_Nodes, _Creds, _Op, 0, Acc) ->
    {0, Acc};

collection_auth([ Node | Nodes ], Creds, Op, Left, Acc) ->
    Left2 = if undefined =:= Left -> undefined; true -> Left-1 end,
    Success = fun () -> 
		      Entity = erocci_node:data(Node),
		      collection_auth(Nodes, Creds, Op, Left2, occi_collection:append([Entity], Acc)) 
	      end,
    Fail = fun ({error, forbidden}) -> 
		   collection_auth(Nodes, Creds, Op, Left, Acc);
	       ({error, {unauthorized, _}}=Err) ->
		   Err
	   end,
    auth(Op, Creds, Node, Success, Fail).


delete_all2([], ok) ->
    ok;

delete_all2(_, {error, _}=Err) ->
    Err;

delete_all2([ Entity | Entities ], ok) ->
    Id = occi_entity:id(Entity),
    Backend = erocci_backends:by_path(Id),
    delete_all2(Entities, erocci_backend:delete(Backend, Id)).


apply_collection(Collection, Creds, Fun) ->
    apply_collection(Collection, Creds, Fun, ok).


apply_collection(Collection, Creds, Fun, Acc) ->
    apply_collection(sets:to_list(occi_collection:ids(Collection)), Creds, 
		     occi_collection:id(Collection), Fun, Acc).


apply_collection(_, _Creds, _MixinId, _Fun, {error, _}=Err) ->
    Err;

apply_collection([], _Creds, _MixinId, _Fun, Acc) ->
    Acc;

apply_collection([ Id | Tail ], Creds, MixinId, Fun, ok) ->
    Backend = erocci_backends:by_path(Id),
    case erocci_backend:get(Backend, Id) of
	{ok, Entity} when ?is_entity(Entity) ->
	    Node = erocci_node:entity(Entity),
	    Success = fun() ->
			      Ret = Fun(Backend, MixinId, Entity),
			      apply_collection(Tail, Creds, MixinId, Fun, Ret)
		      end,
	    auth(update, Creds, Node, Success);
	{ok, _} ->
	    apply_collection(Tail, Creds, MixinId, Fun, ok);
	{error, _}=Err ->
	    Err
    end.


create(PathOrCategory, Obj, Creds, BackendFun) ->
    try	occi_entity:from_map(PathOrCategory, Obj) of
	Entity -> create2(occi_type:type(Entity), Entity, Creds, BackendFun)
    catch throw:Err -> Err
    end.


create2(resource, Resource, Creds, BackendFun) ->
    Success = fun () ->
		      erocci_backend:create(BackendFun(), Resource),
		      create_resource_links(occi_resource:links(Resource), Creds, {ok, Resource})
	      end,
    auth(create, Creds, erocci_node:entity(Resource), Success);

create2(link, Link, Creds, BackendFun) ->
    Success = fun () ->
		      erocci_backend:create(BackendFun(), Link)
	      end,
    auth(create, Creds, erocci_node:entity(Link), Success).


create_resource_links(_, _Creds, {error, _}=Err) ->
    Err;

create_resource_links([], _Creds, {ok, _}=Ret) ->
    Ret;

create_resource_links([ Link | Links ], Creds, _Acc) ->
    BackendFun = case occi_link:url(Link) of
		     undefined ->
			 fun () -> erocci_backends:by_category_id(occi_link:kind(Link)) end;
		     Url ->
			 fun () -> erocci_backends:by_path(Url) end
		 end,			 
    create_resource_links(Links, Creds, create2(link, Link, Creds, BackendFun)).


%%auth(Op, Creds, Node) ->
%%    auth(Op, Creds, Node, 
%%	 fun () -> {ok, Node} end,
%%	 fun (Err) -> Err end).


auth(Op, Creds, Node, Success) ->
    auth(Op, Creds, Node, Success, fun (Err) -> Err end).


auth(Op, Creds, Node, Success, Fail) ->
    case auth2(erocci_acls:check(Op, Node, Creds), Creds) of
	ok ->
	    Success();
	{error, _}=Err ->
	    Fail(Err)
    end.


auth2(allow, _Creds) ->
    ok;

auth2(deny, Creds) ->
    case erocci_creds:is_authenticated(Creds) of
	true -> {error, forbidden};
	false -> {error, {unauthorized, erocci_creds:challenge(Creds)}}
    end.
