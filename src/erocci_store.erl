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
-include("erocci_log.hrl").
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

-type error() :: not_found
	       | method_not_allowed
	       | forbidden
	       | conflict
	       | {unauthorized, binary()}.

-type data() :: {occi_utils:mimetype(), iolist()}.

-export_type([error/0]).

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
			  {ok, [occi_category:t()], erocci_node:serial()} | {error, error()}.
capabilities(Creds, Filter) when ?is_creds(Creds), ?is_filter(Filter) ->
    Categories = lists:filter(fun (E) -> erocci_filter:match(E, Filter) end, occi_models:categories()),
    Node = erocci_node:capabilities(Categories),
    auth(read, Creds, Node, fun () -> {ok, Categories, undefined} end).


%% @doc Add a user-defined mixin
%% @end
-spec new_mixin(data(), erocci_creds:t()) -> {ok, occi_mixin:t()} | {error, error()}.
new_mixin({Mimetype, Data}, Creds) when ?is_creds(Creds) ->
    try occi_rendering:parse(Mimetype, Data, occi_mixin) of
	Mixin ->
	    Node = erocci_node:capabilities([Mixin]),
	    Success = fun() ->
			      new_mixin2(Mixin)
		      end,
	    auth(create, Creds, Node, Success)
    catch throw:Err ->
	    {error, Err}
    end.


%% @doc Delete a user-defined mixin
%% @end
-spec delete_mixin(data(), erocci_creds:t()) -> ok | {error, error()}.
delete_mixin({Mimetype, Data}, Creds) when ?is_creds(Creds) ->
    Fun = fun(AST) -> occi_category:id_from_map(AST) end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	Id ->
	    Node = erocci_node:capabilities([Id]),
	    Success = fun () ->
			      delete_mixin2(Id)
		      end,
	    auth(delete, Creds, Node, Success)
    catch throw:Err ->
	    {error, Err}
    end.


%% @doc Retrieve a bounded collection
%% `page' is an indexed integer that refers to a sub-collection of the requested.
%% `number' is an integer of items that SHOULD be contained in the collection. 
%% If `number = undefined', retrieve whole collection.
%% @end
-spec collection(occi_category:t(), erocci_creds:t(), erocci_filter:t(), integer(), integer() | undefined) ->
			{ok, occi_collection:t(), erocci_node:serial()} | {error, error()}.
collection(Category, Creds, Filter, Page, Number) ->
    collection(Category, Creds, read, Filter, Page, Number).


%% @doc Delete all entities from bounded collection
%% @end
-spec delete_all(occi_category:t(), erocci_creds:t()) -> ok | {error, error()}.
delete_all(Category, Creds) ->
    case collection(Category, Creds, delete) of
	{ok, Coll} ->
	    delete_all2(sets:to_list(occi_collection:ids(Coll)), ok);
	{error, _}=Err ->
	    Err
    end.


%% @doc Associate entities to the given mixin
%% @end
-spec append_mixin(occi_category:t(), data(), erocci_creds:t()) -> {ok, occi_collection:t(), erocci_node:serial()} | {error, error()}.
append_mixin(Mixin, {Mimetype, Data}, Creds) ->
    MixinId = occi_collection:id(Mixin),
    Fun = fun (AST) -> occi_collection:from_map(MixinId, AST) end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	Coll ->
	    Ret = apply_collection(Coll, Creds, fun (Backend, Category, Entity) ->
							erocci_backend:mixin(Backend, Category, Entity)
						end),
	    case Ret of
		ok -> collection(Mixin, Creds, read);
		{error, Err} -> {error, Err}
	    end
    catch throw:Err ->
	    {error, Err}
    end.


%% @doc Replace collection of entities associated to mixin
%% @end
-spec set_mixin(occi_category:t(), data(), erocci_creds:t()) -> {ok, occi_collection:t(), erocci_node:serial()} | {error, error()}.
set_mixin(Mixin, {Mimetype, Data}, Creds) ->
    MixinId = occi_collection:id(Mixin),
    Fun = fun (AST) -> occi_collection:from_map(MixinId, AST) end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
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
	    {error, Err}
    end.


%% @doc Disassociate entities from the given mixin
%% @end
-spec remove_mixin(occi_category:t(), data(), erocci_creds:t()) -> ok | {error, error()}.
remove_mixin(Mixin, {Mimetype, Data}, Creds) ->
    MixinId = occi_collection:id(Mixin),
    Fun = fun (AST) -> occi_collection:from_map(MixinId, AST) end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
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
	    {error, Err}
    end.


%% @doc Creates new entity
%% @end
-spec create(occi_category:t() | binary(), data(), erocci_creds:t()) -> 
		    {ok, occi_entity:t()} | {error, error()}.
create(Path, Data, Creds) when is_binary(Path) ->
    create(Path, Data, Creds, fun () -> erocci_backends:by_path(Path) end);

create(Category, Data, Creds) when ?is_category(Category) ->
    Id = occi_category:id(Category),
    create(Category, Data, Creds, fun () -> hd(erocci_backends:by_category_id(Id)) end).


%% @doc Retrieve an entity or unbounded collection
%% @todo Implement unbounded collection (?)
%% @end
-spec get(binary(), erocci_creds:t(), erocci_filter:t(), integer(), integer() | undefined) -> 
		 {ok, occi_entity:t() | occi_collection:t(), erocci_node:serial()} | {error, error()}.
get(Path, Creds, Filter, _Start, _Number) when is_binary(Path),
					       ?is_filter(Filter) ->
    entity(Path, Creds, read).


%% @doc Delete entity
%% @end
-spec delete(binary(), erocci_creds:t()) -> ok | {error, error()}.
delete(Path, Creds) ->
    Success = fun () ->
		      Backend = erocci_backends:by_path(Path),
		      erocci_backend:delete(Backend, Path)
	      end,
    auth(delete, Creds, erocci_node:entity(Path), Success).


%% @doc Execute an action on the given entity or collection
%% @end
-spec action(binary() | occi_category:t(), binary(), data(), erocci_creds:t()) ->
		    {ok, erocci_type:t()} | {error, error()}.
action(Path, ActionTerm, {Mimetype, Data}, Creds) when is_binary(Path),
					  is_binary(ActionTerm),
					  ?is_creds(Creds) ->
    Fun = fun (AST) -> occi_invoke:from_map(AST) end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	Invoke ->
	    case entity(Path, Creds, {action, occi_invoke:id(Invoke)}) of
		{ok, Entity, _Serial} ->
		    action2(Invoke, Entity);
		{error, _}=Err ->
		    Err
	    end
    catch throw:Err ->
	    {error, Err}
    end.


%% @doc Update entity
%% @end
-spec update(binary(), data(), erocci_creds:t()) -> {ok, occi_entity:t()} | {error, error()}.
update(Path, Data, Creds) when is_binary(Path),
			      ?is_creds(Creds) ->
    case entity(Path, Creds, update) of
	{ok, Entity}->
	    update2(Entity, Data);
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
			      Entity = erocci_node:data(Node),
			      case occi_type:type(Entity) of
				  resource ->
				      resource_links(Entity, erocci_node:serial(Node), Creds);
				  link ->
				      {ok, Entity, erocci_node:serial(Node)}
			      end
		      end,
	    auth(Op, Creds, Node, Success);
	{error, _}=Err ->
	    Err
    end.


resource_links(Resource, Serial, Creds) ->
    Links = lists:foldl(fun (LinkId, Acc) ->
				[ entity(LinkId, Creds, read) | Acc ]
			end, [], occi_resource:links(Resource)),
    {ok, occi_resource:links(Links, Resource), Serial}.


new_mixin2(Mixin) ->
    case occi_models:category(occi_category:id(Mixin)) of
	undefined ->
	    Mixin1 = occi_mixin:tag(true, Mixin),
	    case occi_models:add_category(Mixin1) of
		ok -> {ok, Mixin1};
		{error, _}=Err -> Err
	    end;
	Category ->
	    case occi_category:class(Category) =:= mixin 
		andalso occi_mixin:tag(Category) =:= true of
		true ->
		    {ok, Category};
		false ->
		    {error, conflict}
	    end
    end.


delete_mixin2(Id) ->
    case occi_models:category(occi_category:id(Id)) of
	undefined ->
	    {error, not_found};
	Category ->
	    case occi_category:class(Category) =:= mixin 
		andalso occi_mixin:tag(Category) =:= true of
		true ->
		    occi_models:rm_category(Id);
		false ->
		    {error, forbidden}
	    end
    end.
    

action2(Invoke, Entity) ->
    Backend = erocci_backends:by_path(occi_entity:id(Entity)),
    erocci_backend:action(Backend, Invoke, Entity).


update2(Entity, {Mimetype, Data}) ->
    Fun = fun(AST) -> occi_entity:update_from_map(AST, Entity) end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	Attributes ->
	    Backend = erocci_backends:by_path(occi_entity:id(Entity)),
	    erocci_backend:update(Backend, Entity, Attributes)
    catch throw:Err ->
	    {error, Err}
    end.


%% Get full collection
collection(Category, Creds, Op) ->
    collection(Category, Creds, Op, [], 0, undefined).


collection(Category, Creds, Op, Filter, Start, 0) ->
    collection(Category, Creds, Op, Filter, Start, undefined);

collection(Category, Creds, Op, Filter, Start, Number) ->
    Backends = erocci_backends:by_category_id(occi_category:id(Category)),
    Id = occi_category:id(Category),
    collection2(erocci_backend:collection(hd(Backends), Id, Filter, Start, Number),
		Creds, Op, Filter, Start, Number, Backends, 
		occi_collection:new(Id)).


%% @doc Creds=undefined for internal purpose (credentials will be checked later)
%% @todo Manage serial from multiple backends. Actually, serial is the one from latest backend
%% @end
collection2({error, _}=Err, _, _, _, _, _, _, _) ->
    Err;

%%collection2({ok, [], Serial}, _Creds, _Op, _Filter, _Start, _Number, [], Acc) ->
%%    %% No more entity, no more backends
%%    {ok, Acc, Serial};

collection2({ok, [], Serial}, _Creds, _Op, _Filter, _Start, _Number, [ _Backend ], Acc) ->
    %% No more entity, no more backends
    {ok, Acc, Serial};

collection2({ok, [], _Serial}, Creds, Op, Filter, Start, Number, [ _Backend | Backends ], Acc) ->
    %% No more entity in this backend
    Id = occi_collection:id(Acc),
    Start2 = case occi_collection:size(Acc) of
		0 -> Start;  %% still no element, apply shift on next backends
		_ -> 0       %% got first elements, do not shift requests on next backends
	    end,
    collection2(erocci_backend:collection(hd(Backends), Id, Filter, Start2, Number),
		Creds, Op, Filter, Start2, Number, Backends, Acc);

collection2({ok, Nodes, Serial}, Creds, Op, Filter, Start, Number, [ Backend | Backends ], Acc) ->
    case collection_auth(Nodes, Creds, Op, Number, Acc) of
	{error, Err} ->
	    %% Authorization required
	    {error, Err};
	{0, Acc2} ->
	    {ok, Acc2, Serial};
	{undefined, Acc2} ->
	    {ok, Acc2, Serial};
	{Left, Acc2} ->
	    %% Still trying same backend
	    Id = occi_collection:id(Acc),
	    collection2(erocci_backend:collection(Backend, Id, Filter, Start+Number, Left),
			Creds, Op, Filter, 0, Left, [ Backend | Backends ], Acc2)
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
    apply_collection(occi_collection:ids(Collection), Creds, 
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


create(PathOrCategory, {Mimetype, Data}, Creds, BackendFun) ->
    Fun = fun (AST) -> occi_entity:from_map(PathOrCategory, AST) end,
    try	occi_rendering:parse(Mimetype, Data, Fun) of
	Entity -> 
	    create2(occi_type:type(Entity), Entity, Creds, BackendFun)
    catch throw:Err -> {error, Err}
    end.


create2(resource, Resource, Creds, BackendFun) ->
    Success = fun () ->
		      Ret = erocci_backend:create(BackendFun(), canonical_links(Resource), 
						  erocci_creds:user(Creds), erocci_creds:group(Creds)),
		      case Ret of
			  {ok, Resource1} ->
			      create_resource_links(occi_resource:links(Resource1), [], Resource1, Creds);
			  {error, _}=Err ->
			      Err
		      end
	      end,
    auth(create, Creds, erocci_node:entity(Resource), Success);

create2(link, Link, Creds, BackendFun) ->
    Success = fun () ->
		      erocci_backend:create(BackendFun(), Link, 
					    erocci_creds:user(Creds), erocci_creds:group(Creds))
	      end,
    auth(create, Creds, erocci_node:entity(Link), Success).


canonical_links(Resource) ->
    Links2 = lists:foldl(fun (Id, Acc) when is_binary(Id) ->
				 [ Id | Acc ];
			     (Link, Acc) when ?is_link(Link) ->
				 [ occi_link:id(Link) | Acc ]
			 end, [], occi_resource:links(Resource)),
    occi_resource:links(Links2, Resource).


create_resource_links([], Acc, Resource, _Creds) ->
    {ok, occi_resource:links(Acc, Resource)};

create_resource_links([ Link | Links ], Acc, Resource, Creds) ->
    case create_resource_link(Link, Creds) of
	{ok, Link1} ->
	    create_resource_links(Links, [ Link1 | Acc ], Resource, Creds);
	{error, _}=Err ->
	    Err
    end.


create_resource_link(Link, Creds) ->
    BackendFun = case occi_link:location(Link) of
		     undefined ->
			 fun () -> hd(erocci_backends:by_category_id(occi_link:kind(Link))) end;
		     Url ->
			 fun () -> erocci_backends:by_path(Url) end
		 end,
    Success = fun () ->
		      erocci_backend:create(BackendFun(), Link, 
					    erocci_creds:user(Creds), erocci_creds:group(Creds))
	      end,
    auth(create, Creds, erocci_node:entity(Link), Success).


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
