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
	 append_mixin/4,
	 set_mixin/4,
	 remove_mixin/4,
	 create/4,
	 get/5,
	 delete/2,
	 action/4,
	 update/3]).

-type error() :: not_found
	       | {not_found, occi_uri:url()}
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
-spec new_mixin(data(), erocci_creds:t()) -> {ok, occi_mixin:t(), undefined} | {error, error()}.
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
    Fun = fun(AST) -> occi_mixin:from_map(AST) end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	Mixin ->
	    Node = erocci_node:capabilities([Mixin]),
	    Success = fun () ->
			      delete_mixin2(Mixin)
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
	{ok, Coll, _} ->
	    delete_all2(occi_collection:locations(Coll), ok);
	{error, _}=Err ->
	    Err
    end.


%% @doc Associate entities to the given mixin
%% @end
-spec append_mixin(occi_category:t(), data(), occi_uri:url(), erocci_creds:t()) -> 
			  {ok, occi_collection:t(), erocci_node:serial()} | {error, error()}.
append_mixin(Mixin, {Mimetype, Data}, Endpoint, Creds) ->
    MixinId = occi_category:id(Mixin),
    Fun = fun (AST) -> 
		  Coll = occi_collection:from_map(MixinId, AST),
		  occi_collection:endpoint(Endpoint, Coll)
	  end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	Coll ->
	    Ret = apply_collection(occi_collection:locations(Coll), Creds,
				   fun (Backend, Entity, Acc) ->
					   case erocci_backend:mixin(Backend, Entity, Mixin, #{}) of
					       {ok, Entity2, _Serial} ->
						   {ok, occi_collection:append(Entity2, Acc)};
					       {error, _}=Err ->
						   Err
					   end
				   end, Coll),
	    case Ret of
		{ok, Coll2} -> {ok, Coll2, undefined};
		{error, Err} -> {error, Err}
	    end
    catch throw:Err ->
	    {error, Err}
    end.


%% @doc Replace collection of entities associated to mixin
%% @end
-spec set_mixin(occi_category:t(), data(), occi_uri:url(), erocci_creds:t()) -> 
		       {ok, occi_collection:t(), erocci_node:serial()} | {error, error()}.
set_mixin(Mixin, {Mimetype, Data}, Endpoint, Creds) ->
    MixinId = occi_mixin:id(Mixin),
    Fun = fun (AST) -> 
		  Coll = occi_collection:from_map(MixinId, AST),
		  occi_collection:endpoint(Endpoint, Coll)		      
	  end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	New ->
	    case collection(Mixin, Creds, update) of
		{ok, Actual, _} ->
		    ToDelete = ordsets:subtract(occi_collection:locations(Actual), occi_collection:locations(New)),
		    ToAdd = ordsets:subtract(occi_collection:locations(New), occi_collection:locations(Actual)),
		    Ret =  apply_collection(ToDelete, Creds, 
					    fun (Backend, Entity, Acc) ->
						    case erocci_backend:unmixin(Backend, Entity, MixinId) of
							{ok, Entity2, _} ->
							    {ok, occi_collection:delete(Entity2, Acc)};
							{error, _}=Err ->
							    Err
						    end
					    end, Actual),
		    case Ret of
			{ok, Coll2} ->
			    Ret2 = apply_collection(ToAdd, Creds,
						    fun (Backend, Entity, Acc) ->
							    case erocci_backend:mixin(Backend, Entity, Mixin, #{}) of
								{ok, Entity2, _} ->
								    {ok, occi_collection:append(Entity2, Acc)};
								{error, _}=Err ->
								    Err
							    end
						    end, Coll2),
			    case Ret2 of
				{ok, Coll3} -> {ok, Coll3, undefined};
				{error, _}=Err -> Err
			    end;
			{error, _}=Err ->
			    Err
		    end;
		{error, _}=Err -> Err
	    end
    catch throw:Err ->
	    {error, Err}
    end.


%% @doc Disassociate entities from the given mixin
%% @end
-spec remove_mixin(occi_category:t(), data(), occi_uri:url(), erocci_creds:t()) -> ok | {error, error()}.
remove_mixin(Mixin, {Mimetype, Data}, Endpoint, Creds) ->
    MixinId = occi_category:id(Mixin),
    Fun = fun (AST) -> 
		  Coll = occi_collection:from_map(MixinId, AST),
		  occi_collection:endpoint(Endpoint, Coll)
	  end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	Coll ->
	    Coll2 = case occi_collection:size(Coll) of
			0 -> 
			    {ok, Coll0, _} = collection(Mixin, Creds, update),
			    Coll0;
			_ -> 
			    Coll
		    end,
	    Ret = apply_collection(occi_collection:locations(Coll2), Creds,
				   fun (Backend, Entity, Acc) ->
					   case erocci_backend:unmixin(Backend, Entity, Mixin) of
					       {ok, Entity2, _} ->
						   {ok, occi_collection:delete(Entity2, Acc)};
					       {error, _}=Err ->
						   Err
					   end
				   end, Coll2),
	    case Ret of
		{ok, Coll3} -> {ok, Coll3, undefined};
		{error, _}=Err -> Err
	    end
    catch throw:Err ->
	    {error, Err}
    end.


%% @doc Creates new entity
%% @end
-spec create(occi_category:t() | binary(), Data :: data(), Endpoint :: binary(), Creds :: erocci_creds:t()) -> 
		    {ok, occi_entity:t()} | {error, error()}.
create(Path, Data, Endpoint, Creds) when is_binary(Path) ->
    create(Path, Data, Endpoint, Creds, fun () -> erocci_backends:by_path(Path) end);

create(Category, Data, Endpoint, Creds) when ?is_category(Category) ->
    Id = occi_category:id(Category),
    create(Category, Data, Endpoint, Creds, fun () -> hd(erocci_backends:by_category_id(Id)) end).


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
		    {ok, erocci_type:t(), undefined} | {error, error()}.
action(Path, ActionTerm, {Mimetype, Data}, Creds) when is_binary(Path),
					  is_binary(ActionTerm),
					  ?is_creds(Creds) ->
    Fun = fun (AST) -> occi_invoke:from_map(AST) end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	Invoke ->
	    case entity(Path, Creds, {action, occi_invoke:id(Invoke)}) of
		{ok, Entity, _Serial} ->
		    action2(Invoke, Entity, Creds);
		{error, _}=Err ->
		    Err
	    end
    catch throw:Err ->
	    {error, Err}
    end.


%% @doc Update entity
%% @end
-spec update(binary(), Data :: data(), erocci_creds:t()) -> 
		    {ok, occi_entity:t()} | {error, error()}.
update(Path, Data, Creds) when is_binary(Path),
			      ?is_creds(Creds) ->
    case entity(Path, Creds, update) of
	{ok, Entity, _}->
	    update2(Entity, Data, Creds);
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
			      {ok, fetch_links(Entity, Creds), erocci_node:serial(Node)}
		      end,
	    auth(Op, Creds, Node, Success);
	{error, not_found} ->
	    {error, {not_found, Path}};
	{error, _}=Err ->
	    Err
    end.


fetch_links(Entity, Creds) ->
    case occi_type:type(Entity) of
	resource ->
	    resource_links(occi_resource:links(Entity), [], Entity, Creds);
	link ->
	    Entity
    end.


resource_links([], Acc, Resource, _Creds) ->
    occi_resource:links(Acc, Resource);

resource_links([ LinkId | Links ], Acc, Resource, Creds) ->
    case entity(LinkId, Creds, read) of
	{ok, Link, _} ->
	    resource_links(Links, [ Link | Acc ], Resource, Creds);
	{error, not_found} ->
	    %% ignore
	    resource_links(Links, Acc, Resource, Creds);
	{error, _}=Err ->
	    Err
    end.


new_mixin2(Mixin) ->
    case occi_models:category(occi_category:id(Mixin)) of
	undefined ->
	    Mixin1 = occi_mixin:tag(true, Mixin),
	    case occi_models:add_category(Mixin1) of
		{error, _}=Err -> Err;
		Mixin2 -> {ok, Mixin2, undefined}
	    end;
	Category ->
	    case occi_category:class(Category) =:= mixin 
		andalso occi_mixin:tag(Category) =:= true of
		true ->
		    {ok, Category, undefined};
		false ->
		    {error, conflict}
	    end
    end.


delete_mixin2(Mixin) ->
    Id = occi_mixin:id(Mixin),
    case occi_models:category(Id) of
	undefined ->
	    {error, {not_found, Id}};
	Category ->
	    case occi_category:class(Category) =:= mixin 
		andalso occi_mixin:tag(Category) =:= true of
		true ->
		    occi_models:rm_category(Id);
		false ->
		    {error, forbidden}
	    end
    end.
    

action2(Invoke, Entity, Creds) ->
    Backend = erocci_backends:by_path(occi_entity:location(Entity)),
    case erocci_backend:action(Backend, Invoke, Entity) of
	{ok, Entity2, Serial} ->
	    {ok, fetch_links(Entity2, Creds), Serial};
	{error, _}=Err ->
	    Err
    end.


update2(Entity, {Mimetype, Data}, Creds) ->
    Fun = fun(AST) -> occi_entity:update_from_map(AST, Entity) end,
    try occi_rendering:parse(Mimetype, Data, Fun) of
	Attributes ->
	    Backend = erocci_backends:by_path(occi_entity:location(Entity)),
	    case erocci_backend:update(Backend, occi_entity:location(Entity), Attributes) of
		{ok, Entity2, Serial} ->
		    {ok, fetch_links(Entity2, Creds), Serial};
		{error, _}=Err ->
		    Err
	    end
    catch throw:Err ->
	    {error, Err}
    end.


%% Get full collection
collection(Category, Creds, Op) ->
    collection(Category, Creds, Op, [], 0, undefined).


collection(Category, Creds, Op, Filter, Start, 0) ->
    collection(Category, Creds, Op, Filter, Start, undefined);

collection(Category, Creds, Op, Filter, Start, Number) ->
    Backends = case occi_category:class(Category) =:= mixin andalso occi_mixin:tag(Category) of
		   true ->
		       erocci_backends:all();
		   false ->
		       erocci_backends:by_category_id(occi_category:id(Category))
	       end,
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

collection2({ok, Locations, Serial}, Creds, Op, Filter, Start, Number, [ Backend | Backends ], Acc) ->
    case collection_entities(Locations, Creds, Op, Number, Acc) of
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


collection_entities(_, _, _, _, {error, _}=Err) ->
    Err;

collection_entities([], _Creds, _Op, Left, Acc) ->
    {Left, Acc};

collection_entities(_Locations, _Creds, _Op, 0, Acc) ->
    {0, Acc};

collection_entities([ Location | Locations ], Creds, Op, Left, Acc) ->
    case entity(Location, Creds, read) of
	{ok, Entity, _Serial} ->
	    Left2 = if undefined =:= Left -> undefined; true -> Left-1 end,
	    collection_entities(Locations, Creds, Op, Left2, occi_collection:append([Entity], Acc));
	{error, forbidden} ->
	    collection_entities(Locations, Creds, Op, Left, Acc);
	{error, {unauthorized, _}}=Err ->
	    Err;
	{error, not_found} ->
	    collection_entities(Locations, Creds, Op, Left, Acc);
	{error, _}=Err ->
	    Err
    end.


delete_all2([], ok) ->
    ok;

delete_all2(_, {error, _}=Err) ->
    Err;

delete_all2([ Entity | Entities ], ok) ->
    Location = occi_entity:location(Entity),
    Backend = erocci_backends:by_path(Location),
    delete_all2(Entities, erocci_backend:delete(Backend, Location)).


-spec apply_collection([occi_uri:url()], erocci_creds:t(), fun(), occi_collection:t()) -> 
			      {ok, occi_collection:t()} | {error, term()}.
apply_collection(_, _Creds, _Fun, {error, _}=Err) ->
    Err;

apply_collection([], _Creds, _Fun, Acc) ->
    {ok, Acc};

apply_collection([ Location | Tail ], Creds, Fun, Acc) ->
    Backend = erocci_backends:by_path(Location),
    case occi_collection:entity(Location, Acc) of
	undefined -> 
	    case entity(Location, Creds, update) of
		{ok, Entity, _} ->
		    apply_collection_entity(Backend, Entity, Tail, Creds, Fun, Acc);
		{error, _}=Err ->
		    Err
	    end;
	Entity ->
	    apply_collection_entity(Backend, Entity, Tail, Creds, Fun, Acc)
    end.


apply_collection_entity(Backend, Entity, Locations, Creds, Fun, Acc) ->
    Node = erocci_node:entity(Entity),
    Success = fun() ->
		      case Fun(Backend, Entity, Acc) of
			  {ok, Acc1} ->
			      apply_collection(Locations, Creds, Fun, Acc1);
			  {error, _}=Err ->
			      Err
		      end
	      end,
    auth(update, Creds, Node, Success).


create(PathOrCategory, {Mimetype, Data}, Endpoint, Creds, BackendFun) ->
    Fun = fun (AST) -> occi_entity:from_map(PathOrCategory, AST) end,
    try	occi_rendering:parse(Mimetype, Data, Fun) of
	Entity -> 
	    create2(occi_type:type(Entity), Entity, Endpoint, Creds, BackendFun)
    catch throw:Err -> {error, Err}
    end.


create2(resource, Resource0, Endpoint, Creds, BackendFun) ->
    Resource = occi_resource:endpoint(Endpoint, Resource0),
    Success = fun () ->
		      ResourceBackend = BackendFun(),
		      Ret = erocci_backend:create(ResourceBackend, occi_resource:links([], Resource), 
						  erocci_creds:user(Creds), erocci_creds:group(Creds)),
		      case Ret of
			  {ok, Resource1, Serial} ->
			      try create_resource_links(occi_resource:links(Resource), 
							[], Resource1, Serial, ResourceBackend, Endpoint, Creds)
			      catch throw:Err ->
				      {error, Err}
			      end;
			  {error, _}=Err ->
			      Err
		      end
	      end,
    auth(create, Creds, erocci_node:entity(Resource), Success);

create2(link, Link, Endpoint, Creds, BackendFun) ->
    try occi_link:endpoint(Endpoint, Link) of
	Link1 ->
	    create_link(Link1, Creds, BackendFun)
    catch throw:Err ->
	    {error, Err}
    end.


create_link(Link, Creds, BackendFun) ->
    Success = fun () ->
		      CreateLink = fun (LinkAcc, _, CredsAcc) ->
					   erocci_backend:create(BackendFun(), 
								 LinkAcc, 
								 erocci_creds:user(CredsAcc), 
								 erocci_creds:group(CredsAcc)) end,
		      lists:foldl(fun (Fun, {ok, Acc, SerialAcc}) ->
					  Fun(Acc, SerialAcc, Creds);
				      (_Fun, {error, _}=Err) ->
					  Err
				  end, {ok, Link, undefined}, [CreateLink, 
							       fun link_resource_source/3,
							       fun link_resource_target/3])
	      end,
    auth(create, Creds, erocci_node:entity(Link), Success).


create_resource_links([], Acc, Resource, Serial, ResourceBackend, _Endpoint, _Creds) ->
    lists:foldl(fun (Link, {ok, Acc1, SerialAcc}) ->
			Ret = erocci_backend:link(ResourceBackend, Acc1, source, 
						  occi_link:location(Link)),
			case Ret of
			    ok -> {ok, occi_resource:add_link(Link, Acc1), SerialAcc};
			    {error, _}=Err -> Err
			end;
		    (_, {error, _}=Err) ->
			Err
		end, {ok, Resource, Serial}, Acc);

create_resource_links([ Link | Links ], Acc, Resource, Serial, ResourceBackend, Endpoint, Creds) when ?is_link(Link) ->
    Link1 = occi_link:set(#{<<"occi.core.source">> => occi_resource:location(Resource)}, internal, Link),
    case create_resource_link(Link1, Endpoint, Creds) of
	{ok, Link2, _} ->
	    create_resource_links(Links, [ Link2 | Acc ], Resource, Serial, ResourceBackend, Endpoint, Creds);
	{error, _}=Err ->
	    Err
    end.


create_resource_link(Link0, Endpoint, Creds) ->
    Link = occi_link:endpoint(Endpoint, Link0),
    BackendFun = case occi_link:location(Link) of
		     undefined ->
			 fun () -> hd(erocci_backends:by_category_id(occi_link:kind(Link))) end;
		     Url ->
			 fun () -> erocci_backends:by_path(Url) end
		 end,
    Success = fun () ->
		      Ret = erocci_backend:create(BackendFun(), Link, 
						  erocci_creds:user(Creds), erocci_creds:group(Creds)),
		      case Ret of
			  {ok, Link1, _} ->
			      link_resource_target(Link1, undefined, Creds);
			  {error, _}=Err ->
			      Err
		      end
	      end,
    auth(create, Creds, erocci_node:entity(Link), Success).


link_resource_source(Link, Serial, Creds) ->
    SourceLocation = occi_link:source(Link),
    case entity(SourceLocation, Creds, read) of
	{ok, Source, _} ->
	    Backend = erocci_backends:by_path(occi_entity:location(Source)),
	    case erocci_backend:link(Backend, Source, source, occi_link:location(Link)) of
		ok -> {ok, Link, Serial};
		{error, _}=Err -> Err
	    end;
	{error, _}=Err ->
	    Err
    end.


link_resource_target(Link, Serial, Creds) ->
    TargetLocation = occi_link:target(Link),
    case entity(TargetLocation, Creds, read) of
	{ok, Target, _} ->
	    Backend = erocci_backends:by_path(occi_entity:location(Target)),
	    case erocci_backend:link(Backend, Target, target, occi_link:location(Link)) of
		ok -> {ok, Link, Serial};
		{error, _}=Err -> Err
	    end;
	{error, _}=Err ->
	    Err
    end.


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
