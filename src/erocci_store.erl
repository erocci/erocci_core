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

-include("erocci.hrl").


-export([capabilities/0,
	 capabilities/3,
	 find/4,
	 delete/1,
	 load/1,
	 load_collection/3,
	 save/2,
	 update/2,
	 action/3,
	 associate/2,
	 disassociate/2,
	 full_associate/2,
	 add_mixin/2,
	 delete_mixin/1]).

-type store_error() :: not_found
		     | {unauthorized, binary()}.

-type errors() :: [store_error()].

%% @equiv capabilities(erocci_creds:anonymous(), undefined, <<>>)
%% @end
-spec capabilities() -> erocci_node:t().
capabilities() ->
    capabilities(erocci_creds:anonymous(), undefined, <<>>).


%% @doc Retrieve capabilities node
%% @todo
%% @end
-spec capabilities(erocci_creds:t(), erocci_filter:t() | undefined, Baseurl :: binary()) -> 
			  {ok, erocci_node:t()} | {error, errors()}.
capabilities(Creds, Filter, Url) ->
    {error, not_found}.


%% @doc Retrieve the node belonging to a path
%% @todo
%% @end
-spec find(Path :: binary(), erocci_creds:t(), erocci_filter:t() | undefined, Baseurl :: binary()) -> erocci_node:t().
find(Path, Creds, Filter, Url) ->
    {error, not_found}.


%% @doc Delete the given node
%% @todo
%% @end
-spec delete(erocci_node:t()) -> ok | {error, errors()}.
delete(Node) ->
    ok.


%% @doc Load content of the node
%% @todo
%% @end
-spec load(erocci_node:t()) -> occi_type:t().
load(Node) ->
    undefined.


%% @doc Load content of a collection
%% @todo
%% @end
-spec load_collection(erocci_node:t(), integer(), integer() | undefined) -> occi_collection:t().
load_collection(Node, Start, Limit) ->
    undefined.


%% @doc Save the entity in the given node
%% Return the entity normalized in the context of the node
%% @todo
%% @end
-spec save(occi_type:t(), erocci_node:t()) -> {ok, occi_entity:t()} | {error, errors()}.
save(Entity, Node) ->
    {error, not_found}.


%% @doc Update an entity
%% @todo
%% @end
-spec update(occi_type:t(), erocci_node:t()) -> {ok, occi_entity:t()} | {error, errors()}.
update(Entity, Node) ->
    {error, not_found}.


%% @doc Invoke an action on an entity
%% @todo
%% @end
-spec action(Term:: binary(), occi_type:t(), erocci_node:t()) -> {ok, occi_type:t()} | {error, errors()}.
action(Term, Obj, Node) ->
    {error, not_found}.


%% @doc Associate entities with mixin
%% @todo
%% @end
-spec associate(occi_type:t(), erocci_node:t()) -> {ok, occi_collection:t()} | {error, errors()}.
associate(Obj, Node) ->
    {error, not_found}.


%% @doc Disassociate entities from the collection.
%% @todo
%% @end
-spec disassociate(occi_type:t(), erocci_node:t()) -> {ok, occi_collection:t()} | {error, errors()}.
disassociate(Obj, Node) ->
    {error, not_found}.


%% @doc Set the full list of entities associated with the collection
%% @todo
%% @end
-spec full_associate(occi_type:t(), erocci_node:t()) -> {ok, occi_collection:t()} | {error, errors()}.
full_associate(Coll, Node) ->
    {error, not_found}.


%% @doc Add user mixin
%% @todo
%% @end
-spec add_mixin(occi_type:t(), erocci_node:t()) -> ok | {error, errors()}.
add_mixin(Mixin, Node) ->
    {error, not_found}.


%% @doc Delete a user mixin
%% @todo
%% @end
-spec delete_mixin(occi_type:t()) -> ok | {error, errors()}.
delete_mixin(Mixin) ->
    {error, not_found}.
