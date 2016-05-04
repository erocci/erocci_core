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
%%% Created : 24 Jan 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(erocci_node).

-include_lib("occi/include/occi_types.hrl").

-export([capabilities/1,
	 entity/1,
	 type/1,
	 location/1,
	 data/1,
	 data/2,
	 owner/1,
	 owner/2,
	 group/1,
	 group/2]).


-type type() :: capabilities
	      | {collection, kind}
	      | {collection, mixin}
	      | entity.


-type t() :: #{}.

-export_type([t/0]).

%%%
%%% API
%%%

%% @doc Creates a capabilities node
%% @end
-spec capabilities([occi_category:t()]) -> t().
capabilities(Categories) ->
    #{ type => capabilities,
       data => Categories,
       location => <<"/-/">>
     }.


%% @doc Creates an entity node
%% @end
-spec entity(occi_entity:t() | binary()) -> t().
entity(Path) when is_binary(Path) ->
    #{ type => entity,
       id => Path,
       data => undefined,
       location => Path };

entity(Entity) when ?is_entity(Entity) ->
    Id = occi_entity:id(Entity),
    #{ type => entity,
       id => Id,
       data => Entity,
       location => Id
     }.


%% @doc Get node type
%% @end
-spec type(t()) -> type().
type(#{ type := Type }) ->
    Type.


%% @doc Get node data
%% @end
-spec data(t()) -> occi_type:t().
data(#{ data := Data }) ->
    Data;

data(_) ->
    undefined.


%% @doc Set node data
%% @end
-spec data(term(), t()) -> t().
data(Data, Node) ->
    Node#{ data => Data }.


%% @doc Get node location
%% @end
-spec location(t()) -> binary().
location(#{ location := Location }) ->
    Location.


%% @doc Get node owner
%% @end
-spec owner(t()) -> erocci_creds:user().
owner(N) ->
    maps:get(owner, N, anonymous).


%% @doc Set node owner
%% @end
-spec owner(erocci_creds:t(), t()) -> t().
owner(Owner, N) when is_binary(Owner); anonyous =:= Owner ->
    N#{ owner => Owner }.


%% @doc Get node owner group
%% @end
-spec group(t()) -> erocci_creds:group().
group(N) ->
    maps:get(group, N, anonymous).


%% @doc Set node owner group
%% @end
-spec group(erocci_creds:group(), t()) -> t().
group(Group, N) when is_binary(Group); anonymous =:= Group ->
    N#{ group => Group }.
