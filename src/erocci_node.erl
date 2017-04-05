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
         entity/3,
         entity/4,
         type/1,
         location/1,
         data/1,
         data/2,
         owner/1,
         owner/2,
         group/1,
         group/2,
         serial/1,
         serial/2]).


-type type() :: capabilities
              | {collection, kind}
              | {collection, mixin}
              | entity.


-type serial() :: binary() | undefined.
-type t() :: #{}.

-export_type([t/0, serial/0]).

%%%
%%% API
%%%

%% @doc Creates a capabilities node
%% @end
-spec capabilities([occi_category:t()]) -> t().
capabilities(Categories) ->
  #{ type => capabilities,
     data => Categories,
     location => <<"/-/">>,
     serial => undefined
   }.


%% @doc Creates an entity node
%% @end
-spec entity(occi_entity:t() | binary()) -> t().
entity(Path) when is_binary(Path) ->
  #{ type => entity,
     id => Path,
     data => undefined,
     location => Path,
     serial => undefined
   };

entity(Entity) when ?is_entity(Entity) ->
  #{ type => entity,
     data => Entity,
     location => occi_entity:location(Entity),
     serial => undefined
   }.


%% @doc Creates an entity node
%% @end
-spec entity(occi_entity:t(), erocci_creds:user(), erocci_creds:group()) -> t().
entity(Entity, Owner, Group) ->
  group(Group, owner(Owner, entity(Entity))).


%% @doc Creates an entity node
%% @end
-spec entity(occi_entity:t(), erocci_creds:user(), erocci_creds:group(), serial()) -> t().
entity(Entity, Owner, Group, Serial) ->
  serial(Serial, entity(Entity, Owner, Group)).


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
-spec owner(erocci_creds:user(), t()) -> t().
owner(Owner, N) when is_binary(Owner); anonymous =:= Owner ->
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


%% @doc Get node serial (tag)
%% @end
-spec serial(t()) -> serial().
serial(#{ serial := Serial }) ->
  Serial.


%% @doc Set node serial number (tag)
%% @end
-spec serial(serial(), t()) -> t().
serial(Serial, Node) when is_map(Node) ->
  Node#{ serial := Serial }.
