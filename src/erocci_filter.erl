%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Represent an OCCI request filter
%%%
%%% @end
%%% Created : 30 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_filter).

-include_lib("occi/include/occi_types.hrl").

-export([new/0,
         add_eq/3,
         add_like/3,
         match/2]).

-type key() :: binary() | '_'.
-type value() :: term().

-type filter() :: {eq, key(), value()}
                | {like, key(), value()}.

-type t() :: [filter()].

-export_type([t/0]).

%% @doc Create new filter
%% @end
-spec new() -> t().
new() ->
  [].


%% @doc Add an equality filter
%% @end
-spec add_eq(key(), value(), t()) -> t().
add_eq(Key, Value, Filters) ->
  [ {eq, Key, Value} | Filters ].


%% @doc Add a like filter.
%% Value must be a string.
%% @end
-spec add_like(key(), value(), t()) -> t().
add_like(Key, Value, Filters) ->
  [ {like, Key, Value} | Filters ].


%% @doc Return true if Object match filter, false otherwise
%% @todo Implements filtering (always returns true)
%% @end
-spec match(occi_type:t(), t()) -> boolean().
match(_Obj, []) ->
  true;

match(_Obj, _Filter) ->
  true.


%%%
%%% Priv
%%%
