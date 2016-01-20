%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2014-2016 Jean Parpaillon
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
%%% Created : 1 Jul 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_capabilities).

-include("occi.hrl").

-export([new/0,
	 new/3,
	 set/4,
	 add_mixin/2,
	 merge/2]).

-spec new() -> occi_node().
new() ->
    #occi_node{id=#uri{path="/-/"}, type=capabilities, objid=capabilities, owner=nobody, data={[],[],[]}}.

-spec new([occi_kind()], [occi_mixin()], [occi_action()]) -> occi_node().
new(Kinds, Mixins, Actions) when is_list(Kinds),
				 is_list(Mixins),
				 is_list(Actions) ->
    #occi_node{id=#uri{path="/-/"}, objid=capabilities, type=capabilities, owner=nobody, 
	       data={Kinds, Mixins, Actions}}.

-spec set(occi_node(), [occi_kind()], [occi_mixin()], [occi_action()]) -> occi_node().
set(#occi_node{type=capabilities}=N, Kinds, Mixins, Actions) when is_list(Kinds),
								  is_list(Mixins),
								  is_list(Actions) ->
    N#occi_node{data={Kinds, Mixins, Actions}}.
    
-spec add_mixin(occi_node(), occi_mixin()) -> occi_node().
add_mixin(#occi_node{type=capabilities, data={K, M, A}}=N, #occi_mixin{}=Mixin) ->
    N#occi_node{data={K, [Mixin | M], A}};
add_mixin(#occi_node{type=capabilities}=N, #occi_mixin{}=Mixin) ->
    N#occi_node{data={[], [Mixin], []}}.

-spec merge(occi_node(), occi_node()) -> occi_node().
merge(#occi_node{type=capabilities, data={K1, M1, A1}}=C1, 
      #occi_node{type=capabilities, data={K2, M2, A2}}) ->
    C1#occi_node{data={K1++K2, M1++M2, A1++A2}};
merge(#occi_node{type=capabilities}=C1, 
      #occi_node{type=capabilities, data={K2, M2, A2}}) ->
    C1#occi_node{data={K2, M2, A2}}.
