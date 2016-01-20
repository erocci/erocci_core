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
%%%
%%% @end
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_uri_list).

-behaviour(occi_renderer).

-include("occi.hrl").

%% API
-export([render/2]).

%%%===================================================================
%%% API
%%%===================================================================
render(#occi_node{type=occi_collection, data=Coll}, Env) ->
    Data = lists:map(fun (#uri{}=Id) ->
                             [ occi_uri:to_iolist(Id, Env), "\n"];
                         (#occi_node{id=Id}) ->
                             [ occi_uri:to_iolist(Id, Env), "\n" ]
                     end, occi_collection:get_entities(Coll)),
    {Data, Env};

render(#occi_node{type=capabilities, data={Kinds, Mixins, Actions}}, Env) ->
    Data = occi_renderer:join(
             lists:reverse(
               lists:foldl(fun (#occi_kind{location=#uri{}=Uri}, Acc) ->
                                   [occi_uri:to_iolist(Uri, Env)|Acc];
                               (#occi_mixin{location=#uri{}=Uri}, Acc) ->
                                   [occi_uri:to_iolist(Uri, Env)|Acc];
                               (#occi_action{location=undefined}, Acc) ->
                                   Acc;
                               (#occi_action{location=#uri{}=Uri}, Acc) ->
                                   [occi_uri:to_iolist(Uri, Env)|Acc]
                           end, [], Kinds ++ Mixins ++ Actions)),
             <<"\n">>),
    {Data, Env}.

%%%
%%% Private
%%%
