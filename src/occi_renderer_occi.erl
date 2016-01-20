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
-module(occi_renderer_occi).

-behaviour(occi_renderer).

-include("occi.hrl").

%% API
-export([render/2,
         render_headers/2]).

%%%===================================================================
%%% API
%%%===================================================================
render(Node, Env) ->
    occi_renderer_text:render(Node, Env, fun ?MODULE:render_headers/2).

render_headers(Headers, #occi_env{req=Req}=Env) ->
    Req2 = lists:foldl(fun (Name, Acc) -> 
                               Value = occi_renderer:join(
                                         lists:reverse(orddict:fetch(Name, Headers)),
                                         ", "),
                               cowboy_req:set_resp_header(Name, Value, Acc)
                       end, Req, orddict:fetch_keys(Headers)),
    {<<"OK\n">>, Env#occi_env{req=Req2}}.
