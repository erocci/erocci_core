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
-module(occi_renderer_plain).

-behaviour(occi_renderer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("occi.hrl").

%% API
-export([render/2]).
-export([render_headers/2]).

%%%===================================================================
%%% API
%%%===================================================================
render(Node, #occi_env{}=Env) ->
    occi_renderer_text:render(Node, Env, fun ?MODULE:render_headers/2).

render_headers(Headers, #occi_env{req=Req}=Env) ->
    {BodyHdr, Req2} = 
	lists:foldl(fun (Name, Acc) ->
			    dispatch_headers(Name, Acc, Headers)
		    end, {[], Req}, lists:reverse(orddict:fetch_keys(Headers))),
    {[ occi_renderer:join(BodyHdr, "\n") ], Env#occi_env{req=Req2}}.

%%
%% Private
%%
dispatch_headers(<<"location">>, {BodyAcc, ReqAcc}, Headers) ->
    Value = occi_renderer:join(orddict:fetch(<<"location">>, Headers), ", "),
    ReqAcc2 = cowboy_req:set_resp_header(<<"location">>, Value, ReqAcc),
    {BodyAcc, ReqAcc2};

dispatch_headers(Name, {BodyAcc, ReqAcc}, Headers) -> 
    {[ render_header(Name, orddict:fetch(Name, Headers)) | BodyAcc ], ReqAcc}.


render_header(Name, Values) ->
    occi_renderer:join(
      lists:foldl(fun ([], Acc) ->
			  Acc;
		      (Value, Acc) ->
			  [ [ Name, ": ", Value] | Acc]
		  end, [], Values),
      "\n").
