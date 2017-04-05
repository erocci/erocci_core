%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 12 May 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_errors).

-include("erocci_log.hrl").

-export([render/2]).

-type t() :: erocci_store:error()
           | erocci_backend:error()
           | occi_rendering:error().

%% @doc Render errors
%% @end
-spec render(occi_utils:mimetype(), t()) -> binary().
render(Mimetype, Errors) ->
  ?debug("ERROR (~p): ~p", [Mimetype, Errors]),
  (renderer(occi_utils:normalize_mimetype(Mimetype)))(Errors).


%%%
%%% Priv
%%%
renderer({<<"application">>, <<"occi+xml">>, _}) -> fun render_xml/1;
renderer({<<"application">>, <<"occi+json">>, _})-> fun render_json/1;
renderer({<<"text">>, <<"occi+plain">>, _})      -> fun render_plain/1;
renderer({<<"text">>, <<"occi">>, _})            -> fun render_occi/1;
renderer({<<"text">>, <<"uri-list">>, _})        -> fun render_uri/1.


render_xml(_Errors) ->
  <<>>.


render_json(_Errors) ->
  <<>>.


render_plain(Errors) when is_list(Errors) ->
  render_plain(Errors, []);

render_plain(Error) ->
  io_lib:format("error: ~p~n", [Error]).


render_plain([], Acc) ->
  Acc;

render_plain([ Error | Tail ], Acc) ->
  render_plain(Tail, [ Acc, io_lib:format("error: ~p~n", [Error]) ]).


render_occi(_Errors) ->
  <<>>.


render_uri(_Errors) ->
  <<>>.
