%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2013-2016 Jean Parpaillon
%%% 
%%% This file is provided to you under the license described
%%% in the file LICENSE at the root of the project.
%%%
%%% You can also download the LICENSE file from the following URL:
%%% https://github.com/erocci/erocci/blob/master/LICENSE
%%% 
%%% @doc An event based JSON parser
%%%
%%% @end
%%% Created : 11 Dec 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser).

-include("occi_log.hrl").
-include("occi_parser.hrl").

-export([send_event/3,
         parse_error/2]).

parse_error(#token{}=Token, Ctx) ->
    ?error(build_err(Token)),
    {reply, {error, parse_error}, eof, Ctx}.

build_err(#token{name=Name, pos=undefined, data=undefined}) ->
    io_lib:format("Invalid term: ~p~n", [Name]);
build_err(#token{name=Name, pos=undefined, data=Data}) ->
    io_lib:format("Invalid term: ~p(~p)~n", [Name, Data]);
build_err(#token{name=Name, pos=Pos, data=undefined}) ->
    io_lib:format("Invalid term at line ~p: ~p~n", [Pos, Name]);
build_err(#token{name=Name, pos=Pos, data=Data}) ->
    io_lib:format("Invalid term at line ~p: ~p(~p)~n", [Pos, Name, Data]).

send_event(_Event, IfOk, #parser{sink=undefined}) ->
    IfOk;
send_event(Event, IfOk, #parser{sink=Sink}=Ctx) ->
    Res = case Event of
              eof ->
                  stop_parser(Sink);
              Else ->
                  gen_fsm:sync_send_event(Sink#parser.id, Else)
          end,
    case Res of
        ok ->
            IfOk;
        {eof, Result} ->
            stop_parser(Sink),
            {reply, {eof, Result}, eof, Ctx};
        {error, Reason} ->
            stop_parser(Sink),
            {reply, {error, Reason}, eof, Ctx}
    end.

stop_parser(#parser{id=Ref}) ->
    try gen_fsm:sync_send_all_state_event(Ref, stop) of
        ok ->
            ok
    catch
        exit:{normal, _} ->
            ok;
        exit:{noproc, _} ->
            ok;
        _:Err ->
            {error, Err}
    end.
