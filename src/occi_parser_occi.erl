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
%%% Created : 5 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_parser_occi).

-include("occi_parser_text.hrl").

%% API
-export([parse_action/3,
	 parse_entity/3,
	 parse_user_mixin/2,
	 parse_collection/2]).

%% New API
-export([parse/3]).

%%%===================================================================
%%% API
%%%===================================================================
-spec parse(Content :: binary(), Ctx :: term(), Type :: req_type()) -> {ok, term()} | {error, term()}.
parse(_, Req, Type) ->
    case parse_headers(Req, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse(Headers, #state{type=Type})
    end.

parse_action(_, Req, Action) ->
    case parse_headers(Req, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_action(Headers, #state{action=Action})
    end.    

parse_entity(_, Req, #occi_resource{}=Res) ->
    case parse_headers(Req, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_entity(Headers, #state{entity=Res})
    end;

parse_entity(_, Req, #occi_link{}=Link) ->
    case parse_headers(Req, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_entity(Headers, #state{entity=Link})
    end;

parse_entity(_, Req, #occi_entity{id=Id}) ->
    case parse_headers(Req, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_entity(Headers, #state{entity_id=Id})
    end.

parse_user_mixin(_, Req) ->
    case parse_headers(Req, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_user_mixin(Headers, #state{mixin=occi_mixin:new(#occi_cid{class=mixin})})
    end.

parse_collection(_, Req) ->
    case parse_headers(Req, orddict:new()) of
	{error, Err} ->
	    {error, Err};
	{ok, Headers} ->
	    occi_parser_text:parse_collection(Headers, #state{})
    end.

%%%
%%% Priv
%%%
parse_headers(Req, Acc) ->
    {Headers, _} = cowboy_req:headers(Req),
    parse_header(Headers, Acc).

parse_header([], Acc) ->
    {ok, reverse(Acc)};
parse_header([{Name, Bin}|T], Acc) ->
    parse_header(T, add_header_value(Name, parse_values(Bin), Acc)).

parse_values(Bin) ->
    parse_values(Bin, <<>>, []).

parse_values(<<>>, SoFar, Acc) ->
    [SoFar | Acc];
parse_values(<< $\\, $,, Rest/bits >>, SoFar, Acc) ->
    parse_values(Rest, << SoFar/binary, $,, $\\ >>, Acc);
parse_values(<< $,, Rest/bits >>, <<>>, Acc) ->
    parse_values(Rest, <<>>, Acc);
parse_values(<< $,, Rest/bits >>, SoFar, Acc) ->
    parse_values(Rest, <<>>, [SoFar | Acc]);
parse_values(<< C, Rest/bits >>, SoFar, Acc) ->
    parse_values(Rest, << SoFar/binary, C >>, Acc).

add_header_value(Name, Values, Acc) ->
    CanName = ?hdr_to_atom(Name),
    case orddict:find(CanName, Acc) of
	{ok, V} -> 
	    orddict:store(CanName, Values ++ V, Acc);
	error -> 
	    orddict:store(CanName, Values, Acc)
    end.

reverse(H) ->
    orddict:fold(fun (Key, Values, Acc) ->
			 orddict:store(Key, lists:reverse(Values), Acc)
		 end, orddict:new(), H).
