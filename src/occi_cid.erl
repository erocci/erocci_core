%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% 
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%% 
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%% 
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_cid).

-include("occi.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([parse/1,
		 to_binary/1]).

-type t() :: #occi_cid{}.
-export_type([t/0]).

-spec parse(binary()) -> occi_cid().
parse(undefined) ->
    throw({error, invalid_cid});
parse(Bin) when is_binary(Bin) ->
    case binary:split(Bin, <<"#">>) of
	[Scheme, Term] ->
	    #occi_cid{scheme=?scheme_to_atom(<< Scheme/binary, "#" >>), term=?term_to_atom(Term), class='_'};
	_ ->
	    throw({error, {invalid_cid, Bin}})
    end.


-spec to_binary(occi_cid()) -> binary().
to_binary('_') ->
	<<>>;
to_binary(undefined) ->
	<<>>;
to_binary(#occi_cid{scheme=Scheme}=Cid) when is_atom(Scheme) ->
    to_binary(Cid#occi_cid{scheme=atom_to_binary(Scheme, utf8)});
to_binary(#occi_cid{term=Term}=Cid) when is_atom(Term) ->
    to_binary(Cid#occi_cid{term=atom_to_binary(Term, utf8)});
to_binary(#occi_cid{scheme=Scheme, term=Term}) when is_binary(Scheme), is_binary(Term) ->
    <<Scheme/binary, Term/binary>>.


%%%
%%% eunit
%%%
-ifdef(TEST).
to_binary_test_() ->
	[
	 ?_assertEqual(<<>>, 
				   to_binary('_'))
	,?_assertEqual(<<>>, 
				   to_binary(undefined))
	,?_assertEqual(<<"http://schemas.ogf.org/infrastructure#compute">>, 
				   to_binary(#occi_cid{scheme='http://schemas.ogf.org/infrastructure#', term= <<"compute">>}))
	,?_assertEqual(<<"http://schemas.ogf.org/infrastructure#compute">>, 
				   to_binary(#occi_cid{scheme= <<"http://schemas.ogf.org/infrastructure#">>, term= <<"compute">>}))
	,?_assertEqual(<<"http://schemas.ogf.org/infrastructure#compute">>, 
				   to_binary(#occi_cid{scheme='http://schemas.ogf.org/infrastructure#', term='compute'}))
	].
-endif.
