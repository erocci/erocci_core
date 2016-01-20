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
%%% Created : 10 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_parser_tests).

-include_lib("eunit/include/eunit.hrl").

parser_test_() ->
    Files = filelib:wildcard("../tests/plain/valid*.txt"),
    {setup,
     fun setup/0,
     fun cleanup/1,
     lists:map(fun(File) ->
		       ?_test(parse(File))
	       end, Files)}.

parse(File) ->
    ?debugFmt("parse ~s", [File]),
    {ok, In} = file:read_file(File),
    case  occi_parser:parse(occi_scanner:scan(In)) of
	{ok, _Result} ->
	    %?debugFmt("~n~p", [Result]),
	    ok;
	{error, {Line, Number, Msg}} ->
	    ?debugMsg(?MODULE:format_error(Msg)),
	    throw({error, {Line, Number, Msg}})
    end.

setup() ->
    ok.

cleanup(_) ->
    ok.
