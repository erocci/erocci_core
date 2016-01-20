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
%%% Created :  3 Sep 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_renderer).

-include("occi.hrl").

% Some common functions
-export([join/2, to_list/1]).

-callback render(Node :: occi_node(), Env :: any()) ->   % text/occi rendering needs HTTP request, beurk !!!
    binary() | list().

%%%
%%% API
%%%
join(L, Sep) ->
    join(L, [], Sep).

join([], Acc, _Sep) ->
    lists:reverse(Acc);
join([H|[]], Acc, _Sep) ->
    lists:reverse([H|Acc]);
join([H, []|T], Acc, Sep) ->
    join([H|T], Acc, Sep);
join([H|T], Acc, Sep) ->
    join(T, [[H, Sep]|Acc], Sep).

to_list(L) ->
    lists:map(fun(X) when is_atom(X) ->
		      atom_to_list(X);
		 (X) -> X end, L).
