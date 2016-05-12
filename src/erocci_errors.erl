%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 12 May 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_errors).

-export([render/2]).

-type t() :: erocci_store:error()
	   | erocci_backend:error()
	   | occi_rendering:error().

%% @doc Render errors
%% @end
-spec render(occi_utils:mimetype(), t()) -> binary().
render(_Mimetype, _Errors) ->
    <<>>.
