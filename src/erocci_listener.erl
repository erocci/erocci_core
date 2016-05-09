%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_listener).

-export([new/1,
	 id/1,
	 spec/1]).

-type id() :: atom().
-record(listener, { id      :: id(),
		    handler :: atom(),
		    opts    :: term()}).
-type t() :: #listener{}.

-export_type([id/0, t/0]).

-type opts() :: [{atom(), any()}].
-callback start_link(atom(), opts()) -> ok | {error, atom()}.
-callback terminate(atom(), term()) -> ok.

%% @doc Create a listener structure
%% @throw {listener, term()}
%% @end
-spec new({Ref :: atom(), Handler :: atom(), Opts :: term()}) -> t().
new({Ref, Handler, Opts}) when is_atom(Handler) ->
    #listener{ id=Ref, handler=Handler, opts=Opts};

new(Else) ->
    throw({listener, Else}).


%% @doc Get listener ref
%% @end
-spec id(t()) -> id().
id(#listener{ id=Id }) ->
    Id.


%% @doc Get listener desc as child spec
%% @end
-spec spec(t()) -> supervisor:child_spec().
spec(#listener{ id=Id, handler=Handler, opts=Opts }) ->
    #{ id => Id,
       start => {Handler, start_link, [Id, Opts]} 
     }.
