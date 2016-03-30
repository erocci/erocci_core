%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_listener).

-export([new/3,
	 id/1,
	 spec/1]).

-type id() :: atom().
-record(listener, { id      :: id(),
		    handler :: atom(),
		    opts    :: term()}).
-type t() :: #listener{}.

-export_type([id/0, t/0]).


%% @doc Create a listener structure
%% @end
-spec new(Ref :: atom(), Handler :: atom(), Opts :: term()) -> t().
new(Ref, Handler, Opts) ->
    #listener{ id=Ref, handler=Handler, opts=Opts}.


%% @doc Get listener ref
%% @end
-spec id(t()) -> id().
id(#listener{ id=Id }) ->
    Id.


%% @doc Get listener desc as child spec
%% @end
-spec spec(t()) -> suervisor:child_spec().
spec(#listener{ id=Id, handler=Handler, opts=Opts }) ->
    {Id, 
     {Handler, start_link, [Id, Opts]},
     permanent, 2000, worker,
     [Handler]}.
