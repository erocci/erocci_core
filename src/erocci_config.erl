%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright (c) 2014-2016 Jean Parpaillon
%% @doc 
%% @end
%% Created : 20 Jan 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_config).

-export([get/1,
	 get_raw/2]).

-type key() :: listeners.
-type value() :: [listener()].

-type listener() :: {Ref :: atom(), Handler :: atom(), Opts :: term()}.

%% @doc Get a configuration value amongst
%% * `listeners -> [occi_listener:t()]'
%% @end
-spec get(Key :: key()) -> term().
get(listeners) ->
    lists:map(fun ({Ref, Handler, Opts}) ->
		      occi_listener:new(Ref, Handler, Opts)
	      end, application:get_env(erocci, listeners, []));

get(Key) ->
    get_raw(Key, undefined).


%% @doc Get raw value from @see application:get_env
%% @end
-spec get_raw(Key :: key(), Default :: term()) -> value().
get_raw(Key, Default) ->
    application:get_env(erocci, Key, Default).
