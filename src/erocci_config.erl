%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright (c) 2014-2016 Jean Parpaillon
%% @doc
%% @end
%% Created : 20 Jan 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_config).

-include("erocci_log.hrl").

-export([get/1,
         get_raw/2]).

-type key() :: listeners
             | backends
             | acl.
-type value() :: [ erocci_listener:t() ]
               | [ erocci_backend:t() ]
               | [ erocci_acl:t() ].

%% @doc Get a configuration value, eventually pre-processed
%% * `listeners -> [occi_listener:t()]'
%% * `backends -> [occi_backend:t()]'
%% * `acl -> [erocci_acl:t()]'
%% @todo Cache computed values (eg ACLs)
%% @end
-spec get(Key :: key()) -> value().
get(listeners) ->
  lists:map(fun (Config) ->
                erocci_listener:new(Config)
            end, application:get_env(erocci_core, listeners, []));

get(backends) ->
  lists:map(fun (Config) ->
                erocci_backend:new(Config)
            end, application:get_env(erocci_core, backends, []));

get(acl) ->
  lists:map(fun (Acl) ->
                erocci_acl:validate(Acl)
            end, application:get_env(erocci_core, acl, []));

get(Key) ->
  get_raw(Key, undefined).


%% @doc Get raw value from @see application:get_env
%% @end
-spec get_raw(Key :: key(), Default :: term()) -> term().
get_raw(Key, Default) ->
  application:get_env(erocci_core, Key, Default).
