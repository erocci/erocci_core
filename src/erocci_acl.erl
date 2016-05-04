%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_acl).

-include("erocci_acl.hrl").

-export([validate/1]).

-define(is_policy(X), (allow =:= X orelse deny =:= X)).

-define(is_op_action(X), (is_tuple(X) andalso action =:= element(1, X))).
-define(is_op(X), (create =:= X orelse read =:= X orelse update =:= X orelse ?is_op_action(X) orelse delete =:= Op)).

-define(is_location(X), (is_binary(X) orelse query =:= X)).

-define(is_user(X), (is_binary(X) orelse admin =:= X orelse anonymous =:= X)).

-define(is_group(X), (is_binary(X) orelse admin =:= X orelse anonymous =:= X)).

-define(is_id(X), (owner =:= X 
		   orelse (is_tuple(X) andalso user =:= element(1, X) andalso ?is_user(element(2, X)))
		   orelse group =:= X
		   orelse (is_tuple(X) andalso group =:= element(1, X) andalso ?is_group(element(2, X)))
		   orelse authenticated =:= X
		   orelse admin =:= X)).

%% @doc Validate ACL
%% @throws {invalid_acl, term()}
%% @end
-spec validate(term()) -> t().
validate({Policy, Op, Location, Id}) when is_list(Location) ->
    validate({Policy, Op, list_to_binary(Location), Id});

validate({Policy, Op, <<"/-/">>, Id}) ->
    validate({Policy, Op, query, Id});

validate({Policy, Op, <<"/.well-known/org/ogf/occi/-/">>, Id}) ->
    validate({Policy, Op, query, Id});

validate({Policy, Op, Location, Id}=Acl) 
  when ?is_policy(Policy), ?is_op(Op), ?is_location(Location), ?is_id(Id) ->
    Acl;

validate(Acl) ->
    throw({invalid_acl, Acl}).

