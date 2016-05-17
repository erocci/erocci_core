%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2014-2016 Jean Parpaillon
%%% 
%%% This file is provided to you under the license described
%%% in the file LICENSE at the root of the project.
%%%
%%% You can also download the LICENSE file from the following URL:
%%% https://github.com/erocci/erocci/blob/master/LICENSE
%%% 
%%% @doc See ACL format in `erocci_acl.hrl'
%%%
%%% @end
%%% Created : 15 May 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(erocci_acls).

-include("erocci_acl.hrl").

-behaviour(gen_server).

-export([start_link/0,
	 acls/0,
	 check/3]).

%% gen_server callbacks
-export([init/1,
	 handle_info/2,
	 handle_call/3,
	 handle_cast/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).

-type state() :: [erocci_acl:t()].

%% @doc Start ACLs manager
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Retrieve all ACLs
-spec acls() -> [erocci_acl:t()].
acls() ->
    gen_server:call(?SERVER, acls).
    

-spec check(op(), occi_node:t(), occi_creds:t()) -> policy().
check(Op, Node, Creds) ->
    check_acls(acls(), {Op, Node, Creds}).

%%%
%%% Priv
%%%
-spec init([]) -> {ok, state()}.
init([]) ->
    Acls = erocci_config:get(acl),
    {ok, Acls}.


handle_info(_Info, S) ->
    {noreply, S}.


handle_call(acls, _From, S) ->
    {reply, S, S};

handle_call(_Evt, _From, S) ->
    {reply, ok, S}.


handle_cast(_Evt, S) ->
    {noreply, S}.


terminate(_Reason, _S) ->
    ok.


code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


check_acls([], _) ->
    % Default policy. Should be configurable ?
    deny;

check_acls([ Acl | Acls ], Req) ->
    case match(Acl, Req) of
	false -> check_acls(Acls, Req);
	{true, Policy} -> Policy
    end.

match({P, '_', '_',            '_'},         {_, _,            _}) ->
    {true, P};

match({P, '_', '_',            Id},          {_, Node,         Creds}) -> 
    match_id(Id, Node, Creds, P);

match({P, '_', Prefix,         '_'},         {_, Node,         _}) -> 
    match_path(Prefix, Node, P);

match({P, Op1,  '_',           '_'},         {Op2, _,          _}) -> 
    match_op(Op1, Op2, P);

match({P, '_', Prefix,         Id},          {_, Node,         Creds}) ->
    match_and([{fun match_path/3, [Prefix, Node, P]},
	       {fun match_id/4, [Id, Node, Creds, P]}], P);

match({P, Op1, '_',            Id},          {Op2,Node,        Creds}) -> 
    match_and([{fun match_op/3, [Op1, Op2, P]},
	       {fun match_id/4, [Id, Node, Creds, P]}], P);

match({P, Op1, Prefix,        '_'},          {Op2, Node,       _}) -> 
    match_and([{fun match_op/3, [Op1, Op2, P]},
	       {fun match_path/3, [Prefix, Node, P]}], P);

match({P, Op1,  Prefix,       Id},           {Op2,Node,        Creds}) ->
    match_and([{fun match_op/3, [Op1, Op2, P]},
	       {fun match_path/3, [Prefix, Node, P]},
	       {fun match_id/4, [Id, Node, Creds, P]}], P);

match(_,                                     _) -> 
    false.


match_and([], P) ->
    {true, P};

match_and([ {Fun, Args} | Funs ], P) ->
    case erlang:apply(Fun, Args) of
	{true, _P} -> match_and(Funs, P);
	false -> false
    end.


match_op(update, {action, _}, P) -> {true, P};

match_op(O, O, P) ->                {true, P};

match_op(_, _, _) ->                false.


match_id(owner, Node, Creds, P) ->
    match_owner(erocci_node:owner(Node), erocci_creds:user(Creds), P);

match_id({user, User}, _Node, Creds, P) ->
    match_user(User, erocci_creds:user(Creds), P);

match_id(group, Node, Creds, P) ->
    match_group_owner(erocci_node:group(Node), erocci_creds:group(Creds), P);

match_id({group, Group}, _Node, Creds, P) ->
    match_group(Group, erocci_creds:group(Creds), P);

match_id(authenticated, _Node, Creds, P) ->
    case erocci_creds:is_authenticated(Creds) of
	true -> {true, P};
	false -> false
    end.


match_owner(User, User, P) ->
    {true, P};

match_owner(_Owner, _User, _P) ->
    false.


match_user(User, User, P) ->
    {true, P};

match_user(_Owner, _User, _P) ->
    false.


match_group_owner(Group, Group, P) ->
    {true, P};

match_group_owner(_Group1, _Group2, _P) ->
    false.


match_group(Group, Group, P) ->
    {true, P};

match_group(_Group1, _Group2, _P) ->
    false.


match_path(Prefix, Node, P) ->
    case erocci_node:type(Node) of
	capabilities -> 
	    match_path_query(Prefix, P);
	_ ->
	    match_path2(filename:split(Prefix), filename:split(erocci_node:location(Node)), P)
    end.


match_path_query(query, P) ->
    {true, P};

match_path_query(_, _) ->
    false.


match_path2([], _Path, P) ->
    {true, P};

match_path2(_Prefix, [], _P) ->
    false;

match_path2([ H | Prefix ], [ H | Path ], P) ->
    match_path2(Prefix, Path, P);

match_path2([ _H1 | _Prefix ], [ _H2 | _Path ], _P) ->
    false.

