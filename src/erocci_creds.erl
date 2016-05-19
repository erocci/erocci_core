%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Represents authentication credentials
%%%
%%% @end
%%% Created : 30 Mar 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(erocci_creds).

%% Constructors
-export([basic/1,
	 basic/3,
	 anonymous/0]).

-export([is_authenticated/1,
	 challenge/1,
	 type/1,
	 user/1,
	 group/1]).


-type type() :: basic 
	      | anonymous.

-record(creds, { type                  :: type(),
		 challenge             :: fun(),
		 authenticated = false :: boolean(),
		 data                  :: term() }).
-type t() :: #creds{}.

-type user() :: binary() | admin | anonymous.
-type group() :: binary() | admin | anonymous.

-export_type([t/0, user/0, group/0]).

%% @doc Basic authentication, with authentication
%% WARNING: implements hard coded user/password
%% @todo implements real authentication with erocci_authnz (or PAM ?)
%% @end
-spec basic(User :: user(), Password :: binary(), Challenge :: fun()) -> t().
basic(<<"erocci">>, <<"erocci">>, Challenge) ->
    #creds{ type=basic, challenge=Challenge, authenticated=true, data=#{} };

basic(_User, _Password, Challenge) when is_binary(_User), is_binary(_Password) ->
    #creds{ type=basic, challenge=Challenge, authenticated=false, data=#{} }.


%% @doc Basic credentials, without authentication
%% @end
-spec basic(Challenge :: fun()) -> t().
basic(Challenge) ->
    #creds{ type=basic, challenge=Challenge, authenticated=false, data=#{} }.


%% @doc Anonymous authentication
%% @end
-spec anonymous() -> t().
anonymous() ->
    #creds{ type=anonymous, authenticated=true, data=#{} }.


%% @doc
%% @end
-spec is_authenticated(t()) -> boolean().
is_authenticated(#creds{ authenticated=A }) ->
    A.


%% @doc Authentication type
%% @end
-spec type(t()) -> type().
type(#creds{type=T}) ->
    T.


%% @doc Returns a challenge for authentication
%% @end
-spec challenge(t()) -> binary().
challenge(#creds{ type=anonymous }) ->
    <<>>;

challenge(#creds{ type=basic, challenge=Challenge }=Creds) ->
    Challenge(Creds).


%% @doc Get request user
%% @end
-spec user(t()) -> user().
user(#creds{ data=Data }) ->
    maps:get(user, Data, anonymous).


%% @doc Get request group
%% @end
-spec group(t()) -> group().
group(#creds{ data=Data }) ->
    maps:get(group, Data, anonymous).
