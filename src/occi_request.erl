%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (c) 2014-2016 Jean Parpaillon
%%% 
%%% This file is provided to you under the license described
%%% in the file LICENSE at the root of the project.
%%%
%%% You can also download the LICENSE file from the following URL:
%%% https://github.com/erocci/erocci/blob/master/LICENSE
%%% 
%%% @doc An event based JSON parser
%%%
%%% @end
%%% Created : 11 Jan 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_request).

-include("occi.hrl").

-export([new/0,
	 add_entity/2,
	 get_entities/1,
	 set_action/2,
	 get_action/1,
	 add_mixin/2,
	 get_mixins/1,
	 set_collection/2,
	 get_collection/1]).

new() ->
    #occi_request{}.

add_entity(#occi_request{entities=Entities}=Req, #occi_resource{}=Res) ->
    Req#occi_request{entities=[Res|Entities]};

add_entity(#occi_request{entities=Entities}=Req, #occi_link{}=Link) ->
    Req#occi_request{entities=[Link|Entities]}.

get_entities(#occi_request{entities=Entities}) ->
    Entities.

add_mixin(#occi_request{mixins=Mixins}=Req, Mixin) ->
    Req#occi_request{mixins=[Mixin|Mixins]}.

get_mixins(#occi_request{mixins=Mixins}) ->
    Mixins.

set_collection(#occi_request{}=Req, #occi_collection{}=Col) ->
    Req#occi_request{collection=Col}.

get_collection(#occi_request{collection=Col}) ->
    Col.

set_action(#occi_request{}=Req, Action) ->
    Req#occi_request{action=Action}.

get_action(#occi_request{action=Action}) ->
    Action.

