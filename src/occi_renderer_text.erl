%%%-------------------------------------------------------------------
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
%%% Created : 18 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_renderer_text).

-include("occi.hrl").

%% API
-export([render/3]).

%%%===================================================================
%%% API
%%%===================================================================
render(#occi_node{id=Id, data=#occi_resource{}=Entity}, Env, Renderer) ->
    Headers = render_resource(Id, Entity, orddict:new(), Env),
    Renderer(Headers, Env);

render(#occi_node{id=Id, data=#occi_link{}=Entity}, Env, Renderer) ->
    Headers = render_link(Id, Entity, orddict:new(), Env),
    Renderer(Headers, Env);

render(#occi_node{type=capabilities, data={Kinds, Mixins, Actions}}, Env, Renderer) ->
    Headers = lists:foldl(fun (Cat, Acc) ->
                                  render_category(Cat, Acc, Env)
                          end, orddict:from_list([{<<"category">>, []}]), Kinds ++ Mixins ++ Actions),
    Renderer(Headers, Env);

render(#occi_node{type=occi_collection, data=Coll}=_N, Env, Renderer) ->
    F = fun (#uri{}=EntityId, Acc) ->
                Uris = orddict:fetch(<<"x-occi-location">>, Acc),
                orddict:store(<<"x-occi-location">>, 
                              [occi_uri:to_iolist(EntityId, Env) | Uris], Acc);
            (#occi_node{id=Id, type=occi_resource, data=Res}, Acc) ->
                render_resource(Id, Res, Acc, Env);
            (#occi_node{id=Id, type=occi_link, data=Link}, Acc) ->
                render_link(Id, Link, Acc, Env)
        end,
    Headers = lists:foldl(F, orddict:from_list([{<<"x-occi-location">>, []}]), 
                          occi_collection:get_entities(Coll)),
    Renderer(Headers, Env).


render_category(#occi_kind{}=Kind, Hdr, Env) ->
    L = [build_cid(occi_kind:get_id(Kind), Env),
         render_kv(<<"title">>, occi_kind:get_title(Kind), Env),
         render_kv(<<"rel">>, render_cid_uri(occi_kind:get_parent(Kind)), Env),
         render_kv(<<"location">>, [occi_uri:to_iolist(occi_kind:get_location(Kind))], Env),
         render_kv(<<"attributes">>, render_attr_specs(occi_kind:get_attributes(Kind)), Env),
         render_kv(<<"actions">>, render_action_specs(occi_kind:get_actions(Kind)), Env)],
    add_header_value(<<"category">>, occi_renderer:join(L, "; "), Hdr);

render_category(#occi_mixin{}=Mixin, Hdr, Env) ->
    Applies = occi_mixin:get_applies(Mixin),
    Depends = occi_mixin:get_depends(Mixin),
    L = [build_cid(occi_mixin:get_id(Mixin), Env),
         render_kv(<<"title">>, occi_mixin:get_title(Mixin), Env),
         render_kv(<<"rel">>, [ render_cid_uri(Cid) || Cid <- Applies ++ Depends ], Env),
         render_kv(<<"location">>, [occi_uri:to_iolist(occi_mixin:get_location(Mixin))], Env),
         render_kv(<<"attributes">>, render_attr_specs(occi_mixin:get_attributes(Mixin)), Env),
         render_kv(<<"actions">>, render_action_specs(occi_mixin:get_actions(Mixin)), Env)],
    add_header_value(<<"category">>, occi_renderer:join(L, "; "), Hdr);

render_category(#occi_action{}=Action, Hdr, Env) ->
    L = [build_cid(occi_action:get_id(Action), Env),
         render_kv(<<"title">>, occi_action:get_title(Action), Env),
         render_kv(<<"attributes">>, render_attr_specs(occi_action:get_attributes(Action)), Env)],
    add_header_value(<<"category">>, occi_renderer:join(L, "; "), Hdr).

render_cid(#occi_cid{}=Cid, Acc, Env) ->
    add_header_value(<<"category">>, build_cid(Cid, Env), Acc).

render_resource(Id, #occi_resource{}=Res, Acc, Env) ->
    Acc2 = render_cid(occi_resource:get_cid(Res), Acc, Env),
    Acc3 = sets:fold(fun(X, IntAcc) -> render_cid(X, IntAcc, Env) end, 
                     Acc2, occi_resource:get_mixins(Res)),
    Attrs = occi_resource:get_attributes(Res),
    Acc4 = lists:foldl(fun(X, IntAcc) -> render_attribute(X, IntAcc, Env) end, 
                       Acc3, Attrs),
    Acc5 = lists:foldl(fun (X, IntAcc) -> render_inline_link(X, IntAcc, Env) end, 
                       Acc4, occi_resource:get_links(Res)),
    render_location(Id, Acc5, Env).

render_link(Id, #occi_link{}=Link, Acc, Env) ->
    Acc2 = render_cid(occi_link:get_cid(Link), Acc, Env),
    Acc3 = sets:fold(fun (X, IntAcc) -> render_cid(X, IntAcc, Env) end, 
                     Acc2, occi_link:get_mixins(Link)),
    Attrs = occi_link:get_attributes(Link),
    Acc4 = lists:foldl(fun (X, IntAcc) -> render_attribute(X, IntAcc, Env) end, 
                       Acc3, Attrs),
    render_location(Id, Acc4, Env).

render_inline_link(#uri{}=Uri, Acc, Env) ->
    add_header_value(<<"link">>, [$<, occi_uri:to_iolist(Uri, Env), $>], Acc);
render_inline_link(#occi_link{id=LinkId}=Link, Acc, Env) ->
    add_header_value(<<"link">>, build_inline_link(LinkId, Link, Env), Acc).

render_location(#uri{}=Uri, Acc, Env) ->
    add_header_value(<<"location">>, occi_uri:to_iolist(Uri, Env), Acc).

render_cid_uri(undefined) ->
    undefined;
render_cid_uri(#occi_cid{scheme=Scheme, term=Term}) ->
    [ to_list(Scheme), to_list(Term) ].

render_attr_specs(Attributes) ->
    occi_renderer:join(orddict:fold(fun render_attr_spec/3, [], Attributes), " ").

render_attr_spec(Id, #occi_attr{}=Attr, Acc) ->
    [ [ to_list(Id), render_attr_properties(Attr) ] | Acc ].

render_attr_properties(#occi_attr{}=A) ->
    L = case occi_attribute:is_required(A) of
            true -> ["required"];       
            false -> []
        end,
    L1 = lists:append(L, case occi_attribute:is_immutable(A) of
                             true -> ["immutable"];
                             false -> []
                         end),
    case L1 of
        [] -> [];
        L2 ->
            [ <<"{">>, occi_renderer:join(L2, <<" ">>), <<"}">>]
    end.

render_action_specs(Actions) ->
    occi_renderer:join(
      lists:map(
        fun(Action) -> render_action_spec(Action)  end,
        Actions), " ").

render_action_spec(#occi_action{id=Id}) ->
    render_cid_uri(Id).

render_attribute(#occi_attr{value=undefined}, Acc, _Env) ->
    Acc;
render_attribute(#occi_attr{}=Attr, Acc, Env) ->
    Bin = build_attribute(Attr, Env),
    add_header_value(<<"x-occi-attribute">>, Bin, Acc).

render_kv(_Key, undefined, _) ->
    [];
render_kv(_Key, <<>>, _) ->
    [];
render_kv(_Key, [], _) ->
    [];
render_kv(Key, Value, Env) ->
    [Key, "=", format_value(Value, Env)].

format_value(#uri{}=V, _) ->
    ["\"", occi_uri:to_iolist(V), "\""];
format_value(V, _) when is_atom(V) ->
    io_lib:format("\"~s\"", [V]);
format_value(V, _) when is_integer(V) ->
    io_lib:format("~b", [V]);
format_value(V, _) when is_float(V) ->
    io_lib:format("~f", [V]);
format_value(#uri{}=U, Env) ->
    ["\"", occi_uri:to_iolist(U, Env), "\""];
format_value(V, _) ->
	try ["\"", io_lib:format("~s", [V]), "\""]
	catch _:badarg ->
			["\"", io_lib:format("~p", [V]), "\""]
	end.

build_inline_link(Id, #occi_link{}=Link, Env) ->
    L = [ [ "<", occi_uri:to_iolist(occi_link:get_target(Link), Env), ">" ],
          render_kv(<<"rel">>, render_cid_uri(occi_link:target_cid(Link)), Env),
          case Id of
              #uri{} -> render_kv("self", occi_uri:to_iolist(Id, Env), Env);
              _ -> []
          end,
          render_kv("category", render_cid_uri(occi_link:get_cid(Link)), Env)],
    L2 = lists:foldl(fun (Attr, Acc) ->
							 case occi_attribute:id(Attr) of
								 'occi.core.source' -> 
								 	 Acc;
								 'occi.core.target' -> 
								 	 Acc;
								 _ ->
									 Acc ++ [ build_attribute(Attr, Env) ]
							 end
                     end, L, occi_link:get_attributes(Link)),
    occi_renderer:join(L2, "; ").

build_attribute(#occi_attr{}=Attr, Env) ->
    Name = to_list(occi_attribute:get_id(Attr)),
    render_kv(Name, occi_attribute:get_value(Attr), Env).

build_cid(#occi_cid{term=Term, scheme=Scheme, class=Cls}, Env) ->
    occi_renderer:join(
      [to_list(Term),
       render_kv(<<"scheme">>, to_list(Scheme), Env),
       render_kv(<<"class">>, to_list(Cls), Env)], "; ").

add_header_value(Name, Value, Acc) ->
    %% check_iolist(Value),
    Values = case orddict:find(Name, Acc) of
                 {ok, V} -> V;
                 error -> []
             end,
    orddict:store(Name, [Value | Values], Acc).

to_list(#uri{}=V) ->
    occi_uri:to_string(V);
to_list(V) when is_list(V) ->
    V;
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) when is_binary(V) ->
    binary_to_list(V).

%% check_iolist([]) ->
%%  ok;
%% check_iolist([ H | T ]) when is_list(H) ->
%%  check_iolist(H),
%%  check_iolist(T);
%% check_iolist([ H | T ]) when is_binary(H) ->
%%  check_iolist(T);
%% check_iolist([ H | T ]) when is_integer(H) ->
%%  check_iolist(T);
%% check_iolist([ H | T]) ->
%%  throw({invalid_iolist, H, T}).
