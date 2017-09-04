(*
  This module defines option and data modules as argument of funciton
 *)
type key_type = [ `KeyString of string
                | `KeyNumber of int]

module Prop_type = struct
end

(* Type for VNodeData that is used by Component *)
module VNode_data = struct
  type ('prop, 'event) t

  external make:
    ?key: key_type ->
    ?slot: string ->
    ?ref: string ->
    ?tag: string ->
    ?staticClass: string ->
    ?_class: _ Js.t ->
    ?style: _ Js.t ->
    ?props: 'prop ->
    ?attrs: _ Js.t ->
    ?domProps: _ Js.t ->
    ?on: 'event Js.t ->
    ?keepAlive: Js.boolean ->
    ?show: Js.boolean ->
    unit -> ('prop, 'event) t = "" [@@bs.obj]
end

(* Type for VNodeData that is used by Element *)
module VNode_element_data = struct
  type t = (unit, unit) VNode_data.t

  external make:
    ?key: key_type ->
    ?slot: string ->
    ?ref: string ->
    ?tag: string ->
    ?staticClass: string ->
    ?_class: _ Js.t ->
    ?style: _ Js.t ->
    ?attrs: _ Js.t ->
    ?domProps: _ Js.t ->
    ?on: Bs_vue_event.t Js.t ->
    ?keepAlive: Js.boolean ->
    ?show: Js.boolean ->
    unit -> t = "" [@@bs.obj]

end

module Component_options = struct
  type ('prop, 'events, 'data) t

  external make:
    ?data: (('prop, 'events, 'data) t -> 'data [@bs.this]) ->
    ?props: 'prop ->
    ?methods: _ Js.t ->
    ?name: string ->
    unit -> ('prop, 'events, 'data) t = "" [@@bs.obj]

end
