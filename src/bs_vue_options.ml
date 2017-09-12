(*
  This module defines option and data modules as argument of funciton
 *)
type key_type = [ `KeyString of string
                | `KeyNumber of int]

type 'a props

module Prop_option = struct
  type 'a t
  type 'a validator = 'a -> bool

  external make: ?required: Js.boolean ->
                 ?default: 'a ->
                 ?validator: 'a validator ->
                 unit -> 'a t = "" [@@bs.obj]

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
    ?on: 'event ->
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
    ?on: Bs_vue_event.t ->
    ?keepAlive: Js.boolean ->
    ?show: Js.boolean ->
    unit -> t = "" [@@bs.obj]

end

module Component_options = struct
  type ('prop, 'events, 'data) t
  type 'prop props_type_gen = unit -> 'prop props

  external make:
    ?data: (('prop, 'events, 'data) t -> 'data [@bs.this]) ->
    ?props: 'prop props ->
    ?propsData: 'prop ->
    ?methods: 'a ->
    ?name: string ->
    unit -> ('prop, 'events, 'data) t = "" [@@bs.obj]

end
