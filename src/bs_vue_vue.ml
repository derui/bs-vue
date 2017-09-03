(*
  Very thin Vue2.x wrapper.

  Usage:
   - create element
     element "tag" any [||]
   - create component
     let component = create_component (fun p s _ -> text "foo") iniital_state in
     React.component component prop [||]

   - You can use pre-defined elements: div, span, a, input ...
*)
[%%bs.raw{|
var _Vue = require('vue');

function _createComponent (fn, options) {
  return _Vue.extend({
          render: function (createElement) {
            return fn({createElement: createElement,
                       context: this});
          },
          props: options.props,
          data: function() {
            if (!options.data) {
              return {};
            }

            return options.data(this.$props);
          }
  });
}

function _newComponent(component, option) {
  return new component(option);
}
|}]

type t

type element
type ('props, 'events, 'data) component
type ('props, 'data) data_fn = 'props -> 'data

type key_type = [ `KeyString of string
                | `KeyNumber of int]

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

  external key: ('props, 'events) t -> key_type option = "" [@@bs.get] [@@bs.return nullable]
  external slot: ('props, 'events) t -> string option = "" [@@bs.get] [@@bs.return nullable]
  external _ref: ('props, 'events) t -> string option = "" [@@bs.get] [@@bs.return nullable]
  external tag: ('props, 'events) t -> string option = "" [@@bs.get] [@@bs.return nullable]
  external staticClass: ('props, 'events) t -> string option = "" [@@bs.get] [@@bs.return nullable]
  external props: ('props, 'events) t -> 'props option = "" [@@bs.get] [@@bs.return nullable]
  external show_: ('props, 'events) t -> Js.boolean option = "show" [@@bs.get] [@@bs.return nullable]
  let show t =
    let b = show_ t in
    match b with
    | None -> false
    | Some b -> Js.to_bool b
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

(* Type for instance of Vue component created via Vue.component *)
module Vue = struct

  external el: element -> Dom.htmlElement = "$el" [@@bs.get]
  external parent: element -> element option = "$parent" [@@bs.send.pipe:t]
  external root: element -> element = "$root" [@@bs.get]
  external children: t -> element array = "$children" [@@bs.get]
end

(* The module for extened Vue component created via Vue.extend *)
module Vue_instance = struct

  external mount: element -> element = "$mount" [@@bs.send]
  external el: element ->  Dom.htmlElement = "$el" [@@bs.get]
  external parent: element -> element option = "$parent" [@@bs.send.pipe:t]
  external root: element -> element = "$root" [@@bs.get]
  external children: element -> element array = "$children" [@@bs.get]

  external props: element -> (('prop, 'event, 'data) component [@bs.ignore]) ->
                  'prop = "$props" [@@bs.get]
  external data: element -> (('prop, 'event, 'data) component [@bs.ignore]) ->
                 'data = "$data" [@@bs.get]
  external set_data: element -> (('prop, 'event, 'data) component [@bs.ignore]) ->
                     'data -> unit = "$data" [@@bs.set]
end

(* A context received when render function is called from Vue *)
module Render_context = struct
  type ('props, 'events, 'data) t

  external context: ('props, 'events, 'data) t -> ('props, 'events, 'data) component = "" [@@bs.get]
  external create_component: ('props, 'events, 'data) t ->
                             ('p, 'e, 'd) component ->
                             ('p, 'e) VNode_data.t -> element array -> element = "createElement" [@@bs.send]
  external create_element: ('props, 'events, 'data) t ->
                           string -> VNode_element_data.t -> element array -> element = "createElement" [@@bs.send]
end

(* make configuration object for component created from createComponent_ function *)
type ('prop, 'event, 'data) render_fn = ('prop, 'event, 'data) Render_context.t -> element

external vue : t = "_Vue" [@@bs.val]
external createComponent_ : ('props, 'events, 'data) render_fn ->
                            ('props, 'events, 'data) Component_options.t ->
                            ('props, 'events, 'data) component = "_createComponent" [@@bs.val]
(* Create component instance via new syntax *)
external newComponent_: ('prop, 'event, 'data) component ->
                 ('prop, 'event, 'data) Component_options.t ->
                 element = "_newComponent" [@@bs.val]

(* Needed so that we include strings and elements as children *)
external text : string -> element = "%identity"

(*
 * We have to do this indirection so that BS exports them and can re-import them
 * as known symbols. This is less than ideal.
 *)
let component = createComponent_
let create = newComponent_