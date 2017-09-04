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
var _Vue = require('vue').default;

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

module Options = Bs_vue_options
               
type element
type ('props, 'events, 'data) component
type ('props, 'data) data_fn = 'props -> 'data

(* Type for instance of Vue component created via Vue.component *)
type vue
module Vue = struct

  external el: element -> Dom.htmlElement = "$el" [@@bs.get]
  external parent: element -> element option = "$parent" [@@bs.send.pipe:vue]
  external root: element -> element = "$root" [@@bs.get]
  external children: vue -> element array = "$children" [@@bs.get]
  external nextTick: (unit -> unit [@bs]) -> unit = "" [@@bs.send.pipe:vue]
end

(* The module for extened Vue component created via Vue.extend *)
module Vue_instance = struct

  external mount: element -> element = "$mount" [@@bs.send]
  external el: element ->  Dom.htmlElement = "$el" [@@bs.get]
  external parent: element -> element option = "$parent" [@@bs.send.pipe:vue]
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
                             ('p, 'e) Options.VNode_data.t -> element array -> element = "createElement" [@@bs.send]
  external create_element: ('props, 'events, 'data) t ->
                           string -> Options.VNode_element_data.t -> element array -> element = "createElement" [@@bs.send]
end

(* make configuration object for component created from createComponent_ function *)
type ('prop, 'event, 'data) render_fn = ('prop, 'event, 'data) Render_context.t -> element

external vue : vue = "_Vue" [@@bs.val]
external createComponent_ : ('props, 'events, 'data) render_fn ->
                            ('props, 'events, 'data) Options.Component_options.t ->
                            ('props, 'events, 'data) component = "_createComponent" [@@bs.val]
(* Create component instance via new syntax *)
external newComponent_: ('prop, 'event, 'data) component ->
                 ('prop, 'event, 'data) Options.Component_options.t option ->
                 element = "_newComponent" [@@bs.val]

(* Needed so that we include strings and elements as children *)
external text : string -> element = "%identity"

(*
 * We have to do this indirection so that BS exports them and can re-import them
 * as known symbols. This is less than ideal.
 *)
let component = createComponent_
let create = newComponent_
