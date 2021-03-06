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
const _Vue = require('vue').default;

function _createComponent (fn, options) {
  const extendedOption = Object.assign({}, options, {
    render: function (createElement) {
      return fn({createElement: createElement,
                 context: this});
    }
  });
  return _Vue.extend(extendedOption);
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
  type ('props, 'events, 'data) t

  external mount: ('props, 'events, 'data) t -> ('props, 'events, 'data) t = "$mount" [@@bs.send]
  external el: ('props, 'events, 'data) t ->  Dom.htmlElement = "$el" [@@bs.get]
  external parent: ('props, 'events, 'data) t -> element option = "$parent" [@@bs.send.pipe:vue]
  external root: ('props, 'events, 'data) t -> element = "$root" [@@bs.get]
  external children: ('props, 'events, 'data) t -> element array = "$children" [@@bs.get]

  external props: ('props, 'events, 'data) t -> 'props = "$props" [@@bs.get]
  external data: ('props, 'events, 'data) t -> 'data = "$data" [@@bs.get]
  external set_data: ('props, 'events, 'data) t -> 'data -> unit = "$data" [@@bs.set]
  external emit: string -> 'a -> ('props, 'events, 'data) t = "$emit" [@@bs.send.pipe:('props, 'events, 'data) t]
end

(* A context received when render function is called from Vue *)
module Render_context = struct
  type ('props, 'events, 'data) t

  external context: ('props, 'events, 'data) t -> ('props, 'events, 'data) Vue_instance.t = "" [@@bs.get]
  external createComponent_: ('props, 'events, 'data) t ->
    ('p, 'e, 'd) component ->
    ('p, 'e) Options.VNode_data.t -> element array -> element = "createElement" [@@bs.send]
  external createElement_: ('props, 'events, 'data) t ->
    string -> Options.VNode_element_data.t -> element array -> element = "createElement" [@@bs.send]

  let create_component ~context ~component ?options ?elements () =
    let options = Js_option.default (Options.VNode_data.make ()) options
    and elements = Js_option.default [||] elements in
    createComponent_ context component options elements

  let create_element ~context ~tag ?options ?elements () =
    let options = Js_option.default (Options.VNode_element_data.make ()) options
    and elements = Js_option.default [||] elements in
    createElement_ context tag options elements

end

(* make configuration object for component created from createComponent_ function *)
type ('prop, 'event, 'data) render_fn = ('prop, 'event, 'data) Render_context.t -> element

external vue : vue = "_Vue" [@@bs.val]
external createComponent_ : ('props, 'events, 'data) render_fn ->
  ('props, 'events, 'data) Options.Component_options.t ->
  ('props, 'events, 'data) component = "_createComponent" [@@bs.val]
(* Create component instance via new syntax *)
external newComponent_: ('prop, 'event, 'data) component ->
  ('prop, 'event, 'data) Options.Component_options.t ->
  ('prop, 'event, 'data) Vue_instance.t  = "_newComponent" [@@bs.val]

(* Needed so that we include strings and elements as children *)
external text : string -> element = "%identity"

(*
 * We have to do this indirection so that BS exports them and can re-import them
 * as known symbols. This is less than ideal.
 *)
let component ~render ?options () =
  let options = match options with
    | None -> Options.Component_options.make ()
    | Some v -> v
  in
  createComponent_ render options

let create ~component ?options () =
  let options = match options with
    | None -> Options.Component_options.make ()
    | Some v -> v
  in
  newComponent_ component options
