(*
  This module defines common events of native element, and base event module for custom element.
*)

(* define types and function for event listeners for Native element. *)
type t

type listener = Dom.event -> unit

external make:
  ?click: listener ->
  ?input: listener ->
  ?change: listener ->
  ?mouseup: listener ->
  ?mousedown: listener ->
  ?mousemove: listener ->
  unit -> t = "" [@@bs.obj]

(* Event type module for functor *)
module Custom = struct
  module type E = sig
    type t

    val to_string: t -> string
  end

  (* Base signature of custom event *)
  module type S = sig
    type t
    type 'a listener = ('a -> t -> unit)

    val to_string: t -> string
    val make_listener: 'a listener -> ('a -> t -> unit [@bs.this])
  end

  (* Functor to make new event for the component. Using custom event with component should
     combine some custom event to one module, and define object builder via external with [@bs.obj].
     Example.

  module Custom_events = struct
    (* Define custom event *)
    module E1 = Make(struct
                    type t = string
                    let to_string v = v
                  end )

    (* Integrate all events that are able to emit from the component *)
    type t = E1 of E1.t

    (* Define external function to build 'on' object *)
    external make: ?e1:'a E1.listener ->
                   unit -> t = "" [@@bs.obj]

    (* Optional: define event emitter with component's emitter for convinient  *)
    let emit emitter = function
      | E1 v -> emitter (E1.to_string v) v
  end

   *)
  module Make(Ev:E) : S with type t = Ev.t = struct
    type t = Ev.t
    type 'a listener = ('a -> Ev.t -> unit)

    let to_string = Ev.to_string
    let make_listener f = fun [@bs.this] this v -> f this v
  end
end
