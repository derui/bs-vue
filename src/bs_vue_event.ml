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
    type arg
  end

  (* Base signature of custom event *)
  module type S = sig
    type arg
    type t = T of arg
    type 'a listener = ('a -> arg -> unit [@bs.this])
  end

  (* A simple functor to create custom event type module. *)
  module Make(E:E) : S with type arg = E.arg = struct
    type arg = E.arg
    type t = T of arg
    type 'a listener = ('a -> arg -> unit [@bs.this])
  end

end
