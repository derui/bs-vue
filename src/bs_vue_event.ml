(*
  This module defines common events of native element.
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
