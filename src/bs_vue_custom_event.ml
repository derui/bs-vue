module V = Bs_vue_vue

module type E = sig
  type arg
  type component_event

  (* name of this event, this should be unique in the custom component *)
  val name: string
end

(* Base signature of custom event *)
module type S = sig
  type arg
  type component_event
  type t = T of arg
  type ('p, 'd) instance = ('p, component_event, 'd) V.Vue_instance.t
  type ('p, 'd) listener = (('p, 'd) instance -> arg -> unit [@bs.this])

  val name: string
  val emit: arg -> ('p, 'd) instance -> unit
end

(* A simple functor to create custom event type module. *)
module Make(E:E) : S with type arg = E.arg
                      and type component_event := E.component_event = struct
  type arg = E.arg
  type t = T of arg
  type ('p, 'd) instance = ('p, E.component_event, 'd) V.Vue_instance.t
  type ('p, 'd) listener = (('p, 'd) instance -> arg -> unit [@bs.this])

  let name = E.name
  let emit arg instance =
    let module I = V.Vue_instance in I.emit name arg instance |> ignore

end
