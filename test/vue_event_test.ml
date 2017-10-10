open Bs_testing

module D = Bs_webapi.Dom
module V = Bs_vue.Vue
module O = Bs_vue.Options

let custom_event_suite () =
  let module R = V.Render_context in
  let module I = V.Vue_instance in
  let module O = Bs_vue.Options in
  let module C = struct
    type event
    module CE = struct
      module E1 = Bs_vue.Event.Custom.Make(struct type arg = string end)

      type emitter = [`test of E1.arg]

      external make:
        ?test:('prop, event, 'data) I.t E1.listener ->
        unit -> event = "" [@@bs.obj]

    end

    let emit ev =
      match ev with
      | `test v -> I.emit "test" v

    type component = (unit, event, unit) V.component
    let component : component =
      V.component ~render:(fun context ->
          R.context context |> emit (`test "foo") |> ignore;
          R.create_element ~context ~tag:"div" ~elements:[|V.text "bar"|] ()
        ) ()
  end in

  suite "Custom event" [
    Async ("should be able to call custom event",
           fun () ->
             Js.Promise.make (fun ~resolve ~reject:_ ->
                 let component =
                   V.component ~render:(fun context ->
                       let events =
                         C.CE.make
                           ~test:(fun [@bs.this] _ v -> resolve (assert_eq "foo" v) [@bs] |> ignore)
                           ()
                       in
                       R.create_component ~context ~component:C.component
                         ~options:(O.VNode_data.make ~on:events ())
                         ()
                     ) () in
                 (V.create ~component ()
                  |> I.mount
                  |> ignore);
                 V.Vue.nextTick (fun [@bs] () -> () ) V.vue
               )
          );
  ]

let _ =
  let module R = V.Render_context in
  let module I = V.Vue_instance in
  let module O = Bs_vue.Options in
  suite "Native element" [
    Async ("should call native event",
           fun () ->
             Js.Promise.make (fun ~resolve ~reject:_ ->
                 let component = V.component ~render:(fun context ->
                     let events =
                       Bs_vue.Event.make
                         ~click:(fun _ -> resolve (assert_ok true) [@bs] |> ignore)
                         ()
                     in
                     R.create_element ~context ~tag:"div"
                       ~options:(O.VNode_element_data.make ~on:events ()) ()
                   ) () in
                 (V.create ~component ()
                  |> I.mount
                  |> I.el |> Bs_webapi.Dom.HtmlElement.click)
               )
          );
  ];

  custom_event_suite ()
