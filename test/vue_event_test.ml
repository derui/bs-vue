open Bs_testing

module D = Bs_webapi.Dom
module V = Bs_vue.Vue
module O = Bs_vue.Options

let _ =
  let module R = V.Render_context in
  let module I = V.Vue_instance in
  let module O = Bs_vue.Options in
  suite "Native element" [
      Async ("should call native event",
             fun () ->

             Js.Promise.make (fun ~resolve ~reject:_ ->
                let e = V.component (fun ctx ->
                        let events = Bs_vue.Event.make ~click:(fun _ -> resolve (assert_ok true) [@bs] |> ignore) () in 
                        R.create_element ctx "div" (O.VNode_element_data.make ~on:events ()) [||]
                    ) (O.Component_options.make ()) in
                (V.create e (O.Component_options.make ())
                            |> I.mount
                            |> I.el |> Bs_webapi.Dom.HtmlElement.click)
            )
    );
];
