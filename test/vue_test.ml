open Bs_testing
module V = Bs_vue_vue

let _ =
  let module R = V.Render_context in
  let module I = V.Vue_instance in
  let module C = V.Render_context in
  suite "Native element" [
      Sync ("create simple element",
            fun () ->
            let e = V.component (fun ctx ->
                        R.create_element ctx "div" (V.VNode_element_data.make ()) [||]
                      ) (V.Component_options.make ()) in
            assert_eq "div" (V.create e (V.Component_options.make ())
                             |> I.mount
                             |> I.el |> Bs_webapi.Dom.HtmlElement.tagName
                             |> String.lowercase)
        );

      Sync ("should be able to contains some element",
            fun () ->
            let e = V.component (fun ctx ->
                        R.create_element ctx "div" (V.VNode_element_data.make ()) [|
                            R.create_element ctx "span" (V.VNode_element_data.make ()) [|V.text "bar"|]
                          |]
                      ) (V.Component_options.make ()) in
            assert_eq 1 (V.create e (V.Component_options.make ())
                         |> I.mount
                         |> I.children |> Array.length)
        );
      Sync ("should be able to contains text element",
            fun () ->
            let e = V.component (fun ctx ->
                        R.create_element ctx "div" (V.VNode_element_data.make ()) [|
                            V.text "foo"
                          |]
                      ) (V.Component_options.make ()) in
            assert_eq "foo" (V.create e (V.Component_options.make ())
                             |> I.mount |> I.el
                             |> Bs_webapi.Dom.HtmlElement.textContent)
        )
    ]
