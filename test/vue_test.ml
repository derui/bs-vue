open Bs_testing

module D = Bs_webapi.Dom
module V = Bs_vue.Vue
module O = Bs_vue.Options

let custom_component_suite () =
  let module C = struct
    class type _prop =
      object
        method message: string
      end [@bs]
    type prop = _prop Js.t

    external make_props : message:string -> prop = "" [@@bs.obj]
    external make_props_def : message:string O.Prop_option.t -> prop O.props = "" [@@bs.obj]

    let to_props _ = make_props_def ~message:(O.Prop_option.make ~required:(Js.true_) ())

    let component = V.component
        ~render:(fun context ->
            let prop = V.Render_context.context context |> V.Vue_instance.props in
            V.Render_context.create_element ~context ~tag:"div" ~elements:[|V.text prop##message|] ()
          )
        ~options:(O.Component_options.make ~props:(to_props ()) ())
        ()
  end in

  suite "Custom Element" [
    Sync ("create simple custom element",
          fun () ->
            let module I = V.Vue_instance in
            assert_eq "foo" (V.create
                               ~component:C.component
                               ~options:(O.Component_options.make ~propsData:(C.make_props ~message:"foo") ())
                               ()
                             |> I.mount
                             |> I.el |> Bs_webapi.Dom.HtmlElement.textContent
                             |> String.lowercase)
         )
  ]

let _ =
  let module R = V.Render_context in
  let module I = V.Vue_instance in
  let module O = Bs_vue.Options in
  suite "Native element" [
    Sync ("create simple element",
          fun () ->
            let component = V.component
                ~render:(fun context -> R.create_element ~context ~tag:"div" ())
                ()
            in
            assert_eq "div" (V.create ~component ()
                             |> I.mount
                             |> I.el |> Bs_webapi.Dom.HtmlElement.tagName
                             |> String.lowercase)
         );

    Sync ("should be able to contains some element",
          fun () ->
            let component = V.component
                ~render:(fun context ->
                    R.create_element ~context ~tag:"div"  ~elements:[|
                      R.create_element ~context ~tag:"span" ~elements:[|V.text "bar"|] ()
                    |] ()
                  )
                ~options:(O.Component_options.make ()) () in
            assert_eq 1 (V.create ~component ()
                         |> I.mount
                         |> I.el |> D.HtmlElement.childNodes
                         |> D.NodeList.length)
         );
    Sync ("should be able to contains text element",
          fun () ->
            let component = V.component
                ~render:(fun context ->
                    R.create_element ~context ~tag:"div" ~elements:[|
                      V.text "foo"
                    |] ()
                  )
                () in
            assert_eq "foo" (V.create ~component ()
                             |> I.mount |> I.el
                             |> Bs_webapi.Dom.HtmlElement.textContent)
         )
  ];

  custom_component_suite ()
