open Brr

external as_element : Ev.target -> El.t = "%identity"

let rec open_all_details_above elt =
  if Jv.get elt "nodeName" = Jv.of_string "DETAILS" then
    Jv.set elt "open" Jv.true';
  match El.parent (El.of_jv elt) with
  | None -> ()
  | Some elt' -> open_all_details_above (El.to_jv elt')

let jump_to_subtree evt =
  if El.tag_name @@ as_element @@ Ev.target evt = Jstr.v "A" then ()
  else
    let link =
      Jv.call
        (Jv.repr @@ Ev.target evt)
        "closest"
        [|Jv.of_string "span[data-target]"|]
    in
    let selector = Jv.call link "getAttribute" [|Jv.of_string "data-target"|] in
    let tree = Jv.call (Jv.repr G.document) "querySelector" [|selector|] in
    open_all_details_above tree;
    Window.set_location G.window (Uri.of_jv selector)

let add_listeners () =
  El.fold_find_by_selector
    (fun el () ->
      let unlisten =
        Ev.listen Ev.click (fun ev -> jump_to_subtree ev) (El.as_target el)
      in
      ())
    (Jstr.v "[data-target^='#']")
    ()

let init () =
  Ev.(listen load (fun ev -> add_listeners ()) (Window.as_target G.window))
