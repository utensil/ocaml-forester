(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Brr

module Tree = struct
  type t = {
    metas: Jsont.json Map.Make(String).t;
    route: string;
    tags: string list;
    taxon: string option;
    title: string;
    uri: string;
  }

  let make metas route tags taxon title uri =
    {metas; route; tags; taxon; title; uri}

  let metas t = t.metas
  let route t = t.route
  let tags t = t.tags
  let taxon t = t.taxon
  let title t = t.title
  let uri t = t.uri

  let string_null_is_none =
    let null = Jsont.null None in
    let enc = function None -> null | _ -> Jsont.(option string) in
    Jsont.any ~dec_null:null ~dec_string:Jsont.(option string) ~enc ()

  let jsont =
    Jsont.(
      Object.(
        map ~kind:"tree" make
        |> mem "metas" (as_string_map ~kind:"metas" json) ~enc:metas
        |> mem "route" string ~enc:route
        |> mem "tags" (list string) ~enc:tags
        |> mem "taxon" string_null_is_none ~enc:taxon
        |> mem "title" string ~enc:title
        |> mem "uri" string ~enc:uri |> finish))

  let forest_jsont = Jsont.list jsont
end

type t = {
  results_list: El.t;
  root: El.t;
  input: El.t;
  mutable visible: bool;
  mutable results: Tree.t list;
  mutable selected_idx: int;
}

let on_select item =
  Console.log [Jstr.v ("Selected: " ^ item.Tree.uri)];
  ignore
  @@ Window.open' ~name:(Jstr.v "_self") G.window (Jstr.v item.Tree.route)

let render_results t ~on_select =
  El.set_children t.results_list [];
  match t.results with
  | [] ->
    let empty =
      El.div
        ~at:[At.class' @@ Jstr.v "search-modal-empty"]
        [El.txt' "No results found"]
    in
    El.append_children t.results_list [empty]
  | items ->
    let item_els =
      List.mapi
        (fun idx item ->
          let li = El.li [El.txt' item.Tree.title] in
          El.set_class (Jstr.v "search-modal-result") true li;
          if idx = t.selected_idx then El.set_class (Jstr.v "selected") true li;
          ignore
            (Ev.listen Ev.click (fun _ -> on_select item) (El.as_target li));
          li)
        items
    in
    List.iter (El.append_children t.results_list) [item_els]

let stylesheet () =
  let css =
    {|
.search-modal-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: flex-start;
  justify-content: center;
  padding-top: 100px;
  z-index: 9999;
  transition: opacity 0.2s;
}

.search-modal-overlay.hidden {
  opacity: 0;
  pointer-events: none;
}

.search-modal-container {
  background: white;
  border-radius: 12px;
  box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);
  width: min(600px, 90vw);
  max-height: 70vh;
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.search-modal-input-wrapper {
  padding: 16px;
  border-bottom: 1px solid #e5e7eb;
  display: flex;
  align-items: center;
  gap: 10px;
}

.search-modal-input-wrapper::before {
  content: "🔍";
  font-size: 18px;
}

.search-modal-input {
  flex: 1;
  padding: 8px 12px;
  font-size: 16px;
  border: none;
  outline: none;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
}

.search-modal-input::placeholder {
  color: #9ca3af;
}

.search-modal-results {
  flex: 1;
  overflow-y: auto;
  list-style: none;
  margin: 0;
  padding: 0;
}

.search-modal-result {
  padding: 12px 16px;
  cursor: pointer;
  border-bottom: 1px solid #f3f4f6;
  transition: background-color 0.15s;
  color: #1f2937;
  font-size: 14px;
}

.search-modal-result:hover {
  background-color: #f9fafb;
}

.search-modal-result.selected {
  background-color: #3b82f6;
  color: white;
}

.search-modal-empty {
  padding: 32px 16px;
  text-align: center;
  color: #9ca3af;
  font-size: 14px;
}
|}
  in
  let style_el = El.style [El.txt' css] in
  ignore (El.append_children (Document.root G.document) [style_el])

let element_contains parent child =
  Jv.to_bool (Jv.call (El.to_jv parent) "contains" [|El.to_jv child|])

external as_element : Ev.target -> El.t = "%identity"

let create ~on_search ~on_select ?(placeholder = "Search...") ~init () : t =
  stylesheet ();
  let input =
    El.input
      ~at:
        [
          At.type' (Jstr.v "search");
          At.placeholder (Jstr.v placeholder);
          At.class' (Jstr.v "search-modal-input");
        ]
      ()
  in
  let input_wrapper =
    El.div ~at:[At.class' @@ Jstr.v "search-modal-input-wrapper"] [input]
  in
  let results_list =
    El.ul ~at:[At.class' @@ Jstr.v "search-modal-results"] []
  in
  let container =
    El.div
      ~at:[At.class' @@ Jstr.v "search-modal-container"]
      [input_wrapper; results_list]
  in
  let overlay =
    El.div ~at:[At.class' @@ Jstr.v "search-modal-overlay hidden"] [container]
  in
  El.append_children (Document.root G.document) [overlay];
  let t =
    {
      input;
      root = overlay;
      results_list;
      results = init;
      selected_idx = 0;
      visible = false;
    }
  in
  ignore
    (Ev.listen Ev.keydown
       (fun _ ->
         let query = Jstr.to_string @@ El.prop El.Prop.value t.input in
         on_search query;
         render_results t ~on_select)
       (El.as_target input));
  ignore
    Ev.(
      listen keydown
        (fun e ->
          let ev = Ev.as_type e in
          let key = Jstr.to_string @@ Keyboard.key ev in
          match key with
          | "ArrowDown" ->
            Ev.prevent_default e;
            let next_idx =
              if t.selected_idx + 1 > List.length t.results then 0
              else t.selected_idx + 1
            in
            t.selected_idx <- next_idx;
            render_results ~on_select t
          | "ArrowUp" ->
            Ev.prevent_default e;
            let next_idx =
              if t.selected_idx < 0 then List.length t.results - 1
              else t.selected_idx - 1
            in
            t.selected_idx <- next_idx;
            render_results ~on_select t
          | "Enter" ->
            Ev.prevent_default e;
            let item = List.nth t.results t.selected_idx in
            on_select item;
            El.set_class (Jstr.v "hidden") true t.root;
            t.visible <- false
          | "Escape" ->
            Ev.prevent_default e;
            El.set_class (Jstr.v "hidden") true t.root;
            t.visible <- false
          | _ -> ())
        (El.as_target t.input));
  ignore
    Ev.(
      listen click
        (fun ev ->
          let target = as_element @@ Ev.target ev in
          let is_inside_container =
            element_contains container target
            || Jv.strict_equal (El.to_jv target) (El.to_jv container)
          in
          if (not is_inside_container) && t.visible then (
            El.set_class (Jstr.v "hidden") true t.root;
            t.visible <- false);
          ())
        (El.as_target overlay));
  render_results t ~on_select;
  t

let set_results t items =
  t.results <- items;
  render_results t ~on_select

let open_modal t =
  El.set_class (Jstr.v "hidden") false t.root;
  El.set_has_focus true t.input;
  t.visible <- true

let close_modal t =
  El.set_class (Jstr.v "hidden") true t.root;
  t.visible <- false;
  t.results <- [];
  t.selected_idx <- 0

let listen_to_hotkey modal =
  ignore
  @@ Ev.listen Ev.keydown
       (fun e ->
         let ev = Ev.as_type e in
         Ev.(
           if Keyboard.ctrl_key ev && Keyboard.key ev = Jstr.v "k" then begin
             prevent_default e;
             open_modal modal
           end))
       (Document.as_target G.document)

let listen_to_escape modal =
  ignore
  @@ Ev.listen Ev.keydown
       (fun e ->
         let ev = Ev.as_type e in
         Ev.(
           if Keyboard.key ev = Jstr.v "Escape" then begin
             Console.log ["Escape"];
             prevent_default e;
             close_modal modal
           end))
       (Document.as_target G.document)

let () =
  let modal = ref None in
  let forest = ref None in

  ignore
    begin
      let open Brr_io.Fetch in
      let req = Request.(v ~init:(init ()) (Jstr.v "/forest.json")) in
      Fut.await (request req) (fun res ->
          match res with
          | Ok res -> begin
            Fut.await
              (Body.json @@ Response.as_body res)
              (function
                | Ok json -> begin
                  match Jsont_brr.decode_jv Tree.forest_jsont json with
                  | Ok trees -> forest := Some trees
                  | Error err -> Console.error [Jv.Error.message err]
                end
                | Error err -> Console.error [Jv.Error.message err])
          end
          | Error err -> Console.error [Jv.Error.message err])
    end;

  let on_search query =
    Brr.Console.log [Jstr.v ("Searching: " ^ query)];

    let results =
      match !forest with
      | Some trees ->
        List.filter
          (fun Tree.{title; _} ->
            let re = Regexp.create ~flags:(Jstr.v "gi") (Jstr.v query) in
            Option.is_some @@ Regexp.match' re (Jstr.v title))
          trees
      | None -> []
    in

    match !modal with None -> () | Some m -> set_results m results
  in

  let m =
    create ~placeholder:"Search trees..." ~on_search ~on_select
      ~init:(Option.value ~default:[] !forest)
      ()
  in
  modal := Some m;
  listen_to_hotkey m;
  listen_to_escape m;
  ()
