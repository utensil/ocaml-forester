(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_app
open Brr

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

  let on_select item =
    Console.log [Jstr.v ("Selected: " ^ item.Tree.uri)];
    ignore
    @@ Window.open' ~name:(Jstr.v "_self") G.window (Jstr.v item.Tree.route)
  in

  let on_search query =
    Brr.Console.log [Jstr.v ("Searching: " ^ query)];
    let query = Jstr.lowercased (Jstr.v query) in

    let results =
      match !forest with
      | Some trees ->
        List.filter
          (fun Tree.{title; _} ->
            Option.is_some
            @@ Jstr.find_sub ~sub:query (Jstr.lowercased (Jstr.v title)))
          trees
      | None -> []
    in

    match !modal with
    | None -> ()
    | Some m -> Search_modal.set_results ~on_select m results
  in

  let m =
    Search_modal.create ~placeholder:"Search trees..." ~on_search ~on_select
      ~init:(Option.value ~default:[] !forest)
      ()
  in
  Search_modal.register_hotkeys m;
  modal := Some m;
  ignore @@ Jump_to_subtree.init ();
  ignore @@ Open_in_editor.init ()
