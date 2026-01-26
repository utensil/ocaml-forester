open Types

module Prefix_map = Map.Make (String)
type t = string Prefix_map.t

let init ~(reserved : xmlns_attr list) =
  List.fold_left
    (fun env (attr : xmlns_attr) -> Prefix_map.add attr.prefix attr.xmlns env)
    Prefix_map.empty reserved

let xmlns_attr_of_qname (q : xml_qname) : xmlns_attr option =
  match q.xmlns with
  | None -> None
  | Some xmlns -> Some {prefix = q.prefix; xmlns}

let xmlns_attr_is_new (attr : xmlns_attr) env : bool =
  match Prefix_map.find_opt attr.prefix env with
  | None -> true
  | Some uri' -> uri' <> attr.xmlns

let extend (bindings : xmlns_attr list) env : t =
  List.fold_left
    (fun env (attr : xmlns_attr) -> Prefix_map.add attr.prefix attr.xmlns env)
    env bindings

let xmlns_attrs_for_elt (elt : 'a xml_elt) env : xmlns_attr list =
  let from_name =
    match xmlns_attr_of_qname elt.name with None -> [] | Some b -> [b]
  in
  let from_attrs =
    elt.attrs |> List.filter_map @@ fun attr -> xmlns_attr_of_qname attr.key
  in
  List.filter (fun attr -> xmlns_attr_is_new attr env) @@ from_name @ from_attrs
