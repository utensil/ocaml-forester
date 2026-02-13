(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

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
