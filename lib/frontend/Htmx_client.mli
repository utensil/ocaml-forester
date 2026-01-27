(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler
module T := Types

(* type query = { *)
(*   query : (string, T.content T.vertex) Forester_core.Datalog_expr.query; *)
(* } *)
(* val query_t : query Repr.ty *)
(* val local_path_components : Forester_core.URI.t -> string list *)
(* val route : *)
(*   State.t -> Forester_core.URI.t -> Forester_core.URI.t *)
(* val title_flags_to_http_header : *)
(*   T.title_flags -> [> `Assoc of (string * [> `String of string ]) list ] *)
(* val section_flags_to_http_header : *)
(*   T.section_flags -> [> `Assoc of (string * [> `String of string ]) list ] *)
(* val content_target_to_http_header : *)
(*   T.content_target -> [> `Assoc of (string * [> `String of string ]) list ] *)
(* val render_xml_qname : Forester_xml_names.xml_qname -> string *)
(* val render_xml_attr : T.content T.xml_attr -> Pure_html.attr *)
(* val render_xmlns_prefix : Forester_xml_names.xmlns_attr -> Pure_html.attr *)
(* type toc_config = { *)
(*   suffix : string; *)
(*   taxon : string; *)
(*   number : string; *)
(*   fallback_number : string; *)
(*   in_backmatter : bool; *)
(*   is_root : bool; *)
(*   implicitly_unnumbered : bool; *)
(* } *)
(* val default_toc_config : *)
(*   ?suffix:string -> *)
(*   ?taxon:string -> *)
(*   ?number:string -> *)
(*   ?fallback_number:string -> ?in_backmatter:bool -> unit -> toc_config *)
val render_article :
  forest:State.t-> T.content T.article -> Pure_html.node
(* val render_section : *)
(*   env:Html_client.env -> T.content T.section -> Pure_html.node *)
(* val render_backmatter : *)
(*   env:Html_client.env -> T.content -> Pure_html.node list *)
(* val render_frontmatter : *)
(*   env:Html_client.env -> T.content T.frontmatter -> Pure_html.node *)
(* val render_transclusion : T.transclusion -> Pure_html.node list *)
val render_content : env:Html_client.env -> T.content -> Pure_html.node list
(* val render_content_node : *)
(*   env:Html_client.env -> T.content T.content_node -> Pure_html.node list *)
(* val render_link : *)
(*   env:Html_client.env -> T.content T.link -> Pure_html.node list *)
(* val contextual_number : T.content T.section -> toc_config -> Pure_html.node *)
(* val _tree_taxon_with_number : *)
(*   T.content T.section -> toc_config -> Pure_html.node *)
(* val _render_toc_item : *)
(*   env:Html_client.env -> T.content T.section -> Pure_html.node *)
val render_toc_mainmatter : T.content -> Pure_html.node
(* val render_toc : T.content T.section -> Pure_html.node *)
val render_query_result :
  forest:State.t -> Forester_core.Vertex_set.t -> Pure_html.node option

