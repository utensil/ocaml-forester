(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_xml_names

open struct
  module T = Types
  module P = Pure_html
  let render_date = Html_client.render_date
  let render_attributions = Html_client.render_attributions
end

open Pure_html
open HTML

type query = {
  query: (string, T.content T.vertex) Forester_core.Datalog_expr.query;
}
[@@deriving repr]

let local_path_components (uri : URI.t) =
  let host =
    match URI.host uri with Some host -> host | None -> assert false (* TODO*)
  in
  host :: URI.path_components uri

let route (forest : State.t) uri : URI.t =
  let open State.Syntax in
  match forest.={uri} with
  | None -> uri
  | Some _ ->
    let path = "" :: local_path_components uri in
    URI.make ~path ()

let title_flags_to_http_header (flags : T.title_flags) =
  match flags with
  | {empty_when_untitled} ->
    `Assoc
      [("Empty-When-Untitled", `String (Bool.to_string empty_when_untitled))]

(* I am encoding these headers to JSON because that is what HTMX requires, but
   it would be more beautiful if we could directly use the header type*)
let section_flags_to_http_header (flags : T.section_flags) =
  match flags with
  | {
   hidden_when_empty;
   included_in_toc;
   header_shown;
   metadata_shown;
   numbered;
   expanded;
  } ->
    let to_header l t =
      match t with
      | Some v -> Some (l, `String (Bool.to_string v))
      | None -> None
    in
    let headers =
      [
        to_header "Hidden-When-Empty" hidden_when_empty;
        to_header "Included-In-Toc" included_in_toc;
        to_header "Header-Shown" header_shown;
        to_header "Metadata-Shown" metadata_shown;
        to_header "Numbered" numbered;
        to_header "Expanded" expanded;
      ]
    in
    `Assoc (List.filter_map Fun.id headers)

let content_target_to_http_header (target : T.content_target) =
  match target with
  | T.Full flags ->
    let (`Assoc flags) = section_flags_to_http_header flags in
    `Assoc (("Full", `String "true") :: flags)
  | T.Mainmatter -> `Assoc [("Mainmatter", `String "true")]
  | T.Title flags ->
    let (`Assoc flags) = title_flags_to_http_header flags in
    `Assoc (("Title", `String "true") :: flags)
  | T.Taxon -> `Assoc [("Taxon", `String "true")]

let render_xml_qname = function
  | {prefix = ""; uname; _} -> uname
  | {prefix; uname; _} -> Format.sprintf "%s:%s" prefix uname

let render_xml_attr : T.content T.xml_attr -> _ =
 fun T.{key; value = _} -> string_attr (render_xml_qname key) "todo"
(* "%a" render_content value *)

let render_xmlns_prefix ({prefix; xmlns} : xmlns_attr) =
  let attr = match prefix with "" -> "xmlns" | _ -> "xmlns:" ^ prefix in
  string_attr attr "%s" xmlns

(*This type is just temporary until I figure out the logic *)
type toc_config = {
  suffix: string;
  taxon: string;
  number: string;
  fallback_number: string;
  (* In XSL, hese require querying the ancestors. We can't do this here, so we
     explicitly pass these parameters down*)
  in_backmatter: bool;
  is_root: bool;
  implicitly_unnumbered: bool;
}

let default_toc_config ?(suffix = "") ?(taxon = "") ?(number = "")
    ?(fallback_number = "") ?(in_backmatter = false) () =
  {
    suffix;
    taxon;
    number;
    fallback_number;
    in_backmatter;
    is_root = false;
    implicitly_unnumbered = false;
  }

let rec render_article ~(forest : State.t) (article : T.content T.article) : node
    =
  (* FIXME: What should reserved be here? *)

  let reserved = [{prefix = ""; xmlns = "http://www.w3.org/1999/xhtml"}] in
  let env : Html_client.env =
    {
      forest;
      section_depth = 0;
      scope = article.frontmatter.uri;
      loops = Loop_detection.empty;
      xmlns = Xmlns.init ~reserved;
    }
  in
  HTML.article
    [id "tree-container"]
    [
      (* FIXME: Should be reusing render_section *)
      HTML.section
        [class_ "block"]
        [
          details [(* TODO: check if expanded*) open_]
          @@ summary [] [render_frontmatter ~env article.frontmatter]
             :: render_content ~env article.mainmatter;
        ];
      (match article.frontmatter.uri with
      | None -> footer [] @@ render_backmatter ~env article.backmatter
      | Some uri ->
        if URI.equal (Config.home_uri env.forest.config) uri then null []
        else footer [] @@ render_backmatter ~env article.backmatter);
    ]

and render_section ~env (section : T.content T.section) : node =
  match section with
  | {frontmatter; mainmatter; flags} ->
    let test k = function
      | Some true -> true
      | Some false -> false
      | None -> k
    in
    let class_ =
      if test false flags.metadata_shown then class_ "block"
      else class_ "block hide-metadata"
    in
    let data_taxon =
      match frontmatter.taxon with
      | None -> null_
      | Some _c ->
        (* string_attr "data-taxon" () *)
        null_
    in
    HTML.section [class_; data_taxon]
      [
        (if test true flags.header_shown then
           details
             [(if test true flags.expanded then open_ else null_)]
             [
               summary [] [render_frontmatter ~env frontmatter];
               null @@ render_content ~env mainmatter;
             ]
         else null @@ render_content ~env mainmatter);
        (* render_frontmatter forest frontmatter; *)
        (* null @@ render_content forest mainmatter; *)
      ]

(* Same as render_section, but adds the backmatter-section class *)
and render_backmatter ~env backmatter =
  let@ node = List.map @~ render_content ~env backmatter in
  let attrs = Format.asprintf "%s backmatter-section" node.@["class"] in
  node +@ class_ "%s" attrs

and render_frontmatter ~env
    (frontmatter : T.content T.frontmatter) : node =
  let taxon =
    Option.value ~default:[]
    @@
    let@ c = Option.map @~ frontmatter.taxon in
    render_content ~env c @ [txt ". "]
  in
  let title =
    Option.value ~default:[]
    @@
    let@ c = Option.map @~ frontmatter.title in
    render_content ~env c
  in
  let uri =
    match frontmatter.uri with
    | None -> null []
    | Some uri ->
      let uri_str =
        (* TODO: replace with proper routing from legacy xml client *)
        Format.asprintf "%a" URI.pp (route env.forest uri)
      in
      a [class_ "slug"; href "%s" uri_str] [txt "[%s]" uri_str]
  in
  let source_path =
    match frontmatter.source_path with
    | Some path ->
      [a [class_ "edit-button"; href "vscode://file%s" path] [txt "[edit]"]]
    | None -> []
  in
  let find_meta key =
    let@ str, content = List.find_map @~ frontmatter.metas in
    if str = key then Some content else None
  in
  let render_meta key f =
    Option.value ~default:(null []) (Option.map f (find_meta key))
  in
  let default_meta_item content =
    li [class_ "meta-item"] (render_content ~env content)
  in
  let labelled_external_link ~href ~label =
    li [class_ "meta-item"] [a [class_ "link external"; href] [txt "%s" label]]
  in
  let to_string =
    Plain_text_client.string_of_content ~forest:env.forest
      ~router:(Legacy_xml_client.route env.forest)
  in
  let position = render_meta "position" default_meta_item in
  let institution = render_meta "institution" default_meta_item in
  let venue = render_meta "venue" default_meta_item in
  let source = render_meta "source" default_meta_item in
  let doi = render_meta "doi" default_meta_item in
  let orcid =
    render_meta "orcid" @@ fun c ->
    let content = to_string c in
    li
      [class_ "meta-item"]
      [
        a
          [class_ "doi link"; href "https://www.doi.org/%s" content]
          [txt "%s" content];
      ]
  in
  let external_ =
    render_meta "external" @@ fun c ->
    let content = to_string c in
    li
      [class_ "meta-item"]
      [a [class_ "link external"; href "%s" content] [txt "%s" content]]
  in
  let slides =
    render_meta "slides" @@ fun c ->
    labelled_external_link ~href:(href "%s" (to_string c)) ~label:"Slides"
  in
  let video =
    render_meta "video" @@ fun c ->
    labelled_external_link ~href:(href "%s" (to_string c)) ~label:"Video"
  in
  header []
    [
      h1 []
      @@ [span [class_ "taxon"] taxon]
      @ title
      @ [txt " "; uri]
      @ source_path;
      div
        [class_ "metadata"]
        [
          ul []
          @@ List.map render_date frontmatter.dates
          @ [
              render_attributions ~env frontmatter.attributions;
              position;
              institution;
              venue;
              source;
              doi;
              orcid;
              external_;
              slides;
              video;
            ];
        ];
    ]

and render_transclusion transclusion =
  match transclusion with
  | T.{href; target} ->
    let headers =
      Yojson.Safe.to_string @@ content_target_to_http_header target
    in
    [
      span
        [
          Hx.trigger "load";
          Hx.get "/trees%s" (URI.path_string href);
          Hx.target "this";
          Hx.swap "outerHTML";
          Hx.headers "%s" headers;
        ]
        [txt "transclusion: %s" (Format.asprintf "%a" URI.pp href)];
    ]

and render_content ~env (Content content : T.content) : node list
    =
  List.concat_map (render_content_node ~env) content

and render_content_node ~env (node : 'a T.content_node) :
    node list =
  match node with
  | Text str -> [txt "%s" str]
  | CDATA str -> [txt ~raw:true "<![CDATA[%s]]>" str]
  | Xml_elt elt ->
    let name = render_xml_qname elt.name in
    let xmlns_attrs = Xmlns.xmlns_attrs_for_elt elt env.xmlns in
    let env = {env with xmlns = Xmlns.extend xmlns_attrs env.xmlns} in
    let content = render_content ~env elt.content in
    [
      P.std_tag name
        (List.map render_xmlns_prefix xmlns_attrs
        @ List.map render_xml_attr elt.attrs)
        content;
    ]
  | Transclude transclusion -> render_transclusion transclusion
  | Contextual_number addr -> begin
    match (State.get_article addr) env.forest with
    | Some a ->
      [contextual_number (T.article_to_section a) (default_toc_config ())]
    | None -> []
  end
  (* let custom_number = *)
  (*   article.frontmatter.number *)
  (* in *)
  (* let num = *)
  (*   match custom_number with *)
  (*   | None -> Format.asprintf "[%a]" URI.pp addr *)
  (*   | Some num -> num *)
  (* in *)
  (* [txt "%s" num] *)
  | Link link -> render_link ~env link
  | Section section -> [render_section ~env section]
  | KaTeX (mode, content) ->
    let body = Plain_text_client.string_of_content ~forest:env.forest content in
    (* [txt ~raw: true "%s%s%s" l body r] *)
    begin match mode with
    | Inline -> [span [class_ "math"] [txt ~raw:true "%s" body]]
    | Display -> [div [class_ "math"] [txt ~raw:true "%s" body]]
    end
  | Results_of_datalog_query q ->
    (* We could just evaluate the query immediately. This is just experimental*)
    [
      span
        [
          Hx.get "/query";
          Hx.trigger "load";
          Hx.swap "outerHTML";
          Hx.target "this";
          Hx.vals "%s" Repr.(to_json_string ~minify:true query_t {query = q});
        ]
        [];
    ]
  | T.Datalog_script _ -> []
  | T.Artefact _ | T.Uri _ | T.Route_of_uri _ -> [txt "todo"]

(* TODO: links need to be flattened in order to produce valid HTML. *)
and render_link ~env (link : T.content T.link) : node list =
  let attrs =
    match State.get_article link.href env.forest with
    | None ->
      (* TODO: rendering of hrefs is suboptimal... *)
      [href "%s" (Format.asprintf "%a" URI.pp link.href)]
    | Some article -> begin
      match article.frontmatter.uri with
      | Some _uri ->
        [
          title_ "%s" @@ Option.value ~default:""
          @@ Option.map
               (Plain_text_client.string_of_content ~forest:env.forest
                  ~router:(Legacy_xml_client.route env.forest))
               article.frontmatter.title;
          href "/trees%s" (Format.asprintf "%s" (URI.path_string link.href));
          Hx.target "#tree-container";
          Hx.swap "innerHTML";
        ]
      | None -> [HTML.null_]
    end
  in
  [span [class_ "link local"] [a attrs (render_content ~env link.content)]]

and contextual_number (_tree : T.content T.section) (cfg : toc_config) =
  let should_number =
    cfg.number <> ""
    || (not cfg.in_backmatter) && (not cfg.is_root)
       && not cfg.implicitly_unnumbered
  in
  let taxon =
    if cfg.taxon <> "" then
      cfg.taxon ^ if should_number || cfg.fallback_number <> "" then " " else ""
    else ""
  in
  let number =
    if should_number then
      if cfg.number <> String.empty then cfg.number
      else
        (* TODO: Implement this: <xsl:number format="1.1"
           count="f:tree[ancestor::f:tree and (not(@toc='false' or
                         @numbered='false'))]" level="multiple" />
        *)
        assert false
    else if cfg.fallback_number <> String.empty then cfg.fallback_number
    else ""
  in
  let suffix =
    if
      cfg.taxon <> String.empty
      || cfg.fallback_number <> String.empty
      || should_number
    then cfg.suffix
    else ""
  in
  null [txt "%s %s %s" taxon suffix number]

and _tree_taxon_with_number (_tree : T.content T.section) cfg =
  (*TODO: Implement.*)
  contextual_number _tree cfg

and _render_toc_item ~(env: Html_client.env) (item : T.content T.section) =
  let to_str =
    Plain_text_client.string_of_content ~forest:env.forest
      ~router:(Legacy_xml_client.route env.forest)
  in
  null
    [
      a
        [
          class_ "bullet";
          href "";
          title_ "%s%s"
            (Option.value ~default:""
            @@ Option.map to_str item.frontmatter.title)
            (Option.value ~default:""
            @@ Option.map (Format.asprintf "[%a]" URI.pp) item.frontmatter.uri);
        ]
        [txt "■"];
      span
        [class_ "link local"]
        [
          span
            [class_ "taxon"]
            [_tree_taxon_with_number item (default_toc_config ())];
          (* null @@ render_content forest item.mainmatter; *)
        ];
      ul [] (render_content ~env item.mainmatter);
    ]

and render_toc_mainmatter content =
  let (T.Content nodes) = content in
  ul [class_ "block"]
  @@
  let@ node = List.filter_map @~ nodes in
  match node with T.Section section -> Some (render_toc section) | _ -> None

and render_toc (section : T.content T.section) =
  if
    Some false
    = List.find_map
        (fun (k, v) ->
          if k = "toc" && v = T.Content [T.Text "true"] then Some true else None)
        section.frontmatter.metas
  then null []
  else
    nav
      [id "toc"; Hx.swap_oob "true"]
      [
        div
          [class_ "block"]
          [
            h1 [] [txt "Table of contents"];
            render_toc_mainmatter section.mainmatter;
          ];
      ]

let render_query_result ~forest (vs : Vertex_set.t) =
  let module C = Types.Comparators (struct
    let string_of_content =
      Plain_text_client.string_of_content ~forest ~router:(route forest)
  end) in
  let env : Html_client.env =
    {
      forest;
      section_depth =  0;
      scope = None;
      loops = Loop_detection.empty;
      xmlns = Xmlns.init ~reserved: [{prefix = ""; xmlns = "http://www.w3.org/1999/xhtml"}];
    }
  in
  let make_section =
    T.article_to_section
      ~flags:
        {
          T.default_section_flags with
          expanded = Some false;
          numbered = Some false;
          included_in_toc = Some false;
          metadata_shown = Some true;
        }
  in
  let nodes =
    vs |> Vertex_set.to_seq
    |> Seq.filter_map Vertex.uri_of_vertex
    |> Seq.filter_map (State.get_article @~ forest)
    |> List.of_seq
    |> List.sort C.compare_article
    |> List.map (Fun.compose (render_section ~env) make_section)
  in
  if List.length nodes = 0 then None
  else Some (div [class_ "tree-content"] nodes)
