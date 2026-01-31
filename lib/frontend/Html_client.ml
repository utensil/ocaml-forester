(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_xml_names
open State.Syntax

open struct
  module T = Types
  module P = Pure_html
  module X = Xml_forester
  module H = P.HTML
end

type env = {
  forest: State.t;
  scope: URI.t option;
  loops: Loop_detection.t;
  xmlns: Xmlns.t;
}

let optional opt kont = match opt with None -> H.null [] | Some v -> kont v

let is_set_to test bopt = match bopt with None -> false | Some b -> b = test

let get_meta (frontmatter : T.content T.frontmatter) meta =
  List.find_map
    (fun (m, v) -> if m = meta then Some v else None)
    frontmatter.metas

(* test if the Home navbar be rendered*)
let is_root config uri =
  match uri with
  | None -> false
  | Some uri -> URI.equal (Config.home_uri config) uri

let should_render_toc _article = false

let route ~env uri =
  let is_local = URI.host uri = URI.host env.forest.config.url in
  if is_local then Format.asprintf "%sindex.html" (URI.path_string uri)
  else Format.asprintf "%a" URI.pp uri

let _get_expanded_title ~env frontmatter forest =
  State.get_expanded_title ?scope:env.scope
    ~flags:T.{empty_when_untitled = true}
    frontmatter forest

let render_date ~env (date : Human_datetime.t) =
  let href_attr =
    let str =
      Format.asprintf "%a" Human_datetime.pp (Human_datetime.drop_time date)
    in
    let uri = URI_scheme.named_uri ~base:env.forest.config.url str in
    match State.get_article ~forest:env.forest uri with
    | None -> None
    | Some _ -> Some (H.href "%s" @@ route ~env uri)
  in
  let year = P.txt "%i" (Human_datetime.year date) in
  let month =
    match Human_datetime.month date with
    | None -> None
    | Some i -> (
      match i with
      | 1 -> Some (P.txt "January")
      | 2 -> Some (P.txt "February")
      | 3 -> Some (P.txt "March")
      | 4 -> Some (P.txt "April")
      | 5 -> Some (P.txt "May")
      | 6 -> Some (P.txt "June")
      | 7 -> Some (P.txt "July")
      | 8 -> Some (P.txt "August")
      | 9 -> Some (P.txt "September")
      | 10 -> Some (P.txt "October")
      | 11 -> Some (P.txt "November")
      | 12 -> Some (P.txt "December")
      | _ -> assert false)
  in
  let day =
    match Human_datetime.day date with
    | None -> H.null []
    | Some i -> P.txt "%i" i
  in
  let content =
    [
      Option.value ~default:(H.null []) month;
      (if Option.is_some month then P.txt " " else H.null []);
      day;
      (if Option.is_some month then P.txt ", " else H.null []);
      year;
    ]
  in
  H.li
    [H.class_ "meta-item"]
    (match href_attr with
    | None -> content
    | Some href -> [H.a [H.class_ "link local"; href] content])

let render_dates ~env = List.map (render_date ~env)

let render_xml_qname qname =
  match qname.prefix with
  | "" -> qname.uname
  (* The browser does not render elements when they are denoted like <html:p> *)
  | "html" -> qname.uname
  | _ -> Format.sprintf "%s:%s" qname.prefix qname.uname

let render_xml_attr ~env T.{key; value} =
  let str_value =
    Plain_text_client.string_of_content ~forest:env.forest value
  in
  P.string_attr (render_xml_qname key) "%s" str_value

let render_xmlns_prefix ({prefix; xmlns} : Forester_xml_names.xmlns_attr) =
  let attr = match prefix with "" -> "xmlns" | _ -> "xmlns:" ^ prefix in
  P.string_attr attr "%s" xmlns

let rec render_content ~env (Content content : T.content) : P.node list =
  match content with
  | T.Text txt0 :: T.Text txt1 :: content ->
    render_content ~env @@ Content (T.Text (txt0 ^ txt1) :: content)
  | node :: content ->
    let xs = render_content_node ~env node in
    let ys = render_content ~env (Content content) in
    xs @ ys
  | [] -> []

and render_content_node ~env (node : 'a T.content_node) : P.node list =
  let config = env.forest.config in
  match node with
  | Text str -> [P.txt "%s" str]
  | CDATA str ->
    (* TODO: Properly handle this. Not printing CDATA because this only works for XML content type*)
    [P.txt ~raw:true "%s" str]
  | Uri uri -> [P.txt "%s" (URI.to_string uri)]
  | Xml_elt elt ->
    let name = render_xml_qname elt.name in
    let xmlns_attrs = Xmlns.xmlns_attrs_for_elt elt env.xmlns in
    let env = {env with xmlns = Xmlns.extend xmlns_attrs env.xmlns} in
    let content = render_content ~env elt.content in
    [
      P.std_tag name
        (List.map render_xmlns_prefix xmlns_attrs
        @ List.map (render_xml_attr ~env) elt.attrs)
        content;
    ]
  | Route_of_uri uri -> [P.txt "%s" (route ~env uri)]
  | Contextual_number uri ->
    let custom_number =
      let@ resource = Option.bind @@ env.forest.@{uri} in
      match resource with
      | T.Article article -> article.frontmatter.number
      | _ -> None
    in
    begin match custom_number with
    | None -> [P.txt "%s" @@ URI.relative_path_string ~base:config.url uri]
    | Some num -> [P.txt "%s" num]
    end
  | KaTeX (_, content) -> [P.HTML.code [] @@ render_content ~env content]
  | Artefact artefact -> render_content ~env @@ artefact.content
  | Section section -> render_section ~env section
  | Transclude transclusion -> render_transclusion ~env transclusion
  | Link link -> [render_link ~env link]
  | Results_of_datalog_query _ -> [] (* TODO: just make a list of links *)
  | Datalog_script _ -> []

and render_link ~env (link : T.content T.link) : P.node =
  let is_local = URI.host link.href = URI.host env.forest.config.url in
  let href =
    if is_local then H.href "%sindex.html" (URI.path_string link.href)
    else H.href "%s" (Format.asprintf "%a" URI.pp link.href)
  in
  H.span
    [(if is_local then H.class_ "link local" else H.class_ "link external")]
    [H.a [href] @@ render_content ~env link.content]

and render_transclusion ~env (transclusion : T.transclusion) : P.node list =
  match State.get_content_of_transclusion ~forest:env.forest transclusion with
  | None -> Reporter.fatal (Resource_not_found transclusion.href)
  | Some content -> render_content ~env content

and _render_section_for_atom_client ~env (section : T.content T.section) :
    P.node list =
  let env = {env with scope = section.frontmatter.uri} in
  [
    H.section []
      [
        begin match section.frontmatter.title with
        | None -> H.null []
        | Some title -> H.header [] [H.h1 [] @@ render_content ~env title]
        end;
        begin if
          Loop_detection.have_seen_uri_opt section.frontmatter.uri env.loops
        then P.txt "Transclusion loop detected, rendering stopped."
        else
          H.null
          @@ render_content
               ~env:
                 {
                   env with
                   loops =
                     Loop_detection.add_seen_uri_opt section.frontmatter.uri
                       env.loops;
                 }
               section.mainmatter
        end;
      ];
  ]

and render_attributions ~env (attributions : T.content T.attribution list) =
  let render_attribution attribution =
    match attribution with
    | T.{vertex; _} -> (
      match vertex with
      | T.Uri_vertex href ->
        let content =
          T.Content
            [T.Transclude {href; target = Title {empty_when_untitled = false}}]
        in
        render_link ~env T.{href; content}
      | T.Content_vertex content -> H.null @@ render_content ~env content)
  in
  let authors, contributors =
    attributions
    |> List.partition_map @@ fun a ->
       match T.(a.role) with T.Author -> Left a | Contributor -> Right a
  in
  H.li
    [H.class_ "meta-item"]
    [
      H.address [H.class_ "author"]
      @@ List.map render_attribution authors
      @ begin if List.length contributors > 0 then
        [P.txt "with contributions from "]
      else []
      end
      @ List.map render_attribution contributors;
    ]

and default_meta_item ~env frontmatter meta =
  optional (get_meta frontmatter meta) (fun content ->
      H.li [H.class_ "meta-item"] (render_content ~env content))

and render_attribution_vertex ~env vtx =
  match vtx with
  | T.Content_vertex content -> H.null (render_content ~env content)
  | T.Uri_vertex href ->
    let content =
      T.Content
        [T.Transclude {href; target = Title {empty_when_untitled = false}}]
    in
    render_link ~env T.{href; content}

and render_authors ~env (frontmatter : T.(content frontmatter)) =
  let authors, contributors =
    List.partition_map (function T.{role; vertex} ->
        (match role with Author -> Left vertex | Contributor -> Right vertex))
    @@ Forest_util.collect_attributions env.forest frontmatter.uri
         frontmatter.attributions
  in
  let authors =
    List_util.intersperse (P.txt ", ")
    @@ List.map (fun author -> render_attribution_vertex ~env author) authors
  in
  let contributors =
    if List.length contributors >= 1 then
      P.txt ", with contributions from "
      :: (List_util.intersperse (P.txt ", ")
         @@ List.map
              (fun contributor -> render_attribution_vertex ~env contributor)
              contributors)
    else []
  in
  H.li
    [H.class_ "meta-item"]
    [H.address [H.class_ "author"] (authors @ contributors)]

and render_position ~env frontmatter =
  default_meta_item ~env frontmatter "position"

and render_institution ~env frontmatter =
  default_meta_item ~env frontmatter "institution"

and render_venue ~env frontmatter = default_meta_item ~env frontmatter "venue"

and render_source ~env frontmatter = default_meta_item ~env frontmatter "source"

and render_doi ~env frontmatter =
  optional (get_meta frontmatter "doi") (fun c ->
      let doi = Plain_text_client.string_of_content ~forest:env.forest c in
      H.li
        [H.class_ "meta-item"]
        [
          H.a
            [H.class_ "doi"; H.href "https://www.doi.org/%s" doi]
            (render_content ~env c);
        ])

and render_orcid ~env frontmatter =
  optional (get_meta frontmatter "orcid") (fun c ->
      let orcid = Plain_text_client.string_of_content ~forest:env.forest c in
      H.li
        [H.class_ "meta-item"]
        [
          H.a
            [H.class_ "orcid"; H.href "https://orcid.org/%s" orcid]
            (render_content ~env c);
        ])

and render_external ~env frontmatter =
  optional (get_meta frontmatter "external") (fun c ->
      let link = Plain_text_client.string_of_content ~forest:env.forest c in
      H.li
        [H.class_ "meta-item"]
        [
          H.a
            [H.class_ "link external"; H.href "%s" link]
            (render_content ~env c);
        ])

and render_slides ~env frontmatter =
  optional (get_meta frontmatter "slides") (fun c ->
      let link = Plain_text_client.string_of_content ~forest:env.forest c in
      H.li
        [H.class_ "meta-item"]
        [H.a [H.class_ "link external"; H.href "%s" link] [P.txt "Slides"]])

and render_video ~env frontmatter =
  optional (get_meta frontmatter "video") (fun c ->
      let link = Plain_text_client.string_of_content ~forest:env.forest c in
      H.li
        [H.class_ "meta-item"]
        [H.a [H.class_ "link external"; H.href "%s" link] [P.txt "Video"]])

and render_bibtex ~env frontmatter =
  optional (get_meta frontmatter "bibtex") (fun c ->
      H.pre [] (render_content ~env c))

and render_tree_taxon_with_number ~env:_ _article = H.null []

and render_title ~env (frontmatter : T.(content frontmatter)) =
  render_content ~env
    (State.get_expanded_title ?scope:env.scope frontmatter env.forest)

and render_display_uri ~env (frontmatter : T.(content frontmatter)) =
  match frontmatter.uri with
  | None -> H.null []
  | Some uri ->
    (* let uri_str = Format.asprintf "%a" URI.pp uri in *)
    H.a
      [H.class_ "slug"; H.href "%s" (route ~env uri)]
      [
        P.txt "[";
        P.txt "%s" @@ URI.display_path_string ~base:env.forest.config.url uri;
        P.txt "]";
      ]

and render_source_path (frontmatter : T.(content frontmatter)) =
  (* TODO: Check dev mode *)
  match frontmatter.source_path with
  | None -> H.null []
  | Some source_path ->
    H.a
      [H.class_ "edit-button"; H.href "vscode://file%s" source_path]
      [P.txt "[edit]"]

and render_frontmatter ~env (frontmatter : _ T.frontmatter) : P.node =
  H.header []
    [
      H.h1 []
        [
          H.span
            [H.class_ "taxon"]
            [render_tree_taxon_with_number ~env frontmatter];
          H.null @@ render_title ~env frontmatter;
          P.txt " ";
          render_display_uri ~env frontmatter;
          P.txt " ";
          render_source_path frontmatter;
        ];
      H.div
        [H.class_ "metadata"]
        [
          H.ul []
            [
              H.null @@ render_dates ~env frontmatter.dates;
              render_authors ~env frontmatter;
              render_position ~env frontmatter;
              render_institution ~env frontmatter;
              render_venue ~env frontmatter;
              render_source ~env frontmatter;
              render_doi ~env frontmatter;
              render_orcid ~env frontmatter;
              render_external ~env frontmatter;
              render_slides ~env frontmatter;
              render_video ~env frontmatter;
            ];
        ];
    ]

and render_section ~env ({flags; mainmatter; frontmatter} : T.content T.section)
    : P.node list =
  let T.{metadata_shown; header_shown; expanded; _} = flags in
  let open_ = if not @@ is_set_to false expanded then H.open_ else H.null_ in
  [
    H.section
      [
        (if is_set_to false metadata_shown then H.class_ "block hide-metadata"
         else H.class_ "block");
      ]
      [
        begin if Loop_detection.have_seen_uri_opt frontmatter.uri env.loops then
          P.txt "Transclusion loop detected, rendering stopped."
        else if not @@ is_set_to false header_shown then
          H.details [open_]
            [
              H.summary [] [render_frontmatter ~env frontmatter];
              (let env =
                 {
                   env with
                   loops =
                     Loop_detection.add_seen_uri_opt frontmatter.uri env.loops;
                   scope = frontmatter.uri;
                 }
               in
               H.null @@ render_content ~env mainmatter);
              render_bibtex ~env frontmatter;
            ]
        else H.null @@ render_content ~env mainmatter
        end;
      ];
  ]

let render_article ~env (article : T.content T.article) : P.node =
  let should_render_backmatter _ = true in
  H.article []
    [
      H.section
        [H.class_ "block"]
        [
          H.details [H.open_]
            (H.summary [] [render_frontmatter ~env article.frontmatter]
             :: render_content
                  ~env:
                    {
                      env with
                      loops =
                        Loop_detection.add_seen_uri_opt article.frontmatter.uri
                          env.loops;
                    }
                  article.mainmatter
            @ [render_bibtex ~env article.frontmatter]);
        ];
      (if should_render_backmatter article then H.footer [] [] else H.null []);
    ]

let render_toc _article = H.ul [] []

(* Just used by the atom client *)
let render_article_as_div ~(forest : State.t) (article : T.content T.article) :
    P.node =
  let reserved = [{prefix = ""; xmlns = "http://www.w3.org/1999/xhtml"}] in
  let env =
    {
      forest;
      scope = article.frontmatter.uri;
      loops = Loop_detection.empty;
      xmlns = Xmlns.init ~reserved;
    }
  in
  H.div
    (List.map render_xmlns_prefix reserved)
    [H.null @@ render_content ~env article.mainmatter]

let page_template ~is_root ~title:ttl c =
  let open H in
  html []
    [
      head []
        [
          meta [http_equiv `content_type; content "text/html"; charset "UTF-8"];
          meta
            [name "viewport"; content "width=device-width, initial-scale=1.0"];
          link [rel "stylesheet"; href "/style.css"];
          link [rel "stylesheet"; href "/katex.min.css"];
          script [type_ "module"; src "/forester.js"] "";
          H.title [] "%s" ttl;
        ];
      body []
        [
          P.std_tag "ninja-keys"
            [placeholder "Start typing a note title or ID"]
            [];
          (if is_root then null []
           else
             header
               [class_ "header"]
               [
                 nav
                   [class_ "nav"]
                   [
                     div
                       [class_ "logo"]
                       [a [href "index.html"; title_ "home"] [P.txt "« Home"]];
                   ];
               ]);
          div [id "grid-wrapper"] c;
        ];
    ]

let render_page ~forest (tree : _ T.article) : P.node =
  let reserved = [{prefix = ""; xmlns = "http://www.w3.org/1999/xhtml"}] in
  let env =
    {
      forest;
      scope = tree.frontmatter.uri;
      loops = Loop_detection.empty;
      xmlns = Xmlns.init ~reserved;
    }
  in
  let ttl =
    match tree.frontmatter.title with
    | None -> (* FIXME: *) ""
    | Some _ ->
      let title =
        State.get_expanded_title ?scope:env.scope tree.frontmatter env.forest
      in
      Plain_text_client.string_of_content ~forest:env.forest title
  in
  let open H in
  let is_root = is_root env.forest.config tree.frontmatter.uri in
  page_template ~is_root ~title:ttl
    [
      render_article ~env tree;
      (if should_render_toc article then
         nav
           [id "toc"]
           [
             div
               [class_ "block"]
               [h1 [] [P.txt "Table of Contents"]; render_toc article];
           ]
       else null []);
    ]
