open Forester_core

type t
val empty : t
val add_seen_uri : URI.t -> t -> t
val add_seen_uri_opt : URI.t option -> t -> t
val have_seen_uri : URI.t -> t -> bool
val have_seen_uri_opt : URI.t option -> t -> bool
