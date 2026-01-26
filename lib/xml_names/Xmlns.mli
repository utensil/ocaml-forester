open Types

type t

val init : reserved:xmlns_attr list -> t
val xmlns_attrs_for_elt : 'a xml_elt -> t -> xmlns_attr list
val extend : xmlns_attr list -> t -> t
