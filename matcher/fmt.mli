(** Representation of parsed [%expect] lines *)

type t =
  | Regexp of string
  | Glob of string
  | Literal of string
[@@deriving_inline sexp_of, compare, equal]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_t : t -> Sexplib0.Sexp.t

  include Ppx_compare_lib.Comparable.S with type t := t
  include Ppx_compare_lib.Equal.S with type t := t
end
[@@ocaml.doc "@inline"]

[@@@end]
