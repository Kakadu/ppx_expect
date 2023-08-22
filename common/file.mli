module Name : sig
  (** Strongly-typed filename *)
  type t = string [@@deriving_inline sexp, compare]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
    include Ppx_compare_lib.Comparable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val relative_to : dir:string -> t -> string

  (* include Identifiable.S with type t := t *)
  val of_string : string -> t
  val to_string : t -> string
end

val initial_dir : unit -> string

module Location : sig
  (** Location within a file *)
  type t =
    { filename : Name.t
    ; line_number : int
    ; line_start : int
    ; start_pos : int
    ; end_pos : int
    }
  [@@deriving_inline sexp, compare]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
    include Ppx_compare_lib.Comparable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val beginning_of_file : Name.t -> t
  val of_source_code_position : Lexing.position -> t

  (* include Comparable.S with type t := t *)
  val equal : t -> t -> bool
end

module Digest : sig
  type t [@@deriving_inline sexp_of, compare]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t

    include Ppx_compare_lib.Comparable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val of_string : string -> t
  val to_string : t -> string
end

module Location_map : sig
  include (module type of Map.Make(Location))
  val to_alist : 'v t -> (key * 'v) list
  val of_alist_multi: (key * 'b) list -> 'b list t
  val of_alist_exn: (key * 'b) list -> 'b t

  (** Creates a map from an association list with unique keys, raising an exception if duplicate 'a keys are found. *)
  val of_alist_reduce: (key * 'b) list -> f:('b -> 'b -> 'b) -> 'b t
end
