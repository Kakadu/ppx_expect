(** Substring search and replace functions.  They use the Knuth-Morris-Pratt algorithm
    (KMP) under the hood.

    The functions in the [Search_pattern] module allow the program to preprocess the
    searched pattern once and then use it many times without further allocations. *)

    type t [@@deriving_inline sexp_of]

  (* val sexp_of_t : t -> Sexplib0.Sexp.t *)

  [@@@end]

  (** [create pattern] preprocesses [pattern] as per KMP, building an [int array] of
      length [length pattern].  All inputs are valid. *)
  val create : ?case_sensitive:bool (** default = true *) -> string -> t

  (** [pattern t] returns the string pattern used to create [t]. *)
  val pattern : t -> string

  (** [case_sensitive t] returns whether [t] matches strings case-sensitively. *)
  val case_sensitive : t -> bool

  (** [matches pat str] returns true if [str] matches [pat] *)
  val matches : t -> string -> bool

  (** [pos < 0] or [pos >= length string] result in no match (hence [index] returns
      [None] and [index_exn] raises). *)
  val index : ?pos:int -> t -> in_:string -> int option

  val index_exn : ?pos:int -> t -> in_:string -> int

  (** [may_overlap] determines whether after a successful match, [index_all] should start
      looking for another one at the very next position ([~may_overlap:true]), or jump to
      the end of that match and continue from there ([~may_overlap:false]), e.g.:

      - [index_all (create "aaa") ~may_overlap:false ~in_:"aaaaBaaaaaa" = [0; 5; 8]]
      - [index_all (create "aaa") ~may_overlap:true ~in_:"aaaaBaaaaaa" = [0; 1; 5; 6; 7;
        8]]

      E.g., [replace_all] internally calls [index_all ~may_overlap:false]. *)
  val index_all : t -> may_overlap:bool -> in_:string -> int list

  (** Note that the result of [replace_all pattern ~in_:text ~with_:r] may still
      contain [pattern], e.g.,

      {[
        replace_all (create "bc") ~in_:"aabbcc" ~with_:"cb" = "aabcbc"
      ]} *)
  val replace_first : ?pos:int -> t -> in_:string -> with_:string -> string

  val replace_all : t -> in_:string -> with_:string -> string

  (** Similar to [String.split] or [String.split_on_chars], but instead uses a given
      search pattern as the separator.  Separators are non-overlapping.  *)
  val split_on : t -> string -> string list

  (**/**)
val is_substring : string -> substring:string -> bool
