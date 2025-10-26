open String
let caseless_equal c1 c2 =
  Char.equal (Char.lowercase_ascii c1) (Char.lowercase_ascii c2)

module Search_pattern0 = struct
  type t =
    { pattern : string
    ; case_sensitive : bool
    ; kmp_array : int array
    }

(*   let sexp_of_t { pattern; case_sensitive; kmp_array = _ } : Sexp.t =
    List
      [ List [ Atom "pattern"; sexp_of_string pattern ]
      ; List [ Atom "case_sensitive"; sexp_of_bool case_sensitive ]
      ]
  ;; *)

  let pattern t = t.pattern
  let case_sensitive t = t.case_sensitive

  (* Find max number of matched characters at [next_text_char], given the current
     [matched_chars]. Try to extend the current match, if chars don't match, try to match
     fewer chars. If chars match then extend the match. *)
  let kmp_internal_loop ~matched_chars ~next_text_char ~pattern ~kmp_array ~char_equal =
    let matched_chars = ref matched_chars in
    while
      !matched_chars > 0
      && not (char_equal next_text_char (String.unsafe_get pattern !matched_chars))
    do
      matched_chars := Array.unsafe_get kmp_array (!matched_chars - 1)
    done;
    if char_equal next_text_char (String.unsafe_get pattern !matched_chars)
    then matched_chars := !matched_chars + 1;
    !matched_chars
  ;;

  let get_char_equal ~case_sensitive =
    match case_sensitive with
    | true -> Char.equal
    | false -> caseless_equal
  ;;

  (* Classic KMP pre-processing of the pattern: build the int array, which, for each i,
     contains the length of the longest non-trivial prefix of s which is equal to a suffix
     ending at s.[i] *)
  let create pattern ~case_sensitive =
    let n = length pattern in
    let kmp_array = Stdlib.Array.make n (-1) in
    if n > 0
    then (
      let char_equal = get_char_equal ~case_sensitive in
      Array.unsafe_set kmp_array 0 0;
      let matched_chars = ref 0 in
      for i = 1 to n - 1 do
        matched_chars
        := kmp_internal_loop
             ~matched_chars:!matched_chars
             ~next_text_char:(unsafe_get pattern i)
             ~pattern
             ~kmp_array
             ~char_equal;
        Array.unsafe_set kmp_array i !matched_chars
      done);
    { pattern; case_sensitive; kmp_array }
  ;;

  (* Classic KMP: use the pre-processed pattern to optimize look-behinds on non-matches.
     We return int to avoid allocation in [index_exn]. -1 means no match. *)
  let index_internal ?(pos = 0) { pattern; case_sensitive; kmp_array } ~in_:text =
    if pos < 0 || pos > length text - length pattern
    then -1
    else (
      let char_equal = get_char_equal ~case_sensitive in
      let j = ref pos in
      let matched_chars = ref 0 in
      let k = length pattern in
      let n = length text in
      while !j < n && !matched_chars < k do
        let next_text_char = unsafe_get text !j in
        matched_chars
        := kmp_internal_loop
             ~matched_chars:!matched_chars
             ~next_text_char
             ~pattern
             ~kmp_array
             ~char_equal;
        j := !j + 1
      done;
      if !matched_chars = k then !j - k else -1)
  ;;

  let matches t str = index_internal t ~in_:str >= 0

  let index ?pos t ~in_ =
    let p = index_internal ?pos t ~in_ in
    if p < 0 then None else Some p
  ;;

  let index_exn ?pos t ~in_ =
    let p = index_internal ?pos t ~in_ in
    if p >= 0
    then p
    else
      failwith (Printf.sprintf  "Substring %S not found " t.pattern)
      (* raise_s
        (Sexp.message "Substring not found" [ "substring", sexp_of_string t.pattern ]) *)
  ;;

  let index_all { pattern; case_sensitive; kmp_array } ~may_overlap ~in_:text =
    if length pattern = 0
    then List.init (1 + length text) Fun.id
    else (
      let char_equal = get_char_equal ~case_sensitive in
      let matched_chars = ref 0 in
      let k = length pattern in
      let n = length text in
      let found = ref [] in
      for j = 0 to n do
        if !matched_chars = k
        then (
          found := (j - k) :: !found;
          (* we just found a match in the previous iteration *)
          match may_overlap with
          | true -> matched_chars := Array.unsafe_get kmp_array (k - 1)
          | false -> matched_chars := 0);
        if j < n
        then (
          let next_text_char = unsafe_get text j in
          matched_chars
          := kmp_internal_loop
               ~matched_chars:!matched_chars
               ~next_text_char
               ~pattern
               ~kmp_array
               ~char_equal)
      done;
      List.rev !found)
  ;;

  let replace_first ?pos t ~in_:s ~with_ =
    match index ?pos t ~in_:s with
    | None -> s
    | Some i ->
      let len_s = length s in
      let len_t = length t.pattern in
      let len_with = length with_ in
      let dst = Bytes.create (len_s + len_with - len_t) in
      BytesLabels.blit_string ~src:s ~src_pos:0 ~dst ~dst_pos:0 ~len:i;
      BytesLabels.blit_string ~src:with_ ~src_pos:0 ~dst ~dst_pos:i ~len:len_with;
      BytesLabels.blit_string
        ~src:s
        ~src_pos:(i + len_t)
        ~dst
        ~dst_pos:(i + len_with)
        ~len:(len_s - i - len_t);
      Bytes.unsafe_to_string dst
  ;;


  let replace_all t ~in_:s ~with_ =
    let matches = index_all t ~may_overlap:false ~in_:s in
    match matches with
    | [] -> s
    | _ :: _ ->
      let len_s = length s in
      let len_t = length t.pattern in
      let len_with = length with_ in
      let num_matches = List.length matches in
      let dst = Bytes.create (len_s + ((len_with - len_t) * num_matches)) in
      let next_dst_pos = ref 0 in
      let next_src_pos = ref 0 in
      ListLabels.iter matches ~f:(fun i ->
        let len = i - !next_src_pos in
        BytesLabels.blit_string ~src:s ~src_pos:!next_src_pos ~dst ~dst_pos:!next_dst_pos ~len;
        BytesLabels.blit_string
          ~src:with_
          ~src_pos:0
          ~dst
          ~dst_pos:(!next_dst_pos + len)
          ~len:len_with;
        next_dst_pos := !next_dst_pos + len + len_with;
        next_src_pos := !next_src_pos + len + len_t);
      BytesLabels.blit_string
        ~src:s
        ~src_pos:!next_src_pos
        ~dst
        ~dst_pos:!next_dst_pos
        ~len:(len_s - !next_src_pos);
      Bytes.unsafe_to_string dst
  ;;

  let split_on t s =
    let pattern_len = String.length t.pattern in
    let matches = index_all t ~may_overlap:false ~in_:s in
    ListLabels.map2
      (-pattern_len :: matches)
      (matches @ [ String.length s ])
      ~f:(fun i j -> StringLabels.sub s ~pos:(i + pattern_len) ~len:(j - i - pattern_len))
  ;;
(*
  module Private = struct
    type public = t

    type nonrec t = t =
      { pattern : string
      ; case_sensitive : bool
      ; kmp_array : int array
      }
    [@@deriving_inline equal, sexp_of]

    let equal =
      (fun a__003_ b__004_ ->
         if Stdlib.( == ) a__003_ b__004_
         then true
         else
           Stdlib.( && )
             (equal_string a__003_.pattern b__004_.pattern)
             (Stdlib.( && )
                (equal_bool a__003_.case_sensitive b__004_.case_sensitive)
                (equal_array equal_int a__003_.kmp_array b__004_.kmp_array))
           : t -> t -> bool)
    ;;

    let sexp_of_t =
      (fun { pattern = pattern__008_
           ; case_sensitive = case_sensitive__010_
           ; kmp_array = kmp_array__012_
           } ->
        let bnds__007_ = ([] : _ Stdlib.List.t) in
        let bnds__007_ =
          let arg__013_ = sexp_of_array sexp_of_int kmp_array__012_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "kmp_array"; arg__013_ ] :: bnds__007_
           : _ Stdlib.List.t)
        in
        let bnds__007_ =
          let arg__011_ = sexp_of_bool case_sensitive__010_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "case_sensitive"; arg__011_ ]
           :: bnds__007_
           : _ Stdlib.List.t)
        in
        let bnds__007_ =
          let arg__009_ = sexp_of_string pattern__008_ in
          (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pattern"; arg__009_ ] :: bnds__007_
           : _ Stdlib.List.t)
        in
        Sexplib0.Sexp.List bnds__007_
        : t -> Sexplib0.Sexp.t)
    ;;

    [@@@end]

    let representation = Fun.id
  end *)
end
  include Search_pattern0

  let create ?(case_sensitive = true) pattern = create pattern ~case_sensitive

let substr_index_gen ~case_sensitive ?pos t ~pattern =
  index ?pos (create ~case_sensitive pattern) ~in_:t
;;

let is_substring_gen ~case_sensitive t ~substring =
  Option.is_some (substr_index_gen t ~pattern:substring ~case_sensitive)
;;
let is_substring = is_substring_gen ~case_sensitive:true
