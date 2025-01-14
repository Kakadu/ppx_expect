open Expect_test_common
open Sexplib0.Sexp_conv

module Result = struct
  (* Either match with an explicit success, or (lazily) produce a correction. *)
  type 'a t =
    | Match
    | Correction of 'a
  [@@deriving_inline sexp_of, compare]

  let _ = fun (_ : 'a t) -> ()

  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun (type a__004_) : ((a__004_ -> Sexplib0.Sexp.t) -> a__004_ t -> Sexplib0.Sexp.t) ->
    fun _of_a__001_ -> function
      | Match -> Sexplib0.Sexp.Atom "Match"
      | Correction arg0__002_ ->
        let res0__003_ = _of_a__001_ arg0__002_ in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Correction"; res0__003_ ]
  ;;

  let _ = sexp_of_t

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a a__005_ b__006_ ->
    if Stdlib.( == ) a__005_ b__006_
    then 0
    else (
      match a__005_, b__006_ with
      | Match, Match -> 0
      | Match, _ -> -1
      | _, Match -> 1
      | Correction _a__007_, Correction _b__008_ -> _cmp__a _a__007_ _b__008_)
  ;;

  let _ = compare

  [@@@end]

  let map t ~f =
    match t with
    | Match -> Match
    | Correction x -> Correction (f x)
  ;;

  let value t ~success =
    match t with
    | Match -> success
    | Correction f -> f
  ;;
end

let matches_regexp ~(pat : Re.t) s =
  Re.execp (Re.compile (Re.whole_string pat)) s
;;

let glob = Re.Glob.glob ~anchored:true ~pathname:false ~expand_braces:true

let line_matches ~(expect : Fmt.t) ~actual =
  match expect with
  | Literal expect -> expect = actual
  | Glob expect -> matches_regexp ~pat:(glob expect) actual
  | Regexp expect -> matches_regexp ~pat:(Re.Emacs.re expect) actual
;;

let literal_line ~allow_output_patterns actual : Fmt.t Cst.Line.t =
  match actual with
  | "" -> Blank ""
  | _ ->
    let line_matches_itself =
      (not allow_output_patterns)
      || line_matches
           ~expect:(Lexer.parse_pretty_line actual ~allow_output_patterns)
           ~actual
    in
    Not_blank
      { data = Literal actual
      ; orig = (if line_matches_itself then actual else actual ^ " (literal)")
      ; trailing_blanks = ""
      }
;;

let reconcile_line ~(expect : Fmt.t) ~actual ~allow_output_patterns
  : Fmt.t Cst.Line.t Result.t
  =
  assert (not (String.contains actual '\n'));
  if line_matches ~expect ~actual
  then Match
  else Correction (literal_line actual ~allow_output_patterns)
;;

let rec lines_match
          ~(expect_lines : Fmt.t Cst.Line.t list)
          ~(actual_lines : string list)
          ~allow_output_patterns
  : bool
  =
  match expect_lines, actual_lines with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | expect :: expect_lines, actual :: actual_lines ->
    let format =
      Cst.Line.data expect ~blank:(Literal "") ~conflict_marker:(fun marker ->
        Literal marker)
    in
    let line = reconcile_line ~expect:format ~actual ~allow_output_patterns in
    (match line with
     | Match -> lines_match ~expect_lines ~actual_lines ~allow_output_patterns
     | Correction _ -> false)
;;

let rec corrected_rev
          acc
          ~(expect_lines : Fmt.t Cst.Line.t list)
          ~(actual_lines : string list)
          ~allow_output_patterns
  : Fmt.t Cst.Line.t list
  =
  match expect_lines, actual_lines with
  | [], [] -> acc
  | [], actual_lines ->
    ListLabels.fold_left actual_lines ~init:acc ~f:(fun acc x ->
      literal_line x ~allow_output_patterns :: acc)
  | _, [] -> acc
  | expect :: expect_lines, actual :: actual_lines ->
    let format =
      Cst.Line.data expect ~blank:(Literal "") ~conflict_marker:(fun marker ->
        Literal marker)
    in
    let line =
      reconcile_line ~expect:format ~actual ~allow_output_patterns
      |> Result.value ~success:expect
    in
    corrected_rev ~expect_lines ~actual_lines (line :: acc) ~allow_output_patterns
;;

let reconcile_lines ~expect_lines ~actual_lines ~allow_output_patterns
  : Fmt.t Cst.Line.t list Result.t
  =
  if lines_match ~expect_lines ~actual_lines ~allow_output_patterns
  then Match
  else
    Correction
      (List.rev (corrected_rev [] ~expect_lines ~actual_lines ~allow_output_patterns))
;;

let expectation_body_internal
      ~(expect : Fmt.t Cst.t Expectation.Body.t)
      ~actual
      ~default_indent
      ~pad_single_line
      ~allow_output_patterns
  : Fmt.t Cst.t Expectation.Body.t Result.t
  =
  match expect with
  | Exact expect -> if expect = actual then Match else Correction (Exact actual)
  | Output -> Match
  | Pretty expect ->
    let actual_lines =
      Lexer.strip_surrounding_whitespaces actual |> Cst.stripped_original_lines
    in
    let expect_lines = Cst.to_lines expect in
    (match reconcile_lines ~expect_lines ~actual_lines ~allow_output_patterns with
     | Match -> Match
     | Correction reconciled_lines ->
       let reconciled =
         Cst.reconcile
           expect
           ~lines:reconciled_lines
           ~default_indentation:default_indent
           ~pad_single_line
       in
       Correction (Pretty reconciled))
  | Unreachable ->
    let actual_lines =
      Lexer.strip_surrounding_whitespaces actual |> Cst.stripped_original_lines
    in
    (match reconcile_lines ~expect_lines:[] ~actual_lines ~allow_output_patterns with
     | Match -> Correction (Pretty (Empty ""))
     | Correction reconciled_lines ->
       let reconciled =
         Cst.reconcile
           (Empty "")
           ~lines:reconciled_lines
           ~default_indentation:default_indent
           ~pad_single_line
       in
       Correction (Pretty reconciled))
;;

let expectation_body
      ~(expect : Fmt.t Cst.t Expectation.Body.t)
      ~actual
      ~default_indent
      ~pad_single_line
      ~allow_output_patterns
  : Fmt.t Cst.t Expectation.Body.t Result.t
  =
  let res =
    expectation_body_internal
      ~expect
      ~actual
      ~default_indent
      ~pad_single_line
      ~allow_output_patterns
  in
  match res with
  | Match -> Match
  | Correction c ->
    (match
       expectation_body_internal
         ~expect:c
         ~actual
         ~default_indent
         ~pad_single_line
         ~allow_output_patterns
     with
     | Match -> res
     | Correction _ -> assert false)
;;

module Private = struct
  let line_matches = line_matches
  let reconcile_line = reconcile_line
end
