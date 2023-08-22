
open Expect_test_common

let bprintf = Printf.bprintf

module Saved_output = struct
  type t =
    | One of string
    | Many_distinct of string list

  module String_set = Set.Make(String)
  let of_nonempty_list_exn outputs =
    let _, rev_deduped_preserving_order =
      ListLabels.fold_left
        outputs
        ~init:(String_set.empty, [])
        ~f:(fun (as_set, as_list) output ->
          if String_set.mem  output as_set
          then as_set, as_list
          else String_set.add  output as_set, output :: as_list)
    in
    match List.rev rev_deduped_preserving_order with
    | [] -> failwith "Saved_output.of_nonempty_list_exn got an empty list"
    | [ output ] -> One output
    | outputs -> Many_distinct outputs
  ;;

  let to_list = function
    | One s -> [ s ]
    | Many_distinct many -> many
  ;;

  let merge t1 t2 = of_nonempty_list_exn (to_list t1 @ to_list t2)
end

module Test_outcome = struct
  module Expectations = struct
    type t = Fmt.t Cst.t Expectation.t File.Location_map.t
    [@@deriving_inline compare, equal]

    let _ = fun (_ : t) -> ()


    let compare =
      (fun a__001_ b__002_ ->
         File.Location_map.compare
           (* (module File.Location) *)
           (fun a__003_ b__004_ ->
              Expectation.compare
                (fun a__005_ b__006_ -> Cst.compare Fmt.compare a__005_ b__006_)
                a__003_
                b__004_)
           a__001_
           b__002_
         : t -> t -> int)
    ;;

    let _ = compare

    let equal =
      (fun a__009_ b__010_ ->
         File.Location_map.compare
           (* (module File.Location) *)
           (fun a__011_ b__012_ : int  ->
              Expectation.compare
                (fun a__013_ b__014_ -> Cst.compare Fmt.compare a__013_ b__014_)
                a__011_
                b__012_)
           a__009_
           b__010_
           = 0
         : t -> t -> bool)
    ;;

    let _ = equal

    [@@@end]
  end

  type t =
    { expectations : Expectations.t
    ; uncaught_exn_expectation : Fmt.t Cst.t Expectation.t option
    ; saved_output : Saved_output.t File.Location_map.t
    ; trailing_output : Saved_output.t
    ; uncaught_exn : Saved_output.t option
    ; upon_unreleasable_issue : Expect_test_config_types.Upon_unreleasable_issue.t
    }

  let merge_exn
        t
        { expectations
        ; uncaught_exn_expectation
        ; saved_output
        ; trailing_output
        ; uncaught_exn
        ; upon_unreleasable_issue
        }
    =
    if not (Expectations.equal t.expectations expectations)
    then failwith "merging tests of different expectations";
    if not
         (Expect_test_config_types.Upon_unreleasable_issue.equal
            t.upon_unreleasable_issue
            upon_unreleasable_issue)
    then failwith "merging tests of different [Upon_unreleasable_issue]";
    if not
         (Option.equal
            (Expectation.equal (Cst.equal Fmt.equal))
            t.uncaught_exn_expectation
            uncaught_exn_expectation)
    then failwith "merging tests of different uncaught exception expectations";
    { expectations
    ; uncaught_exn_expectation
    ; saved_output =
      File.Location_map.merge
        (* Map.merge  *)
         (fun _key left right -> match left, right with
          | None , None -> None
          | Some x, None | None, Some x -> Some x
          | Some x, Some y -> Some (Saved_output.merge x y))
          t.saved_output saved_output
    ; uncaught_exn =
        (match t.uncaught_exn, uncaught_exn with
         | None, None -> None
         | Some x, None | None, Some x -> Some x
         | Some x, Some y -> Some (Saved_output.merge x y))
    ; trailing_output = Saved_output.merge t.trailing_output trailing_output
    ; upon_unreleasable_issue
    }
  ;;
end

module Test_correction = struct
  module Node_correction = struct
    type t =
      | Collector_never_triggered
      | Correction of Fmt.t Cst.t Expectation.Body.t
  end

  module Uncaught_exn = struct
    type t =
      | Match
      | Without_expectation of Fmt.t Cst.t Expectation.Body.t
      | Correction of Fmt.t Cst.t Expectation.t * Fmt.t Cst.t Expectation.Body.t
      | Unused_expectation of Fmt.t Cst.t Expectation.t
  end

  type t =
    { location : File.Location.t
    ; (* In the order of the file *)
      corrections : (Fmt.t Cst.t Expectation.t * Node_correction.t) list
    ; uncaught_exn : Uncaught_exn.t
    ; trailing_output : Fmt.t Cst.t Expectation.Body.t Reconcile.Result.t
    }

  let map_corrections t ~f =
    { location = t.location
    ; corrections =
        ListLabels.map t.corrections ~f:(fun (e, c) ->
          ( e
          , match c with
          | Node_correction.Collector_never_triggered -> c
          | Correction body -> Correction (Expectation.Body.map_pretty body ~f) ))
    ; uncaught_exn =
        (match t.uncaught_exn with
         | (Match | Unused_expectation _) as x -> x
         | Without_expectation body ->
           Without_expectation (Expectation.Body.map_pretty body ~f)
         | Correction (e, body) -> Correction (e, Expectation.Body.map_pretty body ~f))
    ; trailing_output =
        Reconcile.Result.map t.trailing_output ~f:(Expectation.Body.map_pretty ~f)
    }
  ;;

  let compare_locations a b = compare a.location.line_number b.location.line_number

  let make ~location ~corrections ~uncaught_exn ~trailing_output : t Reconcile.Result.t =
    if Utils.list_is_empty corrections
    && (match (trailing_output : _ Reconcile.Result.t) with
        | Match -> true
        | Correction _ -> false)
    &&
    match (uncaught_exn : Uncaught_exn.t) with
    | Match -> true
    | Correction _ | Without_expectation _ | Unused_expectation _ -> false
    then Match
    else Correction { location; corrections; uncaught_exn; trailing_output }
  ;;
end

let indentation_at file_contents (loc : File.Location.t) =
  let n = ref loc.line_start in
  while Char.equal file_contents.[!n] ' ' do
    Stdlib.incr n
  done;
  !n - loc.line_start
;;

let evaluate_test
      ~file_contents
      ~(location : File.Location.t)
      ~allow_output_patterns
      (test : Test_outcome.t)
  =
  let cr_for_multiple_outputs ~cr_body outputs =
    let prefix =
      Expect_test_config_types.Upon_unreleasable_issue.comment_prefix
        test.upon_unreleasable_issue
    in
    let cr = Printf.sprintf "(* %sexpect_test: %s *)" prefix cr_body in
    let sep = StringLabels.init (String.length cr) ~f:(fun _ -> '=') in
    Utils.list_intersperse (cr :: outputs) ~sep |> String.concat "\n"
  in
  let corrections =

    File.Location_map.to_alist test.expectations
    |> ListLabels.filter_map ~f:(fun (location, (expect : Fmt.t Cst.t Expectation.t)) ->
      let correction_for actual =
        let default_indent = indentation_at file_contents expect.body_location in
        match
          Reconcile.expectation_body
            ~expect:expect.body
            ~actual
            ~default_indent
            ~pad_single_line:(Option.is_some expect.tag)
            ~allow_output_patterns
        with
        | Match -> None
        | Correction c -> Some (expect, Test_correction.Node_correction.Correction c)
      in
      match File.Location_map.find location test.saved_output  with
      | exception Stdlib.Not_found ->
        (match expect.body with
         | Unreachable | Output -> None
         | Exact _ | Pretty _ ->
           Some (expect, Test_correction.Node_correction.Collector_never_triggered))
      | (One actual) -> correction_for actual
      | (Many_distinct outputs) ->
        let matches_expectation output = Option.is_none (correction_for output) in
        if ListLabels.for_all outputs ~f:matches_expectation
        then None
        else
          cr_for_multiple_outputs
            outputs
            ~cr_body:"Collector ran multiple times with different outputs"
          |> correction_for)
  in
  let trailing_output =
    let indent = location.start_pos - location.line_start + 2 in
    let actual =
      match test.trailing_output with
      | One actual -> actual
      | Many_distinct outputs ->
        cr_for_multiple_outputs
          outputs
          ~cr_body:"Test ran multiple times with different trailing outputs"
    in
    Reconcile.expectation_body
      ~expect:(Pretty Cst.empty)
      ~actual
      ~default_indent:indent
      ~pad_single_line:true
      ~allow_output_patterns
  in
  let uncaught_exn : Test_correction.Uncaught_exn.t =
    match test.uncaught_exn with
    | None ->
      (match test.uncaught_exn_expectation with
       | None -> Match
       | Some e -> Unused_expectation e)
    | Some x ->
      let indent = location.start_pos - location.line_start in
      let actual =
        match x with
        | One actual -> actual
        | Many_distinct outputs ->
          cr_for_multiple_outputs
            outputs
            ~cr_body:"Test ran multiple times with different uncaught exceptions"
      in
      let expect =
        match test.uncaught_exn_expectation with
        | None -> Expectation.Body.Pretty Cst.empty
        | Some e -> e.body
      in
      (match
         Reconcile.expectation_body
           ~expect
           ~actual
           ~default_indent:indent
           ~pad_single_line:true
           ~allow_output_patterns
       with
       | Match -> Match
       | Correction c ->
         (match test.uncaught_exn_expectation with
          | None -> Without_expectation c
          | Some e -> Correction (e, c)))
  in
  Test_correction.make ~location ~corrections ~uncaught_exn ~trailing_output
;;

type mode =
  | Inline_expect_test
  | Toplevel_expect_test

let output_slice buf s a b = Buffer.add_string buf (StringLabels.sub s ~pos:a ~len:(b - a))

let is_space = function
  | '\t' | '\011' | '\012' | '\r' | ' ' | '\n' -> true
  | _ -> false
;;

let rec output_semi_colon_if_needed buf file_contents pos =
  if pos >= 0
  then (
    match file_contents.[pos] with
    | c when is_space c -> output_semi_colon_if_needed buf file_contents (pos - 1)
    | ';' -> ()
    | _ -> Buffer.add_char buf ';')
;;

let split_lines s = Utils.split_on_chars s ~on:['\n']

let output_corrected buf ~file_contents ~mode test_corrections =
  let id_and_string_of_body : _ Expectation.Body.t -> string * string = function
    | Exact x -> "expect_exact", x
    | Output -> "expect.output", ""
    | Pretty x -> "expect", Cst.to_string x
    | Unreachable -> assert false
  in
  let output_body buf tag body =
    match tag with
    | None ->
      bprintf
        buf
        "\"%s\""
        (String.concat "\n"  (split_lines body |> ListLabels.map ~f:String.escaped))
    | Some tag ->
      let tag = Choose_tag.choose ~default:tag body in
      bprintf buf "{%s|%s|%s}" tag body tag
  in
  let ofs =
    ListLabels.fold_left
      test_corrections
      ~init:0
      ~f:(fun ofs (test_correction : Test_correction.t) ->
        let test_correction, to_skip =
          (* If we need to remove an [%%expect.uncaught_exn] node, start by adjusting the
             end position of the test. *)
          match test_correction.uncaught_exn with
          | Unused_expectation e ->
            (* Unfortunately, the OCaml parser doesn't give us the location of the whole
               extension point, so we have to find the square brackets ourselves :( *)
            let start = ref e.extid_location.start_pos in
            while not (Char.equal file_contents.[!start] '[') do
              if ofs >= !start
              then
                failwith "Cannot find '[' marking the start of [%expect.uncaught_exn]"
                (* raise_s
                  (Sexp.message
                     "Cannot find '[' marking the start of [%expect.uncaught_exn]"
                     [ "ofs", Int.sexp_of_t ofs
                     ; "start", Int.sexp_of_t e.extid_location.start_pos
                     ]) *);
              Stdlib.decr start
            done;
            while !start - 1 > ofs && is_space file_contents.[!start - 1] do
              Stdlib.decr start
            done;
            let file_len = String.length file_contents in
            let stop = ref e.body_location.end_pos in
            while !stop < file_len && not (Char.equal file_contents.[!stop] ']') do
              Stdlib.incr stop
            done;
            if  !stop >= file_len
            then
              failwith "Cannot find ']' marking the end of [%expect.uncaught_exn]";
              (* raise_s
                (Sexp.message
                   "Cannot find ']' marking the end of [%expect.uncaught_exn]"
                   [ "stop", Int.sexp_of_t e.body_location.end_pos ]); *)
            Stdlib.incr stop;
            let test_correction =
              { test_correction with
                location = { test_correction.location with end_pos = !start }
              }
            in
            test_correction, Some (!start, !stop)
          | Match | Without_expectation _ | Correction _ -> test_correction, None
        in
        let ofs =
          ListLabels.fold_left
            test_correction.corrections
            ~init:ofs
            ~f:(fun ofs (e, correction) ->
              match (correction : Test_correction.Node_correction.t) with
              | Collector_never_triggered ->
                output_slice buf file_contents ofs e.Expectation.extid_location.start_pos;
                bprintf buf "expect.unreachable";
                e.body_location.end_pos
              | Correction c ->
                let id, body = id_and_string_of_body c in
                output_slice buf file_contents ofs e.extid_location.start_pos;
                Buffer.add_string buf id;
                output_slice
                  buf
                  file_contents
                  e.extid_location.end_pos
                  e.body_location.start_pos;
                output_body buf e.tag body;
                e.body_location.end_pos)
        in
        let ofs =
          match test_correction.trailing_output with
          | Match -> ofs
          | Correction c ->
            let loc = test_correction.location in
            output_slice buf file_contents ofs loc.end_pos;
            if match mode with
              | Inline_expect_test -> true
              | Toplevel_expect_test -> false
            then output_semi_colon_if_needed buf file_contents loc.end_pos;
            let id, body = id_and_string_of_body c in
            (match mode with
             | Inline_expect_test ->
               let indent = loc.start_pos - loc.line_start + 2 in
               bprintf buf "\n%*s[%%%s " indent "" id
             | Toplevel_expect_test ->
               if loc.end_pos = 0 || not (Char.(equal ) file_contents.[loc.end_pos - 1] '\n')
               then Buffer.add_char buf '\n';
               bprintf buf "[%%%%%s" id);
            output_body buf (Some "") body;
            bprintf buf "]";
            loc.end_pos
        in
        let ofs =
          match test_correction.uncaught_exn with
          | Match -> ofs
          | Unused_expectation _ ->
            (* handled above *)
            ofs
          | Without_expectation c ->
            let loc = test_correction.location in
            output_slice buf file_contents ofs loc.end_pos;
            let indent = loc.start_pos - loc.line_start in
            bprintf buf "\n%*s[@@expect.uncaught_exn " indent "";
            output_body buf (Some "") (snd (id_and_string_of_body c));
            bprintf buf "]";
            loc.end_pos
          | Correction (e, c) ->
            output_slice buf file_contents ofs e.body_location.start_pos;
            output_body buf e.tag (snd (id_and_string_of_body c));
            e.body_location.end_pos
        in
        match to_skip with
        | None -> ofs
        | Some (start, stop) ->
          output_slice buf file_contents ofs start;
          stop)
  in
  output_slice buf file_contents ofs (String.length file_contents)
;;

let get_contents_for_corrected_file ~file_contents ~mode test_corrections =
  let buf = Buffer.create 4096 in
  output_corrected
    buf
    ~file_contents
    ~mode
    (ListLabels.sort test_corrections ~cmp:Test_correction.compare_locations);
  Buffer.contents buf
;;
