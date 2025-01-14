open Utils
open Expect_test_common
open Expect_test_matcher
module Test_result = Ppx_inline_test_lib.Test_result
module Collector_test_outcome = Expect_test_collector.Test_outcome

type group =
  { filename : File.Name.t
  ; file_contents : string
  ; tests : Matcher.Test_outcome.t File.Location_map.t
  }

let convert_collector_test ~allow_output_patterns (test : Collector_test_outcome.t)
  : File.Location.t * Matcher.Test_outcome.t
  =
  let saved_output =
    (* let _ = Map.of_alist_multi (module File.Location) test.saved_output in *)
    File.Location_map.of_alist_multi test.saved_output
    |> File.Location_map.map Matcher.Saved_output.of_nonempty_list_exn
  in
  let expectations =
    ListLabels.map test.expectations ~f:(fun (expect : Expectation.Raw.t) ->
      ( expect.extid_location
      , Expectation.map_pretty expect ~f:(Lexer.parse_pretty ~allow_output_patterns) ))
    |> File.Location_map.of_alist_exn
  in
  let uncaught_exn =
    match test.uncaught_exn with
    | None -> None
    | Some (exn, bt) ->
      let exn =
        try Exn.to_string exn with
        | exn ->
          let name =
            Stdlib.Obj.Extension_constructor.of_val exn
            |> Stdlib.Obj.Extension_constructor.name
          in
          Printf.sprintf "(\"%s(Cannot print more details, Exn.to_string failed)\")" name
      in
      Some
        (match Stdlib.Printexc.raw_backtrace_to_string bt with
         | "" -> exn
         | bt ->
           Expect_test_config_types.Upon_unreleasable_issue
           .message_when_expectation_contains_backtrace
             test.upon_unreleasable_issue
           ^ exn
           ^ "\n"
           ^ bt)
  in
  let uncaught_exn, trailing_output =
    match uncaught_exn, test.trailing_output with
    | None, _ | _, "" -> uncaught_exn, test.trailing_output
    | Some uncaught_exn, trailing_output ->
      ( Some
          (String.concat
             "\n"
             [ uncaught_exn; "Trailing output"; "---------------"; trailing_output ])
      , "" )
  in
  let uncaught_exn_expectation =
    test.uncaught_exn_expectation
    |> Option.map (fun expect ->
      Expectation.map_pretty expect ~f:(Lexer.parse_pretty ~allow_output_patterns))
  in
  ( test.location
  , { expectations
    ; saved_output
    ; trailing_output = Matcher.Saved_output.of_nonempty_list_exn [ trailing_output ]
    ; uncaught_exn =
        uncaught_exn |> Option.map (fun s ->
          Matcher.Saved_output.of_nonempty_list_exn [ s ])
    ; uncaught_exn_expectation
    ; upon_unreleasable_issue = test.upon_unreleasable_issue
    } )
;;

let dir_seps = '/' :: (if Sys.win32 then [ '\\'; ':' ] else [])

let resolve_filename filename =
  let relative_to =
    match Ppx_inline_test_lib.source_tree_root with
    | None -> File.initial_dir ()
    | Some root ->
      if Stdlib.Filename.is_relative root
      then (
        let initial_dir = File.initial_dir () in
        (* Simplification for the common case where [root] is of the form [(../)*..] *)
        let l = Utils.split_on_chars root ~on:dir_seps in
        if ListLabels.for_all l ~f:(String.equal Stdlib.Filename.parent_dir_name)
        then
          ListLabels.fold_left l ~init:initial_dir ~f:(fun dir _ -> Stdlib.Filename.dirname dir)
        else Stdlib.Filename.concat initial_dir root)
      else root
  in
  File.Name.relative_to ~dir:relative_to filename
;;




let create_group ~allow_output_patterns (filename, tests) =
  let module D = File.Digest in
  (* Stdlib.Printf.printf "tests length = %d, filename = %S\n" (List.length tests) filename; *)
  let expected_digest =
    match
      ListLabels.map tests ~f:(fun (t : Collector_test_outcome.t) -> t.file_digest)
      |> dedup_and_sort ~compare:D.compare
    with
    | [ digest ] -> digest
    | [] -> assert false
    | digests ->
      Printf.ksprintf
        failwith
        "Expect tests make inconsistent assumption about file \"%s\" %s"
        (File.Name.to_string filename)
        (Sexplib0.Sexp.to_string_hum
          (Sexplib.Conv.sexp_of_list D.sexp_of_t digests))
  in
  let file_contents = Utils.In_channel.read_all (resolve_filename filename) in
  let current_digest =
    Stdlib.Digest.string file_contents |> Stdlib.Digest.to_hex |> D.of_string
  in
  if D.compare expected_digest current_digest <> 0
  then
    Printf.ksprintf
      failwith
      "File \"%s\" changed, you need rebuild inline_tests_runner to be able to run \
       expect tests (expected digest: %s, current digest: %s)"
      (File.Name.to_string filename)
      (D.to_string expected_digest)
      (D.to_string current_digest);
  let tests =
    ListLabels.map tests ~f:(convert_collector_test ~allow_output_patterns)
    |> File.Location_map.of_alist_reduce ~f:Matcher.Test_outcome.merge_exn
  in
  { filename; file_contents; tests }
;;

module String_map : sig
include module type of Stdlib.Map.Make(String)
  val of_alist_multi: (key * 'b) list -> 'b list t
  val to_alist : 'v t -> (key * 'v) list
end = struct
  include Stdlib.Map.Make(String)
  (* val of_alist_multi: (key -> key -> int) -> (key * 'b) list -> 'b list t *)
  let of_alist_multi xs =
    Stdlib.List.fold_left (fun acc (k,v) ->
      try let vs = find k acc in add k (v::vs) acc
      with Stdlib.Not_found -> add k [v] acc
      ) empty xs

  let to_alist map =
    fold (fun key v acc -> (key, v) :: acc) map []
end

let convert_collector_tests ~allow_output_patterns tests : group list =
  (* Stdlib.Printf.printf "col_outcome length = %d\n" (List.length tests);
  Stdlib.List.iteri (fun i (test : Collector_test_outcome.t) ->
    Stdlib.Printf.printf "  %d. %S. %d expectations\n"
      i test.location.filename (List.length test.expectations);
  ) tests; *)
  tests
  |> Stdlib.List.map (fun (test : Collector_test_outcome.t) ->
    test.location.filename, test)
  |> String_map.of_alist_multi
  |> String_map.to_alist
  |> ListLabels.map ~f:(create_group ~allow_output_patterns)
;;

let process_group
      ~use_color
      ~in_place
      ~diff_command
      ~diff_path_prefix
      ~allow_output_patterns
      { filename; file_contents; tests }
  : Test_result.t
  =
  let bad_outcomes =
    File.Location_map.fold (fun location test acc ->
      match
        Matcher.evaluate_test ~file_contents ~location test ~allow_output_patterns
      with
      | Match -> acc
      | Correction c -> c :: acc) tests  []
    |> List.rev
  in
  let filename = resolve_filename filename in
  let dot_corrected = filename ^ ".corrected" in
  let remove file = if Stdlib.Sys.file_exists file then Stdlib.Sys.remove file in
  match bad_outcomes with
  | [] ->
    remove dot_corrected;
    Success
  | _ :: _ ->
    let next_contents =
      Matcher.get_contents_for_corrected_file
        ~file_contents
        ~mode:Inline_expect_test
        bad_outcomes
    in
    (match in_place with
     | true ->
       Out_channel.write_all filename ~data:next_contents;
       remove dot_corrected;
       Success
     | false ->
       (match diff_command with
        | Some "-" (* Just write the .corrected file - do not output a diff. *) ->
          Out_channel.write_all dot_corrected ~data:next_contents;
          Success
        | None | Some _ ->
          (* By invoking [Make_corrected_file.f] with a fresh temporary file, we avoid the
             following possible race between inline_test_runners A and B:
             1. A runs test T1 and generates next contents C1.
             2. B runs test T2 and generates next contents C2.
             3. A writes C1 to the .corrected file.
             4. B writes C2 to the .corrected file.
             5. A diffs the .corrected file against the original file and reports the
             result. It thinks it is reporting the diff produced by T1, but is in fact
             reporting the diff produced by T2. The key aspect of using temporary files is
             that even if in the above scenario the final contents of the .corrected file
             are C2, the diff reported by A comes from its tmp file and will still be the
             diff produced by T1. *)
          let tmp_corrected =
            Stdlib.Filename.temp_file
              (Stdlib.Filename.basename filename)
              ".corrected.tmp"
              ~temp_dir:(Stdlib.Filename.dirname filename)
          in
          let (Ok () | Error (_ : Make_corrected_file.error)) =
            Make_corrected_file.f
              ~corrected_path:tmp_corrected
              ~use_color
              ?diff_command
              ?diff_path_prefix
              ~next_contents
              ~path:filename
              ()
          in
          Stdlib.Sys.rename tmp_corrected dot_corrected;
          Failure))
;;

let evaluate_tests
      ~use_color
      ~in_place
      ~diff_command
      ~diff_path_prefix
      ~allow_output_patterns
  =
  convert_collector_tests (Expect_test_collector.tests_run ()) ~allow_output_patterns
  |> ListLabels.map ~f:(fun group ->
    match
      process_group
        ~use_color
        ~in_place
        ~diff_command
        ~diff_path_prefix
        ~allow_output_patterns
        group
    with
    | exception _exn ->
      let _bt = Stdlib.Printexc.get_raw_backtrace () in
      failwith "Expect test evaluator bug..."
      (* TODO: make error message more pretty *)
      (* raise_s
        (Sexplib0.Sexp.message
           "Expect test evaluator bug"
           [ "exn", Sexplib0.Conv.sexp_of_exn exn
           ; "backtrace", Atom (Stdlib.Printexc.raw_backtrace_to_string bt)
           ; "filename", File.Name.sexp_of_t group.filename
           ]) *)
    | res -> res)
  |> Test_result.combine_all
;;

let () =
  Ppx_inline_test_lib.add_evaluator ~f:(fun () ->
    evaluate_tests
      ~use_color:Ppx_inline_test_lib.use_color
      ~in_place:Ppx_inline_test_lib.in_place
      ~diff_command:Ppx_inline_test_lib.diff_command
      ~diff_path_prefix:Ppx_inline_test_lib.diff_path_prefix
      ~allow_output_patterns:false)
;;
