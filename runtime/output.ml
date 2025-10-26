module String = struct
  include StringLabels

  let rec char_list_mem l (c : char) =
    match l with
    | [] -> false
    | hd :: tl -> Char.equal hd c || char_list_mem tl c
  ;;

  let split_gen str ~on =
    let is_delim =
      match on with
      | `char c' -> fun c -> Char.equal c c'
      | `char_list l -> fun c -> char_list_mem l c
    in
    let len = String.length str in
    let rec loop acc last_pos pos =
      if pos = -1
      then sub str ~pos:0 ~len:last_pos :: acc
      else if is_delim str.[pos]
      then (
        let pos1 = pos + 1 in
        let sub_str = sub str ~pos:pos1 ~len:(last_pos - pos1) in
        loop (sub_str :: acc) pos (pos - 1))
      else loop acc last_pos (pos - 1)
    in
    loop [] len (len - 1)
  ;;

  let split_on_char str ~on = split_gen str ~on:(`char on)

  (* let split str ~on = split_gen str ~on:(`char on) *)
  (* let split str ~on:chars = split_gen str ~on:(`char_list chars) *)
end

open Types

module Type = struct
  type t =
    | Exact
    | Pretty
end

module Reconciled = struct
  type t = string

  let compare = Stdlib.String.compare
end

module Formatted = struct
  type t = string
end

module Formatter = struct
  type t = string -> Formatted.t

  let create format = format
  let apply format str = format str
end

module Test_result = struct
  type t =
    | Pass
    | Fail of Reconciled.t

  let compare a b =
    match a, b with
    | Pass, Pass -> 0
    | Pass, _ -> -1
    | _, Pass -> 1
    | Fail a, Fail b -> Reconciled.compare a b
  ;;
end

module Payload = struct
  type t =
    { contents : string
    ; tag : String_node_format.Delimiter.t
    }

  let default contents = { contents; tag = String_node_format.Delimiter.default }

  let to_source_code_string { contents; tag } =
    let escape_lines test_output =
      test_output
      |> String.split_on_char ~on:'\n'
      |> List.map String.escaped
      |> StringLabels.concat ~sep:"\n"
    in
    match tag with
    | T (Tag tag) -> Printf.sprintf "{%s|%s|%s}" tag contents tag
    | T Quote -> Printf.sprintf {|"%s"|} (escape_lines contents)
  ;;
end

let reconcile ~expected_output ~test_output : Test_result.t =
  if String.equal expected_output test_output then Pass else Fail test_output
;;

let fail error_output : Test_result.t = Fail error_output

let fix_delimiter_conflicts
      (type handedness)
      ~contents
      ~(delimiter : handedness String_node_format.Delimiter.unpacked)
  : handedness String_node_format.Delimiter.unpacked
  =
  let rec fix_tag_conflicts ~contents ~tag =
    let tag_conflicts_with fstr =
      Base.String.is_substring ~substring:(Printf.sprintf fstr tag) contents
    in
    if tag_conflicts_with "{%s|" || tag_conflicts_with "|%s}"
    then fix_tag_conflicts ~contents ~tag:(tag ^ "xxx")
    else tag
  in
  match delimiter with
  | Quote -> Quote
  | Tag tag -> Tag (fix_tag_conflicts ~contents ~tag)
;;

let to_formatted_payload ~tag:(T delimiter : String_node_format.Delimiter.t) contents
  : Payload.t
  =
  { contents; tag = T (fix_delimiter_conflicts ~contents ~delimiter) }
;;

let to_source_code_string
      ~(expect_node_formatting : Expect_node_formatting.t)
      ~node_shape:(T shape : String_node_format.Shape.t)
      ~(tag : String_node_format.Delimiter.t)
      contents
  =
  let delimiter =
    fix_delimiter_conflicts
      ~contents
      ~delimiter:(String_node_format.Delimiter.handed tag shape.hand)
  in
  let payload : Payload.t = { contents; tag = T delimiter } in
  match shape.hand with
  | Longhand ->
    let prefix =
      match shape.kind with
      | Extension -> expect_node_formatting.extension_sigil
      | Attribute -> expect_node_formatting.attribute_sigil
    in
    Printf.sprintf "[%s%s %s]" prefix shape.name (Payload.to_source_code_string payload)
  | Shorthand ->
    let prefix =
      match shape.kind with
      | Extension -> expect_node_formatting.extension_sigil
    in
    (match delimiter with
     | Tag "" -> Printf.sprintf "{%s%s|%s|}" prefix shape.name contents
     | Tag tag -> Printf.sprintf "{%s%s %s|%s|%s}" prefix shape.name tag contents tag)
;;
