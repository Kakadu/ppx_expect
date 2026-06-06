



module Library = Build_info.V1.Statically_linked_library
(*
let library_version lib_name =
  let libs = Build_info.V1.Statically_linked_libraries.to_list () in
  try
    let lib = List.find (fun l -> String.equal (Library.name l) lib_name) libs in
    Library.version lib
  with Not_found -> None *)

(* [@@@ocaml.warning "-32"]
let has_version lib_name required_version =
  match library_version lib_name with
  | None -> false
  | Some v -> v >= required_version
 *)
(* let feature_available feature_name required_lib required_version =
  has_version required_lib required_version *)

let version_string = function
| None -> "n/a"
| Some v -> Build_info.V1.Version.to_string v

let () =
  let version = Build_info.V1.version () in
  Printf.printf "version: %s\n" (version_string version);

  let libs = Build_info.V1.Statically_linked_libraries.to_list () in
  Printf.printf "statically linked libraries:\n";
  List.iter (fun lib ->
    let name = Build_info.V1.Statically_linked_library.name lib in
    let version = Build_info.V1.Statically_linked_library.version lib in
    Printf.printf "- %s (%s)\n" name (version_string version)
  ) libs;
  let ppxlib = List.find (fun l -> String.equal (Library.name l) "ppxlib") libs in
  let version = Library.version ppxlib |> Option.get |> Build_info.V1.Version.to_string in
  let () =
    match OpamVersionCompare.compare version "0.37.0" with
    | 1 | 0 ->
      Out_channel.with_open_text "ge38.txt" (fun ch -> output_string ch "true");
      Out_channel.with_open_text "lt38.txt" (fun ch -> output_string ch "false")

    | _ ->
      Out_channel.with_open_text "ge38.txt" (fun ch -> output_string ch "false");
      Out_channel.with_open_text "lt38.txt" (fun ch -> output_string ch "true")

  in
  (* Out_channel.with_open_text filename (fun _ -> ()); *)
  Out_channel.with_open_text "runned.txt" (fun ch -> output_string ch "true");
  ()