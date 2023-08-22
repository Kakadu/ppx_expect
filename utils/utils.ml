module Exn = struct
  let to_string = Printexc.to_string
end

include (
  struct
    open StringLabels

    type t = string

    let rec char_list_mem l (c : char) =
      match l with
      | [] -> false
      | hd :: tl -> Char.equal hd c || char_list_mem tl c

    let split_gen str ~on =
      let is_delim =
        match on with
        | `char c' -> fun c -> Char.equal c c'
        | `char_list l -> fun c -> char_list_mem l c
      in
      let len = String.length str in
      let rec loop acc last_pos pos =
        if pos = -1 then sub str ~pos:0 ~len:last_pos :: acc
        else if is_delim str.[pos] then
          let pos1 = pos + 1 in
          let sub_str = sub str ~pos:pos1 ~len:(last_pos - pos1) in
          loop (sub_str :: acc) pos (pos - 1)
        else loop acc last_pos (pos - 1)
      in
      loop [] len (len - 1)

    (* let split str ~on = split_gen str ~on:(`char on) *)
    let split_on_chars str ~on:chars = split_gen str ~on:(`char_list chars)

    let rfindi ?pos t ~f =
      let rec loop i =
        if i < 0 then None else if f i t.[i] then Some i else loop (i - 1)
      in
      let pos =
        match pos with Some pos -> pos | None -> String.length t - 1
      in
      loop pos

    let prefix s n = String.sub s 0 n
    let last_non_drop ~drop t = rfindi t ~f:(fun _ c -> not (drop c))

    let is_whitespace = function
      | '\t' | '\n' | '\011' (* vertical tab *)
      | '\012' (* form feed *)
      | '\r' | ' ' ->
          true
      | _ -> false

    let string_rstrip ?(drop = is_whitespace) t =
      match last_non_drop t ~drop with
      | None -> ""
      | Some i -> if i = String.length t - 1 then t else prefix t (i + 1)

    let is_prefix s ~prefix =
      String.length prefix <= String.length s && String.starts_with s ~prefix

    let string_prefix s len =
      if String.length s < len then s else String.sub s 0 len

    let string_is_empty s = String.length s = 0

    let string_drop_prefix s len =
      let slen = String.length s in
      if len > slen then "" else String.sub s len (slen - len)
  end :
    sig
      type t = string

      val split_on_chars : t -> on:char list -> t list
      val string_rstrip : ?drop:(char -> bool) -> t -> t
      val is_prefix : string -> prefix:string -> bool
      val string_is_empty : string -> bool
      val string_prefix : string -> int -> string
      val string_drop_prefix : string -> int -> string
    end)

let raise_with_original_backtrace = Stdlib.Printexc.raise_with_backtrace

exception Finally of exn * exn

let protectx ~f x ~(finally : _ -> unit) =
  match f x with
  | res ->
      finally x;
      res
  | exception exn -> (
      let bt = Stdlib.Printexc.get_raw_backtrace () in
      match finally x with
      | () -> raise_with_original_backtrace exn bt
      | exception final_exn ->
          (* Unfortunately, the backtrace of the [final_exn] is discarded here. *)
          raise_with_original_backtrace (Finally (exn, final_exn)) bt)

module In_channel = struct
  include Stdlib.In_channel

  let create ?(binary = true) file =
    let flags = [ Open_rdonly ] in
    let flags = if binary then Open_binary :: flags else flags in
    Stdlib.open_in_gen flags 0o000 file

  let with_file ?binary file ~f =
    protectx (create ?binary file) ~f ~finally:close

  let read_all fname = with_file fname ~f:input_all
end

module Out_channel = struct
  include Stdlib.Out_channel

  let create ?(binary = true) ?(append = false) ?(fail_if_exists = false)
      ?(perm = 0o666) file =
    let flags = [ Open_wronly; Open_creat ] in
    let flags = (if binary then Open_binary else Open_text) :: flags in
    let flags = (if append then Open_append else Open_trunc) :: flags in
    let flags = if fail_if_exists then Open_excl :: flags else flags in
    Stdlib.open_out_gen flags perm file

  let with_file ?binary ?append ?fail_if_exists ?perm file ~f =
    protectx
      (create ?binary ?append ?fail_if_exists ?perm file)
      ~f ~finally:close

  let write_all file ~data = with_file file ~f:(fun t -> output_string t data)
end

include struct
  open ListLabels

  (* returns list without adjacent duplicates *)
  let remove_consecutive_duplicates ?(which_to_keep = `Last) list ~equal =
    let rec loop to_keep accum = function
      | [] -> to_keep :: accum
      | hd :: tl ->
          if equal hd to_keep then
            let to_keep =
              match which_to_keep with `First -> to_keep | `Last -> hd
            in
            loop to_keep accum tl
          else loop hd (to_keep :: accum) tl
    in
    match list with [] -> [] | hd :: tl -> rev (loop hd [] tl)

  (** returns sorted version of list with duplicates removed *)
  let dedup_and_sort list ~compare =
    match list with
    | [] | [ _ ] -> list (* performance hack *)
    | _ ->
        let equal x x' = compare x x' = 0 in
        let sorted = sort ~cmp:compare list in
        (remove_consecutive_duplicates ~equal sorted [@nontail])
end

let list_is_empty = function [] -> true | _ -> false

let list_intersperse t ~sep =
  match t with
  | [] -> []
  | x :: xs ->
      x :: ListLabels.fold_right xs ~init:[] ~f:(fun y acc -> sep :: y :: acc)
