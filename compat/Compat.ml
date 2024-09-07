module Char = struct
  include Char

  let ( = ) = equal

  let is_whitespace = function
    | '\t' | '\n' | '\011' (* vertical tab *) | '\012' (* form feed *) | '\r' | ' ' ->
      true
    | _ -> false
  ;;
end

module String = struct
  include String

  let hash = Hashtbl.hash

  let chop_prefix_if_exists s ~prefix =
    if String.starts_with ~prefix s
    then (
      let plen = String.length prefix in
      String.sub s plen (String.length s - plen))
    else s
  ;;

  let rstrip ~drop s =
    let len = length s in
    let rec loop i =
      if i < 0 then "" else if drop s.[i] then loop (i - 1) else String.sub s 0 (i + 1)
    in
    loop (len - 1)
  ;;

  let count ~f s =
    let rec loop acc i len =
      if i >= len
      then acc
      else if f s.[i]
      then loop (acc + 1) (i + 1) len
      else loop acc (i + 1) len
    in
    loop 0 0 (String.length s)
  ;;

  let to_list s = String.fold_right List.cons s []
  let is_empty = ( = ) ""
  let concat ?(sep = "") xs = Stdlib.String.concat sep xs

  let is_substring ~substring s =
    let re = Str.regexp_string substring in
    try
      (* TODO: rewrite without Str module *)
      ignore (Str.search_forward re s 0);
      true
    with
    | Not_found -> false
  ;;

  let escaped _ = assert false

  let rfindi ?pos t ~f =
    let rec loop i = if i < 0 then None else if f i t.[i] then Some i else loop (i - 1) in
    let pos =
      match pos with
      | Some pos -> pos
      | None -> String.length t - 1
    in
    loop pos
  ;;

  let lfindi ?pos t ~f =
    let rec loop i =
      if i >= String.length t then None else if f i t.[i] then Some i else loop (i + 1)
    in
    let pos =
      match pos with
      | Some pos -> pos
      | None -> 0
    in
    loop pos
  ;;

  let last_non_drop ~drop t = rfindi t ~f:(fun _ c -> not (drop c))
  let prefix s n = String.sub s 0 n

  let string_rstrip ?(drop = Char.is_whitespace) t =
    match last_non_drop t ~drop with
    | None -> ""
    | Some i -> if i = String.length t - 1 then t else prefix t (i + 1)
  ;;

  let split_lines xs =
    (* TODO: \r\n should be supported too *)
    String.split_on_char '\n' xs
  ;;

  let split ~on = split_on_char on

  let strip s =
    match rfindi s ~f:(fun _ c -> not (Char.is_whitespace c)) with
    | None -> ""
    | Some rindex ->
      let rec lfind i =
        if i >= rindex
        then assert false
        else if Char.is_whitespace s.[i]
        then lfind (i + 1)
        else i
      in
      let left = lfind 0 in
      String.sub s left (rindex - left + 1)
  ;;

  let () = assert (strip " hello " = "hello")
  let sub = StringLabels.sub

  (* From base *)
  let subo ?(pos = 0) ?len src =
    sub
      src
      ~pos
      ~len:
        (match len with
         | Some i -> i
         | None -> length src - pos)
  ;;
end

module List = struct
  include ListLabels

  let is_empty = function
    | [] -> true
    | _ -> false
  ;;

  let fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t =
    fun xs ~init ~f ->
    let acc, xs =
      List.fold_left
        (fun (acc, xs) x ->
          let acc, x = f acc x in
          acc, x :: xs)
        (init, [])
        xs
    in
    acc, List.rev xs
  ;;

  let take_while : 'a t -> f:('a -> bool) -> 'a t =
    fun xs ~f ->
    let rec loop acc = function
      | x :: xs when f x -> loop (x :: acc) xs
      | [] | _ :: _ -> List.rev acc
    in
    loop [] xs
  ;;

  let drop_while : 'a t -> f:('a -> bool) -> 'a t =
    fun xs ~f ->
    let rec loop = function
      | x :: xs when f x -> loop xs
      | ([] | _ :: _) as rez -> rez
    in
    loop xs
  ;;

  let concat_mapi l ~f =
    let[@tail_mod_cons] rec outer_loop pos = function
      | [] -> []
      | [ hd ] -> (f [@tailcall false]) pos hd
      | hd :: (_ :: _ as tl) -> inner_loop (pos + 1) (f pos hd) tl
    and[@tail_mod_cons] inner_loop pos l1 l2 =
      match l1 with
      | [] -> outer_loop pos l2
      | [ x1 ] -> x1 :: outer_loop pos l2
      | [ x1; x2 ] -> x1 :: x2 :: outer_loop pos l2
      | [ x1; x2; x3 ] -> x1 :: x2 :: x3 :: outer_loop pos l2
      | [ x1; x2; x3; x4 ] -> x1 :: x2 :: x3 :: x4 :: outer_loop pos l2
      | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
        x1 :: x2 :: x3 :: x4 :: x5 :: inner_loop pos tl l2
    in
    (outer_loop 0 l [@nontail])
  ;;

  (* returns list without adjacent duplicates *)
  let remove_consecutive_duplicates ?(which_to_keep = `Last) list ~equal =
    let rec loop to_keep accum = function
      | [] -> to_keep :: accum
      | hd :: tl ->
        if equal hd to_keep
        then (
          let to_keep =
            match which_to_keep with
            | `First -> to_keep
            | `Last -> hd
          in
          loop to_keep accum tl)
        else loop hd (to_keep :: accum) tl
    in
    match list with
    | [] -> []
    | hd :: tl -> rev (loop hd [] tl)
  ;;

  (** returns sorted version of list with duplicates removed *)
  let dedup_and_sort list ~compare =
    match list with
    | [] | [ _ ] -> list (* performance hack *)
    | _ ->
      let equal x x' = compare x x' = 0 in
      let sorted = sort ~cmp:compare list in
      (remove_consecutive_duplicates ~equal sorted [@nontail])
  ;;

  let min_elt ~compare = function
    | [] -> None
    | x :: xs ->
      let rec helper min = function
        | [] -> Some min
        | x :: xs when compare x min < 0 -> helper x xs
        | _ :: xs -> helper min xs
      in
      helper x xs
  ;;

  let sort ~compare:cmp xs = ListLabels.sort ~cmp xs

  module Assoc = struct
    type ('a, 'b) t = ('a * 'b) list
  end
end

module Stdio = struct
  module In_channel = struct
    include In_channel

    let with_file file ~f = with_open_text file f
  end

  module Out_channel = struct
    include Out_channel

    let write_all file ~data = with_open_text file (fun ch -> output_string ch data)
  end
end

module Sys = struct
  include Sys

  let getenv = getenv_opt
end

exception Sexp_exception of Sexplib0.Sexp.t

let raise_s s = raise (Sexp_exception s)
let sexp_of_string s = Sexplib0.Sexp.Atom s

module Sexp = struct
  let message : string -> (string * Sexplib0.Sexp.t) list -> Sexplib0.Sexp.t =
    fun name lst ->
    Sexplib0.Sexp.(List (Atom name :: List.map ~f:(fun (k, v) -> List [ Atom k; v ]) lst))
  ;;
end

module Queue = struct
  include Queue

  let to_list q = Queue.fold (fun acc x -> x :: acc) [] q |> List.rev
  let enqueue q x = add x q
end

module Comparable = struct
  let lift : ('a -> 'a -> 'result) -> f:('b -> 'a) -> 'b -> 'b -> 'result =
    fun cmp ~f a b -> cmp (f a) (f b)
  ;;

  let lexicographic : ('a -> 'a -> int) list -> 'a -> 'a -> int =
    let rec helper a b = function
      | cmp :: cmps ->
        let res = cmp a b in
        if res = 0 then helper a b cmps else res
      | [] -> 0
    in
    fun cmps a b -> helper a b cmps
  ;;
end

module Source_code_position = struct
  type t = Lexing.position

  let to_string { Lexing.pos_fname; pos_lnum; pos_cnum; _ } =
    Printf.sprintf "%s:%d:%d" pos_fname pos_lnum pos_cnum
  ;;
end

module Option = struct
  include Option

  let map x ~f = map f x
  let iter x ~f = iter f x
  let bind x ~f = bind x f

  let value_exn ?error x =
    match x with
    | Some x -> x
    | None ->
      let msg =
        match error with
        | Some s -> s
        | None -> "No value"
      in
      raise_s (Sexplib.Conv.sexp_of_string msg)
  ;;
end

(* From Base *)
module Staged = struct
  type 'a t = 'a

  (* We define these primitives using ['a] instead of ['a t] as proof to ourselves that this
     is a safe use of [%identity], since [external] provides no typechecking. *)
  external stage : 'a -> 'a = "%identity"
  external unstage : 'a -> 'a = "%identity"
end

module Exn = struct
  open Sexplib

  let to_string exc = Sexp.to_string_hum ~indent:2 (Sexplib0.Sexp_conv.sexp_of_exn exc)
end

let compare_int = Int.compare
let compare_string = String.compare

module Merge_into_action = struct
  type 'data t =
    | Remove
    | Set_to of 'data
end

module Hashtbl_make (Key : Hashtbl.HashedType) = struct
  include Hashtbl.Make (Key)

  (* Every key in src will be removed or set in dst according to the return value of f. *)
  let merge_into
    :  src:'a t -> dst:'b t -> f:(key:key -> 'a -> 'b option -> 'b Merge_into_action.t)
    -> unit
    =
    fun ~src ~dst ~f ->
    iter
      (fun key v ->
        let bval = find_opt dst key in
        match f ~key v bval with
        | Remove -> remove dst key
        | Set_to d -> replace dst key d)
      src
  ;;

  let update tbl key ~f =
    match find tbl key with
    | x -> replace tbl key (f (Some x))
    | exception Not_found -> replace tbl key (f None)
  ;;

  let of_alist_exn : (key * 'b) list -> 'b t =
    fun xs ->
    let h = create (List.length xs) in
    List.iter ~f:(fun (k, v) -> add h k v) xs;
    h
  ;;

  let to_alist h = fold (fun key v acc -> (key, v) :: acc) h []
  let data h = to_seq_values h |> List.of_seq
end

module Intable = struct
  module type S = sig
    type t

    val to_int_exn : t -> int
    val of_int_exn : int -> t
  end
end
