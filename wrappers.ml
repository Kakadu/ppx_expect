let compare_int = Int.compare

module Comparable = struct
  let lift cmp ~f x y = cmp (f x) (f y)

  (* compare [x] and [y] lexicographically using functions in the list [cmps] *)
  let lexicographic cmps x y =
    let rec loop = function
      | cmp :: cmps ->
        let res = cmp x y in
        if res = 0 then loop cmps else res
      | [] -> 0
    in
    loop cmps
  ;;
end

module Fn = struct
  let id = Fun.id
end

module Char = struct
  include Char

  let is_whitespace = function
    | '\t' | '\n' | '\011' (* vertical tab *) | '\012' (* form feed *) | '\r' | ' ' ->
      true
    | _ -> false
  ;;

  let ( = ) : char -> char -> bool = equal
  let ( <> ) l r = not (l = r)
end

module List = struct
  include ListLabels

  let take_while ~f:p l =
    let[@tail_mod_cons] rec aux = function
      | x :: l when p x -> x :: aux l
      | _rest -> []
    in
    aux l
  ;;

  let rec drop_while ~f:p = function
    | x :: l when p x -> drop_while ~f:p l
    | rest -> rest
  ;;

  let is_empty = function
    | [] -> true
    | _ -> false
  ;;

  let min_elt ~compare = function
    | [] -> None
    | h :: tl ->
      Some
        (fold_left
           ~f:(fun acc x ->
             match compare acc x with
             | -1 | 0 -> acc
             | _ -> x)
           ~init:h
           tl)
  ;;

  let fold_map t ~init ~f =
    let acc = ref init in
    let result =
      map t ~f:(fun x ->
        let new_acc, y = f !acc x in
        acc := new_acc;
        y)
    in
    !acc, result
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

  let sort l ~compare = Stdlib.ListLabels.sort l ~cmp:compare
end

module String = struct
  include StringLabels

  let chop_prefix_if_exists path ~prefix =
    if starts_with ~prefix path
    then StringLabels.sub path ~pos:(length prefix) ~len:(length path - length prefix)
    else path
  ;;

  let rfindi ?pos t ~f =
    let rec loop i = if i < 0 then None else if f i t.[i] then Some i else loop (i - 1) in
    let pos =
      match pos with
      | Some pos -> pos
      | None -> String.length t - 1
    in
    loop pos
  ;;

  let prefix s n = String.sub s 0 n
  let last_non_drop ~drop t = rfindi t ~f:(fun _ c -> not (drop c))

  let is_whitespace = function
    | '\t' | '\n' | '\011' (* vertical tab *) | '\012' (* form feed *) | '\r' | ' ' ->
      true
    | _ -> false
  ;;

  let rstrip ?(drop = is_whitespace) t =
    match last_non_drop t ~drop with
    | None -> ""
    | Some i -> if i = String.length t - 1 then t else prefix t (i + 1)
  ;;

  let is_empty = ( = ) ""

  let to_list s =
    let rec loop acc i = if i < 0 then acc else loop (s.[i] :: acc) (i - 1) in
    loop [] (length s - 1)
  ;;

  let split_lines =
    let back_up_at_newline ~t ~pos ~eol =
      pos := !pos - if !pos > 0 && Char.equal t.[!pos - 1] '\r' then 2 else 1;
      eol := !pos + 1
    in
    fun t ->
      let n = length t in
      if n = 0
      then []
      else (
        (* Invariant: [-1 <= pos < eol]. *)
        let pos = ref (n - 1) in
        let eol = ref n in
        let ac = ref [] in
        (* We treat the end of the string specially, because if the string ends with a
           newline, we don't want an extra empty string at the end of the output. *)
        if Char.equal t.[!pos] '\n' then back_up_at_newline ~t ~pos ~eol;
        while !pos >= 0 do
          if Char.( <> ) t.[!pos] '\n'
          then decr pos
          else (
            (* Because [pos < eol], we know that [start <= eol]. *)
            let start = !pos + 1 in
            ac := sub t ~pos:start ~len:(!eol - start) :: !ac;
            back_up_at_newline ~t ~pos ~eol)
        done;
        sub t ~pos:0 ~len:!eol :: !ac)
  ;;

  let split_on_char ~on s = String.split_on_char on s
  let strip = Stdlib.String.trim (* they could be not equivalent *)
  let concat ?(sep = "") xs = String.concat sep xs
  let sub = StringLabels.sub

  let subo ?(pos = 0) ?len src =
    sub
      src
      ~pos
      ~len:
        (match len with
         | Some i -> i
         | None -> length src - pos)
  ;;

  include (
  struct
    let substr_index_gen ~case_sensitive ?pos t ~pattern =
      Search_patternW.index ?pos (Search_patternW.create ~case_sensitive pattern) ~in_:t
    ;;

    let is_substring_gen ~case_sensitive t ~substring =
      Option.is_some (substr_index_gen t ~pattern:substring ~case_sensitive)
    ;;

    let is_substring = is_substring_gen ~case_sensitive:true
  end :
  sig
    val is_substring : t -> substring:t -> bool
  end)

  let count s ~f:is_good =
    String.fold_left (fun acc c -> if is_good c then acc + 1 else acc) 0 s
  ;;
end

module Option = struct
  include Option

  let to_list = function
    | Some x -> [ x ]
    | None -> []
  ;;

  let bind x ~f = Option.bind x f

  let value x ~default =
    match x with
    | Some x -> x
    | None -> default
  ;;

  let map x ~f =
    match x with
    | None -> None
    | Some x -> Some (f x)
  ;;

  let value_exn = Option.get

  let iter ~f = function
    | Some x -> f x
    | None -> ()
  ;;
end

module Staged = struct
  type 'a t = 'a

  let stage = Fn.id
  let unstage = Fn.id
end

module Source_code_position = struct
  type t = Lexing.position

  (* This is the same function as Ppx_here.lift_position_as_string. *)
  let make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol =
    String.concat
      ~sep:""
      [ pos_fname; ":"; Int.to_string pos_lnum; ":"; Int.to_string (pos_cnum - pos_bol) ]
  ;;

  let to_string { Stdlib.Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol } =
    make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol
  ;;
end

module Queue = struct
  include Queue

  let enqueue q x = add x q

  let to_list t =
    let acc = ref [] in
    Queue.iter (fun x -> acc := x :: !acc) t;
    List.rev !acc
  ;;
  (* let result = ref [] in
     for i = t.length - 1 downto 0 do
     result := unsafe_get t i :: !result
     done;
     !result *)
end
