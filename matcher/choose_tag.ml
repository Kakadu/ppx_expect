

let choose ~default body =
  let terminators = Lexer.extract_quoted_string_terminators body in
  let rec loop tag =
    if Stdlib.List.mem  tag terminators   then loop (tag ^ "x") else tag
  in
  if Stdlib.List.mem default terminators
  then loop (if 0 = String.length default then "xxx" else default ^ "_xxx")
  else default
;;
