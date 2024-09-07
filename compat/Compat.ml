let chop_prefix_if_exists s ~prefix =
  if String.starts_with ~prefix s
  then (
    let plen = String.length prefix in
    String.sub s plen (String.length s - plen))
  else s
;;
