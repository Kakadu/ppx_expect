open Ppxlib
open Ast_pattern

let value_binding ~expr ~pat =
  value_binding
    ~pat
    ~expr