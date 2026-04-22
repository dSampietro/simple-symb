open SimpleAST
module Symex : Soteria.Symex.S
module Map : Soteria.Soteria_std.Map.S with type key = string
module Typed = Soteria.Tiny_values.Typed

type symb_int = Typed.T.sint Typed.t
type env = symb_int Map.t
type hist = (string * symb_int) list
type ok_state = { env : env; hist : hist }
type err_state = { msg : string; hist : hist }

val build_symb_process : stmt -> (ok_state, err_state, 'a) Symex.Result.t
