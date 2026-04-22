open SimpleAST
module Symex : Soteria.Symex.S
module Map : Soteria.Soteria_std.Map.S with type key = string
module Typed = Soteria.Tiny_values.Typed

type env = Typed.T.sint Typed.t Map.t

val symb_eval_stmt : env -> stmt -> (env, string, 'a) Symex.Result.t
