open SimpleAST
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
module Typed = Soteria.Tiny_values.Typed
module Compo_res = Soteria.Symex.Compo_res
module Map = Soteria.Soteria_std.Map.Make (Soteria.Soteria_std.String)
open Symex.Syntax
open Typed.Infix
open Typed.Syntax

type env = Typed.T.sint Typed.t Map.t

let rec symb_eval_aexpr env = function
  | Int n -> Symex.Result.ok (Typed.int n)
  | Var x -> (
      match Map.find_opt x env with
      | Some v -> Symex.Result.ok v
      | None -> Symex.Result.error (Fmt.str "Variable %s not found" x))
  | NonDet ->
      let* v = Symex.nondet Typed.t_int in
      Symex.Result.ok v
  | AOp (expr1, op, expr2) -> (
      let** v1 = symb_eval_aexpr env expr1 in
      let** v2 = symb_eval_aexpr env expr2 in
      match op with
      | Add -> Symex.Result.ok (v1 +@ v2)
      | Sub -> Symex.Result.ok (v1 -@ v2)
      | Mul -> Symex.Result.ok (v1 *@ v2)
      | Div ->
          if%sat Typed.not (v2 ==@ 0s) then
            let v2 = Typed.cast v2 in
            Symex.Result.ok (v1 /@ v2)
          else Symex.Result.ok (Typed.int 0))

and symb_eval_bexpr env = function
  | Bool b -> Symex.Result.ok (Typed.of_bool b)
  | Not bexpr ->
      let** v = symb_eval_bexpr env bexpr in
      Symex.Result.ok (Typed.not v)
  | BOp (bexpr1, op, bexpr2) -> (
      let** v1 = symb_eval_bexpr env bexpr1 in
      let** v2 = symb_eval_bexpr env bexpr2 in
      match op with
      | And -> Symex.Result.ok (v1 &&@ v2)
      | Or -> Symex.Result.ok (v1 ||@ v2))
  | COp (aexpr1, op, aexpr2) -> (
      let** v1 = symb_eval_aexpr env aexpr1 in
      let** v2 = symb_eval_aexpr env aexpr2 in
      match op with
      | Eq -> Symex.Result.ok (v1 ==@ v2)
      | Neq -> Symex.Result.ok (Typed.not (v1 ==@ v2))
      | Lt -> Symex.Result.ok (v1 <@ v2)
      | Le -> Symex.Result.ok (v1 <=@ v2)
      | Gt -> Symex.Result.ok (v1 >@ v2)
      | Ge -> Symex.Result.ok (v1 >=@ v2))

let rec symb_eval_stmt env = function
  | Skip -> Symex.Result.ok env
  | Assign (x, aexpr) ->
      let** v = symb_eval_aexpr env aexpr in
      Symex.Result.ok (Map.add x v env)
  | Seq (stmt1, stmt2) ->
      let** env = symb_eval_stmt env stmt1 in
      symb_eval_stmt env stmt2
  | If (bexpr, then_stmt, else_stmt) ->
      let** cond = symb_eval_bexpr env bexpr in
      if%sat cond then symb_eval_stmt env then_stmt
      else symb_eval_stmt env else_stmt
  | While (bexpr, stmt) ->
      let** cond = symb_eval_bexpr env bexpr in
      if%sat cond then
        let** env = symb_eval_stmt env stmt in
        symb_eval_stmt env (While (bexpr, stmt))
      else Symex.Result.ok env
  | Assume bexpr ->
      let** cond = symb_eval_bexpr env bexpr in
      let* () = Symex.assume [ cond ] in
      Symex.Result.ok env
  | Assert bexpr ->
      let** cond = symb_eval_bexpr env bexpr in
      (* In OX mode, result = true iff not cond is UNSAT *)
      let* result = Symex.assert_ cond in
      if result then Symex.Result.ok env
      else Symex.Result.error (Fmt.str "Assertion failed: %a" Typed.ppa cond)
