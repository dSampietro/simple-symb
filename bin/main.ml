let mode = Soteria.Symex.Approx.OX

let string_of_state (result, pc) =
  let open SimpleLib.SymbInterpreter in
  let string_of_env env =
    env |> Map.bindings
    |> List.map (fun (x, v) -> Fmt.str "%s -> %a" x Typed.ppa v)
    |> String.concat "\n  "
  in
  let pc =
    pc |> List.map (Fmt.str "%a" Symex.Value.ppa) |> String.concat " && "
  in
  match Soteria.Symex.Compo_res.to_result_opt result with
  | Some (Ok env) -> Fmt.str "(%s) Success:\n  %s" pc (string_of_env env)
  | Some (Error err) -> Fmt.str "(%s) Error: %s" pc err
  | None -> Fmt.str "(%s) Unknown result" pc

let () =
  if Array.length Sys.argv != 2 then Fmt.pr "Usage: %s <src>\n" Sys.argv.(0)
  else
    let src = Sys.argv.(1) in
    let prog = SimpleLib.parse src in
    let final_states = SimpleLib.symb_exec prog ~mode in
    final_states |> List.map string_of_state |> String.concat "\n"
    |> Fmt.pr "%s\n"
