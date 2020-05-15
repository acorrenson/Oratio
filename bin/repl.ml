open Kernel.Tactic
open Language.Parser

let repl prop =
  let rec step ctx =
    match ctx.goals with
    | [] -> qed prop ctx
    | _ ->
      ignore (Sys.command "clear");
      ignore (debug ctx);
      print_string "> ";
      flush stdout;
      match command (read_line ()) with
      | Some f -> step (f ctx)
      | None -> step ctx
  in
  step (init prop)

let _ =
  (* repl (Impl (Or (Atom 'a', Atom 'b'), Or (Atom 'b', Atom 'a'))); *)
  repl (Impl (And (Atom 'a', Not (Atom 'a')), Bot))


