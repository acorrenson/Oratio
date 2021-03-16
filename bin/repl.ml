open Tactic
open Language.Parser

let count = ref 0

let repl prop =
  let rec step ctx =
    match ctx.goals with
    | [] ->
      incr count;
      log ("log_" ^ string_of_int !count) ctx;
      qed prop ctx
    | _ ->
      ignore (Sys.command "clear");
      ignore (debug ctx);
      print_string "> ";
      flush stdout;
      match command (read_line ()) with
      | Some f ->
        begin try step (f ctx) 
          with Failure s ->
            print_endline s;
            step ctx
        end
      | None -> step ctx
  in
  step (init prop)

let () =
  match goals (Sys.argv.(1)) with
  | Some gs ->
    List.iter repl gs
  | None -> failwith "invalid input file"

