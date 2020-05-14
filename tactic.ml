open Rules
open Engine

type tactic = env -> env

let find p (l:prop list) = List.exists ((=) p) l

let apply p {ctx; goals; proof} =
  match goals, p with
  | [], _ -> failwith "no more subgoals"
  | g::goals, Impl (a, b) when g = b && find p ctx ->
    {ctx; goals=a::goals; proof=Axiom (ctx, p)::ElimImpl::proof}
  | _ -> failwith "Unable to use apply"

let elim p {ctx; goals; proof} =
  match goals, p with
  | [], _ -> failwith "no more subgoals"
  | g::goals, And (a, b) when a = g || b = g && find p ctx ->
    {ctx;
     goals=a::goals;
     proof=Axiom (ctx, p)::(if a = g then ElimAndL else ElimAndR)::proof}
  | _ -> failwith "Unable to use elim"

let intro {ctx; goals; proof} =
  match goals with
  | [] -> failwith "no more subgoals"
  | Impl (a, b)::goals ->
    {ctx=a::ctx;
     goals=b::goals;
     proof=IntroImpl a::proof}
  | And (a, b)::goals ->
    {ctx=ctx;
     goals=a::b::goals;
     proof=IntroAnd::proof}
  | _ -> failwith "Unable to use intro"

let left {ctx; goals; proof} =
  match goals with
  | [] -> failwith "no more subgoals"
  | Or (a, b)::goals ->
    {ctx=ctx;
     goals=a::goals;
     proof=IntroOrL b::proof}
  | _ -> failwith "Unable to use left"

let right {ctx; goals; proof} =
  match goals with
  | [] -> failwith "no more subgoals"
  | Or (a, b)::goals ->
    {ctx=ctx;
     goals=b::goals;
     proof=IntroOrR a::proof}
  | _ -> failwith "Unable to use left"

let assumption {ctx; goals; proof} =
  match goals with
  | [] -> failwith "no more subgoals"
  | g::goals when find g ctx ->
    {ctx=ctx;
     goals=goals;
     proof=Axiom (ctx, g)::proof}
  | _ -> failwith "Unable to use assumtion"


let debug env =
  print_newline ();
  List.iteri (fun i p ->
      Printf.printf "   %d : %s\n" i (show_prop p)
    ) env.ctx;
  print_endline "------------------------------";
  List.iteri (fun i p ->
      Printf.printf "[%d/%d]  %s\n" (i+1) (List.length env.goals) (show_prop p)
    ) env.goals;
  env

let qed p e =
  if check p e.proof
  then Printf.printf "Goal %s Proved.\n" (show_prop p)
  else Printf.printf "Goal %s Failed.\n" (show_prop p)




