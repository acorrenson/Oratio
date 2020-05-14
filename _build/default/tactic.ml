open Rules
open Engine

type tactic = env -> env

let find p (l:prop list) = List.exists ((=) p) l

let apply {ctx; goals; proof} p =
  match goals, p with
  | [], _ -> failwith "no more subgoals"
  | g::goals, Impl (a, b) when g = b && find p ctx ->
    {ctx; goals=a::goals; proof=Axiom (ctx, p)::ElimImpl::proof}
  | _ -> failwith "Unable to use apply"

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

let qed p e =
  if check p e.proof
  then Printf.printf "Goal %s Proved.\n" (show_prop p)
  else Printf.printf "Goal %s Failed.\n" (show_prop p)




