(**
   Syntactic definition of the propositionnal logic
*)

open Printf

type prop =
  | And of prop * prop
  | Or of prop * prop
  | Impl of prop * prop
  | Atom of char
  | Bot

let priority p =
  match p with
  | And _ -> 3
  | Or _ -> 2
  | Impl _ -> 1
  | Atom _ -> 4
  | Bot -> 4

let show_prop p =
  let rec step p ppr =
    let cpr = priority p in
    let str =
      match p with
      | And (a, b) -> sprintf "%s ∧ %s" (step a cpr) (step b cpr)
      | Or (a, b) -> sprintf "%s ∨ %s" (step a cpr) (step b cpr)
      | Impl (a, b) -> sprintf "%s -> %s" (step a cpr) (step b (cpr - 1))
      | Atom a -> String.make 1 a
      | Bot -> "⟘"
    in
    if cpr <= ppr then sprintf "(%s)" str else str
  in
  step p 0



(* let rec show_prop t =
   let open Printf in
   match t with
   | Atom s -> String.make 1 s
   | Impl ((Impl _) as a, b) ->
    sprintf "(%s) -> %s" (show_prop a) (show_prop b)
   | Impl (a, b) ->
    sprintf "%s -> %s" (show_prop a) (show_prop b)
   | And ((Impl _ as a), (Impl _ as b)) ->
    sprintf "(%s) /\\ (%s)" (show_prop a) (show_prop b)
   | And ((Impl _) as a, b) ->
    sprintf "(%s) /\\ %s" (show_prop a) (show_prop b)
   | And ((Or _ as a), (Or _ as b)) ->
    sprintf "(%s) /\\ (%s)" (show_prop a) (show_prop b)
   | And ((Or _) as a, b) ->
    sprintf "(%s) /\\ %s" (show_prop a) (show_prop b)
   | And (a, b) ->
    sprintf "%s /\\ %s" (show_prop a) (show_prop b)
   | Or ((Impl _ as a), (Impl _ as b)) ->
    sprintf "(%s) \\/ (%s)" (show_prop a) (show_prop b)
   | Or ((Impl _ as a), b) ->
    sprintf "(%s) \\/ %s" (show_prop a) (show_prop b)
   | Or (a, b) ->
    sprintf "%s \\/ %s" (show_prop a) (show_prop b)
   | Bot -> "Bot"
   | Not a -> sprintf "¬(%s)" (show_prop a) *)
