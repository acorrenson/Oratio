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
[@@deriving show]

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