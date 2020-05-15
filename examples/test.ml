(* open Oratio.Engine *)
open Oratio.Rules
open Oratio.Tactic
(* 
let display n b =
  if b then Printf.printf "Goal %s Proved.\n" n
  else Printf.printf "Goal %s Failed.\n" n

let _ =
  [
    IntroImpl (Impl (Atom 'a', Atom 'b'));
    IntroImpl (Atom 'a');
    ElimImpl;
    Axiom ([Impl (Atom 'a', Atom 'b'); Atom 'a'], Impl (Atom 'a', Atom 'b'));
    Axiom ([Impl (Atom 'a', Atom 'b'); Atom 'a'], Atom 'a');
  ]
  |> List.rev
  |> check (Impl (Impl (Atom 'a', Atom 'b'), Impl (Atom 'a', Atom 'b')))
  |> display "Goal1"

let _ =
  [
    IntroImpl (Atom 'a');
    IntroImpl (Atom 'b');
    Axiom ([Atom 'a'; Atom 'b'], Atom 'a');
  ]
  |> List.rev
  |> check (Impl (Atom 'a', Impl (Atom 'b', Atom 'a')))
  |> display "Goal2"
*)

let _ =
  let p = (Impl (And (Atom 'a', Atom 'b'), And (Atom 'b', Atom 'a'))) in
  init p
  |> debug
  |> intro
  |> debug
  |> intro
  |> debug
  |> elim (And (Atom 'a', Atom 'b'))
  |> debug
  |> elim (And (Atom 'a', Atom 'b'))
  |> debug
  |> qed p

let _ =
  let p = (Impl (Atom 'a', Impl (Atom 'b', Atom 'a'))) in
  init p
  |> intro
  |> intro
  |> assumption
  |> qed p


let _ =
  let p = Impl (Atom 'c', And (Impl (Atom 'a', Atom 'a'), Atom 'c')) in
  init p
  |> debug
  |> intro
  |> debug
  |> intro
  |> debug
  |> intro
  |> debug
  |> assumption
  |> debug
  |> assumption
  |> debug
  |> qed p

let _ =
  let p = Impl (And (Atom 'a', Atom 'b'), 
                And (And (Atom 'a', Atom 'b'), 
                     And (Atom 'b', Atom 'a'))) 
  in
  init p
  |> debug
  |> intro
  |> debug
  |> intro
  |> debug
  |> assumption
  |> debug
  |> intro
  |> debug
  |> elim (And (Atom 'a', Atom 'b'))
  |> debug
  |> elim (And (Atom 'a', Atom 'b'))
  |> debug
  |> qed p