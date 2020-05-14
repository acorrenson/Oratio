open Logic.Engine
open Logic.Rules
open Logic.Tactic

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

let _ =
  [
    IntroImpl (And (Atom 'a', Atom 'b'));
    IntroAnd;
    ElimAndR;
    Axiom ([And (Atom 'a', Atom 'b')], And (Atom 'a', Atom 'b'));
    ElimAndL;
    Axiom ([And (Atom 'a', Atom 'b')], And (Atom 'a', Atom 'b'));
  ]
  |> List.rev
  |> check (Impl (And (Atom 'a', Atom 'b'), And (Atom 'b', Atom 'a')))
  |> display "and_comm"

let th1 =
  [
    IntroImpl (Atom 'a');
    IntroImpl (Atom 'b');
    Axiom ([Atom 'a'; Atom 'b'], (Atom 'a'));
  ]
  |> List.rev
  |> eval

let th2 =
  [
    Theorem th1
  ]
  |> List.rev
  |> check (Impl (Atom 'a', Impl (Atom 'b', Atom 'a')))
  |> display "Goal3"

let _ =
  let p = (Impl (Atom 'a', Impl (Atom 'b', Atom 'a'))) in
  init p
  |> intro
  |> intro
  |> assumption
  |> qed p
