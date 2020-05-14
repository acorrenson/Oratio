open Logic.Engine
open Logic.Rules

let display n b =
  if b then Printf.printf "Goal %s Proved.\n" n
  else Printf.printf "Goal %s Failed.\n" n


let _ =
  [
    IntroImpl (Impl (Atom 'a', Atom 'b'));
    IntroImpl (Atom 'a');
    ElimImpl;
    Axiom (Atom 'a');
    Axiom (Impl (Atom 'a', Atom 'b'))
  ]
  |> check (Impl (Impl (Atom 'a', Atom 'b'), Impl (Atom 'a', Atom 'b')))
  |> display "Goal1"

let _ =
  [
    IntroImpl (Atom 'a');
    IntroImpl (Atom 'b');
    Axiom (Atom 'a');
  ]
  |> check (Impl (Atom 'a', Impl (Atom 'b', Atom 'a')))
  |> display "Goal2"

let th1 =
  [
    IntroImpl (Atom 'a');
    IntroImpl (Atom 'b');
    Axiom (Atom 'a');
  ] |> eval

let th2 =
  [
    Theorem th1
  ]
  |> check (Impl (Atom 'a', Impl (Atom 'b', Atom 'a')))
  |> display "Goal3"