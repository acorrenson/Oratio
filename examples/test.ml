open Tactic
open Kernel.Logic

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

let _ =
  let p = Impl (Or (Atom 'a', Atom 'b'), 
                Or (Atom 'b', Atom 'a'))
  in
  init p
  |> intro
  |> debug
  |> elim (Or (Atom 'a', Atom 'b'))
  |> debug
  |> right
  |> debug
  |> assumption
  |> left
  |> assumption
  |> qed p