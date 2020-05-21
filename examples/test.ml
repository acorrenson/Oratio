open Tactic
open Kernel.Logic
open Translators
open French

module T = Engine.Make (Verified_tree)
module D = Engine.Make (Debuger)

let display e = T.eval e.proof |> french
let clear () = print_endline "\n"

let _ =
  clear ();
  let p = (Impl (And (Atom 'a', Atom 'b'), And (Atom 'b', Atom 'a'))) in
  init p
  |> intro
  |> intro
  |> elim (And (Atom 'a', Atom 'b'))
  |> elim (And (Atom 'a', Atom 'b'))
  |> display

let _ =
  clear ();
  let p = (Impl (Atom 'a', Impl (Atom 'b', Atom 'a'))) in
  init p
  |> intro
  |> intro
  |> assumption
  |> display


let _ =
  clear ();
  let p = Impl (Atom 'c', And (Impl (Atom 'a', Atom 'a'), Atom 'c')) in
  init p
  |> intro
  |> intro
  |> intro
  |> assumption
  |> assumption
  |> display

let _ =
  clear ();
  let p = Impl (And (Atom 'a', Atom 'b'), 
                And (And (Atom 'a', Atom 'b'), 
                     And (Atom 'b', Atom 'a'))) 
  in
  init p
  |> intro
  |> intro
  |> assumption
  |> intro
  |> elim (And (Atom 'a', Atom 'b'))
  |> elim (And (Atom 'a', Atom 'b'))
  |> display

let _ =
  clear ();
  let p = Impl (Or (Atom 'a', Atom 'b'), 
                Or (Atom 'b', Atom 'a'))
  in
  init p
  |> intro
  |> elim (Or (Atom 'a', Atom 'b'))
  |> right
  |> assumption
  |> left
  |> assumption
  |> display