open Translators
open Engine
open Kernel.Logic


let prog = [
  Axiom ([Atom 'p'; Not (Atom 'p')], Not (Atom 'p'));
  Axiom ([Atom 'p'; Not (Atom 'p')], Atom 'p');
  IntroBot
]

let test1 () =
  let open Make(Tree) in
  eval prog

let test2 () =
  let open Make(Verified_tree) in
  eval prog |> Verified_tree.print_light

