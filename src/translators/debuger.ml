(**
   {1 Debuger}

   An rewrite of the {! kernel} module to trace executions in stdout.
*)

open Kernel

type thm = Rules.thm
type prop = Rules.prop

let intro_bot t1 t2 =
  Printf.printf "intro BOT on (%s) and (%s)\n"
    (Rules.show_thm t1) (Rules.show_thm t2);
  Rules.intro_bot t1 t2

let intro_impl t p =
  Printf.printf "intro IMPL on (%s) and (%s)\n"
    (Rules.show_thm t) (Logic.show_prop p);
  Rules.intro_impl t p

let intro_and t1 t2 =
  Printf.printf "intro AND on (%s) and (%s)\n"
    (Rules.show_thm t1) (Rules.show_thm t2);
  Rules.intro_and t1 t2

let intro_or_l t p =
  Printf.printf "intro OR (left) on (%s) and (%s)\n"
    (Rules.show_thm t) (Logic.show_prop p);
  Rules.intro_or_l t p


let intro_or_r t p =
  Printf.printf "intro OR (right) on (%s) and (%s)\n"
    (Rules.show_thm t) (Logic.show_prop p);
  Rules.intro_or_r t p

let elim_bot t p =
  Printf.printf "elim BOT from (%s) to get (%s)\n"
    (Rules.show_thm t) (Logic.show_prop p);
  Rules.elim_bot t p

let elim_impl t1 t2 =
  Printf.printf "elim IMPL from (%s) and (%s)\n"
    (Rules.show_thm t1) (Rules.show_thm t2);
  Rules.elim_impl t1 t2

let elim_and_l t =
  Printf.printf "elim AND (left) from (%s)\n"
    (Rules.show_thm t);
  Rules.elim_and_l t

let elim_and_r t =
  Printf.printf "elim AND (right) from (%s)\n"
    (Rules.show_thm t);
  Rules.elim_and_r t


let elim_or t1 t2 t3 =
  Printf.printf "elim OR from %s, %s and (%s)\n"
    (Rules.show_thm t1) (Rules.show_thm t2) (Rules.show_thm t3);
  Rules.elim_or t1 t2 t3

let axiom l p =
  Printf.printf "%s is in [%s ]\n"
    (Logic.show_prop p)
    (List.fold_left (fun a p -> a ^ " " ^ (Logic.show_prop p)) "" l);
  Rules.axiom l p




