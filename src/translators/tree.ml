open Kernel

type prop = Logic.prop

type thm =
  | Axiom of prop list * prop
  | Intro_bot of thm * thm
  | Intro_impl of thm * prop
  | Intro_and of thm * thm
  | Intro_or_l of thm * prop
  | Intro_or_r of thm * prop
  | Elim_bot of thm * prop
  | Elim_impl of thm * thm
  | Elim_and_l of thm
  | Elim_and_r of thm
  | Elim_or of thm * thm * thm
[@@deriving variants]



