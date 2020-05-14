type prop =
    Impl of prop * prop
  | Atom of char
  | Or of prop * prop
  | And of prop * prop
  | Not of prop
  | Bot
type thm
val show_thm : thm -> string
val show_prop : prop -> string
val hyp : 'a -> 'a list -> bool
val rem : 'a -> 'a list -> 'a list
val get_ctx : thm -> prop list
val get_prop : thm -> prop
val is_proof : thm -> prop -> bool
val axiom : prop list -> prop -> thm
val intro_impl : thm -> prop -> thm
val elim_impl : thm -> thm -> thm
val elim_bot : thm -> prop -> thm
val intro_bot : thm -> thm -> thm
val intro_or_l : thm -> prop -> thm
val intro_or_r : thm -> prop -> thm
val elim_or : thm -> thm -> thm -> thm
val intro_and : thm -> thm -> thm
val elim_and_l : thm -> thm
val elim_and_r : thm -> thm
