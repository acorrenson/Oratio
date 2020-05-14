type prop = Impl of prop * prop | Atom of char | Bot
type thm
val hyp : 'a -> 'a list -> bool
val rem : 'a -> 'a list -> 'a list
val get_ctx : thm -> prop list
val get_prop : thm -> prop
val qed : prop -> thm -> bool
val axiom : prop list -> prop -> thm
val intro_impl : thm -> prop -> thm
val elim_impl : thm -> thm -> thm
val elim_bot : thm -> prop -> thm
