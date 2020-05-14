open Rules

type instructions =
  | IntroImpl of prop
  | ElimImpl
  | ElimBot of prop
  | Axiom of prop list * prop
  | IntroBot
  | IntroOrL of prop
  | IntroOrR of prop
  | ElimOr
  | IntroAnd
  | ElimAndL
  | ElimAndR
  | Theorem of thm

type env = {ctx:prop list;
            goals: prop list;
            proof:instructions list}

val eval : instructions list -> thm
val check : prop -> instructions list -> bool
val init : prop -> env