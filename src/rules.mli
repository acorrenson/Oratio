(**
   {1 Rules }

   This module is the central piece of the kernel.
   It exposes smart constructors to build valid sequents.
   Using these verified constructors is the only way to build 
   a {!thm} object. [thm] objects are then valid by construction.
*)

open Logic

(** {2 The Theorem object } *)

(** The type representing valid sequents *)
type thm

val show_thm : thm -> string
(** Stringify a {!thm} object for pretty printing purposes *)

val get_ctx : thm -> prop list
(** Get the context associated to a sequent.
    If [t] represents the sequent Γ ⊢ p then [get_ctx t] is Γ. *)

val get_prop : thm -> prop
(** Get the proposition associated to a sequent.
    If [t] represents the sequent Γ ⊢ p then [get_ctx t] is p. *)

val is_proof : thm -> prop -> bool
(** Check either a [thm] is a proof of a given proposition or not.
    [is_proof thm prop] is true if and only if [get_ctx thm] is the empty context
    and [get_prop thm] is [prop]. *)

(** {2 The inference rules } *)

(** {b Axiom} rule.
    [axiom Γ p] represents the sequent [Γ ⊢ p] where [p] is in [Γ]. *)
val axiom : prop list -> prop -> thm

(** {b Implication Introduction}.
    [intro_impl (Γ α ⊢ β) α] represents the sequent [Γ ⊢ α → β]. *)
val intro_impl : thm -> prop -> thm

(** {b Implication Elimination} a.k.a {i Modus Ponens} rule.
    [elim_impl (Γ ⊢ α → β) (Γ ⊢ α)] represents the sequent [Γ ⊢ β]. *)
val elim_impl : thm -> thm -> thm

(** {b Bottom Elimination} a.k.a {i Ex Falso} rule.
    [elim_bot (Γ ⊢ ⊥) α] represents the sequent [Γ ⊢ α]. *)
val elim_bot : thm -> prop -> thm

(** {b Bottom Introduction} rule.
    [intro_bot (Γ ⊢ α) (Γ ⊢ ¬α)] represents the sequent [Γ ⊢ ⊥]. *)
val intro_bot : thm -> thm -> thm

(** {b Or Introduction (left)} rule.
    [intro_or_l (Γ ⊢ α) β] represents the sequent [Γ ⊢ α ∨ β]. *)
val intro_or_l : thm -> prop -> thm

(** {b Or Introduction (right)} rule.
    [intro_or_l (Γ ⊢ α) β] represents the sequent [Γ ⊢ β ∨ α ]. *)
val intro_or_r : thm -> prop -> thm

(** {b Or Elimination} rule.
    [elim_or_l (Γ ⊢ α ∨ β) (Γ α ⊢ μ) (Γ β ⊢ μ)] represents the sequent [Γ ⊢ μ]. *)
val elim_or : thm -> thm -> thm -> thm

(** {b And Introduction} rule.
    [intro_and (Γ ⊢ α) (Γ ⊢ β)] represents the sequent [Γ ⊢ α ∧ β]. *)
val intro_and : thm -> thm -> thm

(** {b And Elimination (left)} rule.
    [elim_and_l (Γ ⊢ α ∧ β)] represents the sequent [Γ ⊢ α]. *)
val elim_and_l : thm -> thm

(** {b And Elimination (right)} rule.
    [elim_and_r (Γ ⊢ α ∧ β)] represents the sequent [Γ ⊢ β]. *)
val elim_and_r : thm -> thm
