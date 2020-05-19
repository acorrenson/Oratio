(**
   {1 Oratio's proof construction engine}

   This module exposes a langage as well as a generic evaluator
   for proofs construction programs.

   Multiple backends could be used together with this engine to produce
   indistinclty proof-trees, explanation texts, typed lambda-terms or any other 
   proof objects. Evaluators with custom backends are produced using the {! Make}
   functor.

   {1 The proof construction language}

   The proof construction language is composed of 12 elementary instructions (see {! instructions}).
   Instructions are evaluated by a stack machine as follows :

   - [ElimBot p]

   Pop a [thm] from the stack, apply [elim_bot] with [p] as second 
   argument on it and push the result

   - [ElimImpl]

   Pop 2 [thm] from the stack, apply [elim_impl] on them 
   and push the result

   - [ElimAndL] or [ElimAndR]

   Pop a [thm] from the stack, apply [elim_and_l] (respectively [elim_and_r]) on it
   and push the result

   - [ElimOr]

   Pop 3 [thm] from the stack, apply [elim_or] on them
   and push the result

   - [IntroBot]

   Pop 2 [thm] from the stack, apply [intro_bot] on them
   and push the result

   - [IntroImpl p]

   Pop a [thm] from the stack, apply [intro_impl] with p as second
   argument on it and push the result

   - [IntroAnd]

   Pop 2 [thm] from the stack, apply [intro_and] on them
   and push the result

   - [IntroOrL p] or [IntroOrL p]

   Pop a [thm] from the stack, apply [intro_or_l] (respectively [intro_or_r]) 
   with p as second argument on it and push the result

   - [Axiom l p]

   Push [axiom l p] on the stack
*)

(** Signature of backend modules for the engine *)
module type EVAL_MODEL = sig
  (** Proof object *)
  type thm

  (** Propositions *)
  type prop

  (** {2 Introduction rules} *)

  val intro_bot : thm -> thm -> thm
  val intro_impl : thm -> prop -> thm
  val intro_and : thm -> thm -> thm
  val intro_or_l : thm -> prop -> thm
  val intro_or_r : thm -> prop -> thm

  (** {2 Elimintation rules} *)

  val elim_bot : thm -> prop -> thm
  val elim_impl : thm -> thm -> thm
  val elim_and_l : thm -> thm
  val elim_and_r : thm -> thm
  val elim_or : thm -> thm -> thm -> thm

  (** {2 End of proofs rules} *)

  val axiom : prop list -> prop -> thm
end

(** Proof construction language *)
type 'prop instructions =
  | IntroBot
  | IntroImpl of 'prop
  | IntroAnd
  | IntroOrL of 'prop
  | IntroOrR of 'prop
  | ElimBot of 'prop
  | ElimImpl
  | ElimAndL
  | ElimAndR
  | ElimOr
  | Axiom of 'prop list * 'prop


module Make (X: EVAL_MODEL) = struct
  open X

  let eval prog =
    let fail () = failwith "evaluation failed" in
    let rec exec thms prog =
      match prog with
      | IntroBot::prog ->
        (match thms with
         | t1::t2::thms -> exec (intro_bot t1 t2::thms) prog
         | _ -> fail ())
      | ElimBot prop::prog ->
        (match thms with
         | thm::thms -> exec (elim_bot thm prop::thms) prog
         | _ -> fail ())
      | ElimImpl::prog ->
        (match thms with
         | t1::t2::thms -> exec (elim_impl t1 t2::thms) prog
         | _ -> fail ())
      | IntroImpl prop::prog ->
        (match thms with
         | thm::thms -> exec (intro_impl thm prop::thms) prog
         | _ -> fail ())
      | IntroAnd::prog ->
        (match thms with
         | t1::t2::thms -> exec (intro_and t1 t2::thms) prog
         | _ -> fail ())
      | ElimAndL::prog ->
        (match thms with
         | thm::thms -> exec (elim_and_l thm::thms) prog
         | _ -> fail ())
      | ElimAndR::prog ->
        (match thms with
         | thm::thms -> exec (elim_and_r thm::thms) prog
         | _ -> fail ())
      | IntroOrL prop::prog ->
        (match thms with
         | thm::thms -> exec (intro_or_l thm prop::thms) prog
         | _ -> fail ())
      | IntroOrR prop::prog ->
        (match thms with
         | thm::thms -> exec (intro_or_r thm prop::thms) prog
         | _ -> fail ())
      | ElimOr::prog ->
        (match thms with
         | t1::t2::t3::thms -> exec (elim_or t1 t2 t3::thms) prog
         | _ -> fail ())
      | Axiom (l, prop)::prog -> exec (axiom l prop::thms) prog
      | [] -> thms
    in
    match exec [] prog with
    | [] -> failwith "no thms..."
    | thm::_ -> thm
end
