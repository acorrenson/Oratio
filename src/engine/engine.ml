(**
   Oratio proof construction engine

   This module provides methods to execute a proof construction program
*)

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

  val theorem : thm -> thm
  val axiom : prop list -> prop -> thm
end

type ('thm, 'prop) instructions =
  | IntroImpl of 'prop
  | ElimImpl
  | ElimBot of 'prop
  | Axiom of 'prop list * 'prop
  | IntroBot
  | IntroOrL of 'prop
  | IntroOrR of 'prop
  | ElimOr
  | IntroAnd
  | ElimAndL
  | ElimAndR
  | Theorem of 'thm

module type EVALUATOR = sig
  type thm
  type prop
  val eval : (thm, prop) instructions list -> thm
end


(* module Make (X : EVAL_MODEL) : EVALUATOR with type thm = X.thm and type prop = X.prop = struct *)
module Make (X: EVAL_MODEL) : EVALUATOR with type thm = X.thm and type prop = X.prop = struct
  open X

  type thm = X.thm
  type prop = X.prop

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
      | Theorem thm::prog -> exec (theorem thm::thms) prog
      | Axiom (l, prop)::prog -> exec (axiom l prop::thms) prog
      | [] -> thms
    in
    match exec [] prog with
    | [] -> failwith "no thms..."
    | thm::_ -> thm
end
