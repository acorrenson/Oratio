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
            goals:prop list;
            proof:instructions list}

let eval prog =
  let fail () = failwith "incomplete proof" in
  let rec exec thms prog =
    match prog with
    | IntroBot::prog ->
      (match thms with
       | t1::t2::thms -> exec (intro_bot t1 t2::thms) prog
       | _ -> fail ())
    | ElimBot p::prog ->
      (match thms with
       | t::thms -> exec (elim_bot t p::thms) prog
       | _ -> fail ())
    | ElimImpl::prog ->
      (match thms with
       | t1::t2::thms -> exec (elim_impl t1 t2::thms) prog
       | _ -> fail ())
    | IntroImpl p::prog ->
      (match thms with
       | t::thms -> exec (intro_impl t p::thms) prog
       | _ -> fail ())
    | IntroAnd::prog ->
      (match thms with
       | t1::t2::thms -> exec (intro_and t1 t2::thms) prog
       | _ -> fail ())
    | ElimAndL::prog ->
      (match thms with
       | t::thms -> exec (elim_and_l t::thms) prog
       | _ -> fail ())
    | ElimAndR::prog ->
      (match thms with
       | t::thms -> exec (elim_and_r t::thms) prog
       | _ -> fail ())
    | IntroOrL p::prog ->
      (match thms with
       | t::thms -> exec (intro_or_l t p::thms) prog
       | _ -> fail ())
    | IntroOrR p::prog ->
      (match thms with
       | t::thms -> exec (intro_or_r t p::thms) prog
       | _ -> fail ())
    | ElimOr::prog ->
      (match thms with
       | t1::t2::t3::thms -> exec (elim_or t1 t2 t3::thms) prog
       | _ -> fail ())
    | Theorem t::prog -> exec (t::thms) prog
    | Axiom (l, p)::prog -> exec (axiom l p::thms) prog
    | [] -> thms
  in
  match exec [] prog with
  | [] -> failwith "no thms..."
  | t::_ -> t

let check goal prog = is_proof (eval prog) goal

let init p = {ctx=[]; goals=[p]; proof=[]}
