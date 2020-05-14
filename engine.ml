open Rules

type instructions =
  | IntroImpl of prop
  | ElimImpl
  | ElimBot of prop
  | Axiom of prop
  | Theorem of thm

let eval prog =
  let rec step hyps thms prog instr =
    match prog with
    | [] -> exec thms instr
    | Axiom p::prog ->
      step (hyps) (axiom hyps p::thms) prog instr
    | IntroImpl p::prog ->
      step (p::hyps) thms prog (IntroImpl p::instr)
    | ElimImpl::prog ->
      step hyps thms prog (ElimImpl::instr)
    | ElimBot p::prog ->
      step hyps thms prog (ElimBot p::instr)
    | Theorem t::prog ->
      step hyps (t::thms) prog instr
  and exec thms prog =
    match prog with
    | ElimBot p::prog ->
      (match thms with
       | t::thms -> exec (elim_bot t p::thms) prog
       | _ -> failwith "incomplete proof")
    | ElimImpl::prog ->
      (match thms with
       | t1::t2::thms -> exec (elim_impl t1 t2::thms) prog
       | _ -> failwith "incomplete proof")
    | IntroImpl p::prog ->
      (match thms with
       | t::thms -> exec (intro_impl t p::thms) prog
       | _ -> failwith "incomplete proof")
    | [] -> thms
    | _ -> assert false
  in
  match step [] [] prog [] with
  | [] -> failwith "no thms..."
  | t::_ -> t

let check goal prog = 
  eval prog
  |> qed goal