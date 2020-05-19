(**
   Inference rules

   This module provides smart constructors to build valid sequents
*)

open Logic

type thm = Thm of prop list * prop

type prop = Logic.prop

let show_thm (Thm (e, p)) =
  let show_prop_list l = List.fold_left (fun acc p ->
      acc ^ " " ^ (show_prop p)
    ) " " l |> String.trim
  in
  Printf.sprintf "%s |- %s" (show_prop_list e) (show_prop p)

let hyp p = List.exists ((=) p)
let rem p = List.filter ((<>) p)

let get_ctx (Thm (e, _)) = e
let get_prop (Thm (_, p)) = p
let is_proof (Thm (e, p)) q = e = [] && p = q

let axiom l p =
  if List.exists ((=) p) l
  then Thm (l, p)
  else failwith "[axiom] failed"

let intro_impl (Thm (e, t)) p =
  if hyp p e
  then Thm (rem p e, Impl (p, t))
  else failwith "[intro_impl] failed"

let elim_impl (Thm (e1, p)) (Thm (e2, q)) =
  match p with
  | Impl (a, b) when a = q && e1 = e2 ->
    Thm (e1, b)
  | _ -> failwith "[elim_impl] failed"

let elim_bot (Thm (e, p)) q =
  match p with
  | Bot -> Thm (e, q)
  | _ -> failwith "[elim_bot] failed"

let intro_bot (Thm(e1, p)) (Thm(e2, np)) =
  match np with
  | Not q when q = p && e1 = e2 ->
    Thm (e1, Bot)
  | _ -> failwith "[intro_bot] failed"

let intro_or_l (Thm (e, p)) q = Thm (e, Or (p, q))

let intro_or_r (Thm (e, p)) q = Thm (e, Or (q, p))

let elim_or (Thm (e1, w)) (Thm (e2, p)) (Thm (e3, q)) =
  match w with
  | Or (a, b) when rem a e2 = e1 && rem b e3 = e1 && p = q ->
    Thm (e1, p)
  | _ -> failwith "[elim_or] failed"

let intro_and (Thm (e1, p)) (Thm (e2, q)) =
  if e1 = e2 then Thm (e1, And (p, q))
  else failwith "[intro_and] failed"

let elim_and_l (Thm (e, p)) =
  match p with
  | And (q, _) -> Thm (e, q)
  | _ -> failwith "[elim_and_l] failed"

let elim_and_r (Thm (e, p)) =
  match p with
  | And (_, q) -> Thm (e, q)
  | _ -> failwith "[elim_and_r] failed"

let theorem t = t