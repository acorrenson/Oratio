
type prop =
  | Impl of prop * prop
  | Atom of char
  | Bot

type thm = Thm of prop list * prop

let hyp p = List.exists ((=) p)
let rem p = List.filter ((<>) p)

let get_ctx (Thm (e, _)) = e
let get_prop (Thm (_, p)) = p
let qed q (Thm (e, p)) = e = [] && p = q

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