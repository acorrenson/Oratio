open Kernel
open Notty
open Infix

type prop = Logic.prop

type rules = [
  | `IntroImpl
  | `IntroAnd
  | `IntroOrL
  | `IntroOrR
  | `ElimBot
  | `ElimImpl
  | `ElimOr
  | `ElimAndL
  | `ElimAndR
]

let name = function
  | `IntroImpl -> "IntroImpl"
  | `IntroAnd -> "IntroAnd"
  | `IntroOrL -> "IntroOrL"
  | `IntroOrR -> "IntroOrR"
  | `ElimBot -> "ElimBot"
  | `ElimImpl -> "ElimImpl"
  | `ElimOr -> "ElimOr"
  | `ElimAndL -> "ElimAndL"
  | `ElimAndR -> "ElimAndR"

type thm =
  | Leaf of Rules.thm
  | Node of rules * thm list * Rules.thm

let pprint t =
  match t with
  | Leaf _ -> Printf.printf "Leaf"
  | Node (r, l, _) -> Printf.printf "Node %s %d" (name r) (List.length l)

let get_rthm t =
  match t with
  | Leaf th -> th
  | Node (_, _, th) -> th

let axiom l p =
  Leaf (Rules.axiom l p)

let intro_impl t p =
  Node (`IntroImpl, [t], Rules.intro_impl (get_rthm t) p)

let intro_and t1 t2 =
  Node (`IntroAnd, [t1; t2], Rules.intro_and (get_rthm t1) (get_rthm t2))

let intro_or_l t p =
  Node (`IntroOrL, [t], Rules.intro_or_l (get_rthm t) p)

let intro_or_r t p =
  Node (`IntroOrR, [t], Rules.intro_or_r (get_rthm t) p)

let elim_bot t p =
  Node (`ElimBot, [t], Rules.elim_bot (get_rthm t) p)

let elim_impl t1 t2 =
  Node (`ElimImpl, [t1; t2], Rules.elim_impl (get_rthm t1) (get_rthm t2))

let elim_and_l t =
  Node (`ElimAndL, [t], Rules.elim_and_l (get_rthm t))

let elim_and_r t =
  Node (`ElimAndR, [t], Rules.elim_and_r (get_rthm t))

let elim_or t1 t2 t3 =
  Node (`ElimOr, [t1; t2; t3], Rules.elim_or (get_rthm t1) (get_rthm t2) (get_rthm t3))

let vbar n = I.char A.empty '-' n 1

let rec show t =
  match t with
  | Leaf t -> Rules.show_thm t |> I.string A.empty
  | Node (_, lt, t) ->
    let highs = List.map show lt in
    let hmax = List.fold_left (fun acc i -> max (I.height i) acc) 0 highs in
    let fit i =  i |> I.hpad 2 2 |> I.vsnap ~align:`Bottom hmax in
    let high = List.map fit highs |> I.hcat |> I.hcrop 2 2 in
    let low = Rules.show_thm t |> I.string A.empty in
    let w = max (I.width high) (I.width low) in
    I.hsnap w high <-> vbar w <-> I.hsnap w low

let rec show_light t =
  match t with
  | Leaf t -> Logic.show_prop (Rules.get_prop t) |> I.string A.empty
  | Node (_, lt, t) ->
    let highs = List.map show_light lt in
    let hmax = List.fold_left (fun acc i -> max (I.height i) acc) 0 highs in
    let fit i =  i |> I.hpad 2 2 |> I.vsnap ~align:`Bottom hmax in
    let high = List.map fit highs |> I.hcat |> I.hcrop 2 2 in
    let low = Logic.show_prop (Rules.get_prop t) |> I.string A.empty in
    let w = max (I.width high) (I.width low) in
    I.hsnap w high <-> vbar w <-> I.hsnap w low

let print t =
  show t
  |> Notty_unix.eol
  |> Notty_unix.output_image

let print_light t =
  show_light t
  |> Notty_unix.eol
  |> Notty_unix.output_image



