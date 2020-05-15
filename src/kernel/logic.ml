(**
   Syntactic definition of the propositionnal logic
*)

type prop =
  | Impl of prop * prop
  | Atom of char
  | Or of prop * prop
  | And of prop * prop
  | Not of prop
  | Bot

let rec show_prop t =
  let open Printf in
  match t with
  | Atom s -> String.make 1 s
  | Impl ((Impl _) as a, b) ->
    sprintf "(%s) -> %s" (show_prop a) (show_prop b)
  | Impl (a, b) ->
    sprintf "%s -> %s" (show_prop a) (show_prop b)
  | And ((Impl _ as a), (Impl _ as b)) ->
    sprintf "(%s) /\\ (%s)" (show_prop a) (show_prop b)
  | And ((Impl _) as a, b) ->
    sprintf "(%s) /\\ %s" (show_prop a) (show_prop b)
  | And ((Or _ as a), (Or _ as b)) ->
    sprintf "(%s) /\\ (%s)" (show_prop a) (show_prop b)
  | And ((Or _) as a, b) ->
    sprintf "(%s) /\\ %s" (show_prop a) (show_prop b)
  | And (a, b) ->
    sprintf "%s /\\ %s" (show_prop a) (show_prop b)
  | Or ((Impl _ as a), (Impl _ as b)) ->
    sprintf "(%s) \\/ (%s)" (show_prop a) (show_prop b)
  | Or ((Impl _ as a), b) ->
    sprintf "(%s) \\/ %s" (show_prop a) (show_prop b)
  | Or (a, b) ->
    sprintf "%s \\/ %s" (show_prop a) (show_prop b)
  | Bot -> "Bot"
  | Not a -> sprintf "¬(%s)" (show_prop a)
