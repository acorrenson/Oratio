open Verified_tree
open Kernel
open Logic

let sprop t = t |>Rules.get_prop |> show_prop

let rec french vft =
  match vft with
  | Leaf t -> Printf.printf "Des hypothèses, nous déduisons %s\n" (sprop t)
  | Node (`ElimOr, [t1; t2; t3], t) ->
    (match t1 |> get_rthm |> Rules.get_prop with
     | Or (a, b) as c ->
       Printf.printf "Nous montrons %s par disjonction sur %s :\n" (sprop t) (show_prop c);
       Printf.printf "Tout d'abord nous montrons %s\n" (show_prop c);
       french t1;
       Printf.printf "Dans le cas où nous avons %s\n" (show_prop a);
       french t2;
       Printf.printf "Dans le cas où nous avons %s\n" (show_prop b);
       french t3
     | _ -> assert false)
  | Node ((`ElimAndL | `ElimAndR), [t1], t) ->
    Printf.printf "Pour montrer %s il suffit de montrer %s\n" (sprop t) (get_rthm t1 |> sprop);
    french t1
  | Node (`ElimImpl, [t1; t2], t) ->
    Printf.printf "D'une part nous pouvons montrer %s\n" (get_rthm t1 |> sprop);
    french t1;
    Printf.printf "D'autre part nous pouvons montrer %s\n" (get_rthm t2 |> sprop);
    french t2;
    Printf.printf "Par Modus Ponens nous en déduisons %s\n" (sprop t)
  | Node (`ElimBot, [t1], t) ->
    Printf.printf "Nous avons une contradiction, en effet :\n";
    french t1;
    Printf.printf "Donc nous avons nécessairement %s\n" (sprop t)
  | Node (`IntroImpl, [t1], t) ->
    (match t |> Rules.get_prop with
     | Impl (a, _) ->
       Printf.printf "Montrons %s.\n" (sprop t);
       Printf.printf "Supposons d'abord %s\n" (show_prop a);
       french t1
     | _ -> assert false)
  | Node (`IntroAnd, [t1; t2], t) ->
    (match t |> Rules.get_prop with
     | And (a, b) ->
       Printf.printf "Montrons %s\n" (sprop t);
       Printf.printf "Nous prouvons d'une part %s :\n" (show_prop a);
       french t1;
       Printf.printf "Et d'autre part %s\n" (show_prop b);
       french t2;

     | _ -> assert false)
  | Node (`IntroOrL , [t1], t) ->
    (match t |> Rules.get_prop with
     | Or (a, _) ->
       Printf.printf "Pour montrer %s il suffit de montrer %s\n" (sprop t) (show_prop a);
       french t1
     | _ -> assert false)
  | Node (`IntroOrR , [t1], t) ->
    (match t |> Rules.get_prop with
     | Or (_, a) ->
       Printf.printf "Pour montrer %s il suffit de montrer %s\n" (sprop t) (show_prop a);
       french t1
     | _ -> assert false)
  | _ as t -> pprint t |> failwith "err"











