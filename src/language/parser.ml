open Kernel.Logic
open Kernel.Tactic
open Opal

let parens = between (exactly '(') (exactly ')')

let p_impl = token "->" >> return (fun p q -> Impl (p, q))
let p_and = token "/\\" >> return (fun p q -> And (p, q))
let p_or = token "\\/" >> return (fun p q -> Or (p, q))
let p_atom = spaces >> letter => (fun x -> Atom x)
let p_bot = token "Bot" >> return Bot
let p_not p = token "not" >> p => (fun x -> Not x)

let rec p_prop inp = chainr1 p_disj p_impl inp
and p_disj inp = chainl1 p_conj p_or inp
and p_conj inp = chainl1 p_min p_and inp
and p_min inp = choice [p_not (p_prop); parens p_prop; p_bot; p_atom] inp

let command str = parse
    (choice [
        token "elim" >> p_prop => elim;
        token "intro" >> return intro;
        token "left" >> return left;
        token "right" >> return right;
        token "debug" >> return debug;
        token "exfalso" >> return exfalso;
        token "assertion" >> p_prop => assertion;
        token "contradiction" >> p_prop => contradiction;
        token "assumption" >> return assumption;
      ])
    (LazyStream.of_string str)