open Kernel.Logic
open Tactic
open Opal

let parens = between (exactly '(') (exactly ')')

let p_impl = token "->" >> return (fun p q -> Impl (p, q))
let p_and = token "/\\" >> return (fun p q -> And (p, q))
let p_or = token "\\/" >> return (fun p q -> Or (p, q))
let p_atom = spaces >> letter => (fun x -> Atom x)
let p_bot = token "Bot" >> return Bot
let p_not p = token "not" >> p => (fun x -> Impl (x, Bot))

let rec p_prop inp = between spaces spaces (chainr1 p_disj p_impl) inp
and p_disj inp = between spaces spaces (chainl1 p_conj p_or) inp
and p_conj inp = between spaces spaces (chainl1 p_min p_and) inp
and p_min inp = choice [p_not (p_prop); parens p_prop; p_bot; p_atom] inp

let command str = parse
    (choice [
        token "elimn" >> spaces >> (many1 digit => implode % int_of_string) => elimn;
        token "elim" >> spaces >> p_prop => elim;
        token "applyn" >> spaces >> (many1 digit => implode % int_of_string) => applyn;
        token "apply" >> spaces >> p_prop => apply;
        token "intros" >> spaces >> return intros;
        token "intro" >> spaces >> return intro;
        token "left" >> spaces >> return left;
        token "right" >> spaces >> return right;
        token "debug" >> spaces >> return debug;
        token "help" >> spaces >> return help;
        token "exfalso" >> spaces >> return exfalso;
        token "contradiction" >> spaces >> p_prop => contradiction;
        token "assumption" >> spaces >> return assumption;
      ])
    (LazyStream.of_string str)

let goal = token "Goal" >> spaces >> p_prop

let goals file = parse
    (many (spaces >> goal << spaces))
    (LazyStream.of_channel (open_in file))