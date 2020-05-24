# Oratio

*Interactively build any proof object*

Oratio is an experiment around interactive theorem proving. It includes a program to build and verify natural deduction proof trees for the propositional logic. It is based on a set of tactics which output proof construction code. This code may be evaluated by Oratio's verified kernel and may be re-used to build any kind of proof object as soon as a suitable OCaml module is provided.

## Using Oratio

Oratio takes goal files as input. Their syntax is as follows :

```
goals ::= <goal> <goals>

goal ::= Goal <prop> EOL

prop ::= 
  | <prop> -> <prop>
  | <prop> /\ <prop>
  | <prop> \/ <prop>
  | not <prop>
  | (<prop>)
  | <var>

var ::= [a-zA-Z]+
```

To run Oratio with a given goal file :

```
oratio file.goals
```

This will run an interactive loop asking for proofs of each goal described in the file.
For more details about goals proving, the command `help` may be used at any time while the interactive loop is running.

## Behind Oratio

Oratio is divided into 3 main pieces :
1. **a proof construction engine**
  It's a customizable stack machine which evaluates a tiny assembly-like language
2. **a set of tactics**
  Tactics are functions that may be used by the users to prove theorem in interaction.
  They produce code for the proof construction engine and transform the proof context. No verifications are made at the tactics level.
3. **a kernel**
  The Kernel is a verified set of inference rules. When used as a backend for the proof construction engine, it allows to build valid sequents from a proof construction program.
  Combined with the tactic system, it gives a mini proof-assistant for the propositional logic.

The **proof construction engine** consist in a functor taking a set of inference rules and instantiating an evaluator which build proof object using these rules. The beauty of this approach being that one can provide any kind of backend and still use the same engine and the same tactics to build various proof objects (typed proof trees, lambda-terms, abstract representations of valid sequents...). We are currently trying to use this approach to provide a natural language based backend for Oratio. This will eventually lead to the translation of proofs into natural language such as French or English. 

