import Advents.Utils

open Lean Meta Elab

declare_syntax_cat card
syntax num* : card
syntax "Card " num ":" card "|" card : command

def getList : TSyntax `card → TermElabM (Array Nat)
  | `(card| $c1:num*) => do
    unsafe do
    let mut l1 := #[]
    for x in c1 do
      let vale ← Term.elabTermEnsuringType x (some (.const `Nat []))
      l1 := l1.push (← Term.evalTerm Nat (← inferType vale) x)
    return l1
  | _ => throwUnsupportedSyntax

#eval 0
def card_to_val : TSyntax `command → TermElabM (Array Nat × Array Nat)
  | `(command| Card $n:num : $c1 | $c2) => do --Command.liftTermElabM do
      let (l1, l2) := (← getList c1, ← getList c2)
      dbg_trace 0
      return (l1, l2)
      --dbg_trace f!"{(l1, l2)}"
  | _ => throwUnsupportedSyntax

set_option pp.rawOnError true in
elab_rules : command
  | `(command| $x) => card_to_val x
  | _ => throwUnsupportedSyntax
--#check Card 1: 4 5 | 7

#eval do
  let ls ← card_to_val (← `(command| Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53))

  dbg_trace ls
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
