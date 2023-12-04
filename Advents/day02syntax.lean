import Advents.Utils

open Lean Elab Expr Meta

declare_syntax_cat color
syntax "red"     : color
syntax "green"   : color
syntax "blue"    : color
syntax num color : color

/-- `cols` is a triple of natural numbers and it stands for `colours`:
the `red`, `green` and `blue` components of the puzzle.
It is an `abbrev` rather than a `def`, since this makes it very easy for Lean to see through
the name `cols` and realise that there are three natural numbers.
If you change it to a `def`, you will see that Lean fails to infer instances
on `cols`, even though `Nat × Nat × Nat` has them.
-/
abbrev cols := Nat × Nat × Nat

/-- We can add two `cols` componentwise. -/
instance : Add cols where
  add x y := (x.1 + y.1, x.2.1 + y.2.1, x.2.2 + y.2.2)

instance : HMul Nat cols cols where
  hMul a c := (a * c.1, a * c.2.1, a * c.2.2)

/-- `limit` is the maximum number of cubes of each colour allows for each game in part 1. -/
def limit : cols := (12, 13, 14)

/-- The `sup` of two `cols` is simply the component-wise `max`imum of the two `cols`. -/
def sup (x y : cols) : cols := (max x.1 y.1, max x.2.1 y.2.1, max x.2.2 y.2.2)

/-- We tell Lean how to compare two different `cols`: `a` is less-than-or-equal-to `b`
if each component of `a` is less-than-or-equal-to the corresponding component of `b`. -/
instance : LE cols where
  le x y := (x.1 ≤ y.1) ∧ (x.2.1 ≤ y.2.1) ∧ (x.2.2 ≤ y.2.2)

/-- We state the obvious: the definition of `≤` for `cols`. -/
theorem cols.le_def {x y : cols} :
    x ≤ y ↔ (x.1 ≤ y.1) ∧ (x.2.1 ≤ y.2.1) ∧ (x.2.2 ≤ y.2.2) := Iff.rfl

/-- The previous obvious fact makes it easy to get Lean to `Decide` inequalities of `cols`. -/
instance : DecidableRel (LE.le : cols → cols → Prop) :=
  fun _ _ => decidable_of_iff' _ cols.le_def

def stx_to_color_one (col : TSyntax `color) : TermElabM cols := unsafe do
  let (stx, dir) := ← match col with
    | `(color| $nu:num red)   => return (nu, (1, 0, 0))
    | `(color| $nu:num green) => return (nu, (0, 1, 0))
    | `(color| $nu:num blue)  => return (nu, (0, 0, 1))
    | _  => throwUnsupportedSyntax
  let val ← Term.elabTerm stx none
  let val ← Term.evalTerm Nat (← inferType val) stx
  return val * dir

declare_syntax_cat game
syntax color,* : game

def col_stx_to_colors : TSyntax `game → Command.CommandElabM cols
  | `(game| $nc:color,*) => Command.liftTermElabM do
    let mut tot : cols := (0,0,0)
    for x in nc.getElems do
      tot := tot + (← stx_to_color_one x)
    unsafe do
    return tot
  | _  => throwUnsupportedSyntax

syntax "Game " num ":" sepBy(game, ";") : command

elab_rules : command
  | `(command| Game $ID:num : $gms:game;*) => do
    let mut upbd : cols := (0, 0, 0)
    for x in gms.getElems do
      let nx := ← col_stx_to_colors x
      upbd := sup upbd nx
    let val := ← unsafe do Command.liftTermElabM do
      let vale ← Term.elabTermEnsuringType ID (some (.const `Nat []))
      Term.evalTerm Nat (← inferType vale) ID
--    logInfoAt ID m!"{(upbd ≤ limit : Bool)} {val}"
    let na : Name := .str .anonymous ("myGame" ++ ⟨Nat.toDigits 10 val⟩)
    let val0 := if upbd ≤ limit then toExpr val else toExpr 0
    let decl := mkDefinitionValEx na [] (.const `Nat []) val0 .abbrev .safe []
    Command.liftCoreM <| addDecl (Declaration.defnDecl decl)

def getVal : ConstantInfo → Expr
  | .defnInfo val => val.value
  | _ => default

--#check evalNat
def cadd (t : Nat) (na : Name) : MetaM Nat := do
  if let some nav := (← getEnv).find? na then
    let d := ← (evalNat <| getVal nav).run
    return t + d.getD 0
  else
    return t
/-
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

#eval show MetaM _ from do
  let mut tot := 0
  for i in [:num] do
    let tadd := ← cadd tot (Name.str .anonymous ("myGame" ++ ⟨Nat.toDigits 10 i⟩))
    tot := tadd
  return tot
--/
