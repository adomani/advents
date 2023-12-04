import Advents.Utils

open Lean Elab Expr Meta

/-- The syntax category `color` consists of the main building blocks for the games.
Valid expressions are `<number> <color>`, where `<color>` is one of `red, green, blue`.
-/
declare_syntax_cat color

/-- `red` is valid `color` syntax. -/
syntax "red"     : color

/-- `green` is valid `color` syntax. -/
syntax "green"   : color

/-- `blue` is valid `color` syntax. -/
syntax "blue"    : color

/-- `<number> <color>` is valid `color` syntax. -/
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

/-- We can multiply a `cols` on the left by a scalar in `Nat`, multiplying each component. -/
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

/-- `stx_to_color_one col` parses one `color`-syntax expression of the form `<number> <color>`
and produces a `cols` whose component corresponding to `color` has value `<number>` and the
rest is `0`. -/
def stx_to_color_one (col : TSyntax `color) : TermElabM cols := unsafe do
  let (stx, dir) := ← match col with
    | `(color| $nu:num red)   => return (nu, (1, 0, 0))
    | `(color| $nu:num green) => return (nu, (0, 1, 0))
    | `(color| $nu:num blue)  => return (nu, (0, 0, 1))
    | _  => throwUnsupportedSyntax
  let val ← Term.elabTerm stx none
  let val ← Term.evalTerm Nat (← inferType val) stx
  return val * dir

/-- The syntax category `game` allows to concatenate the syntax `color` in comma-separated list. -/
declare_syntax_cat game

/-- We can concatenate `color`s in comma-separated lists. -/
syntax color,* : game

/-- `col_stx_to_colors game` is similar to `stx_to_color_one`: it convertes each comma-separated
`color`-syntax into the corresponding vector and sums them all up. -/
def col_stx_to_colors : TSyntax `game → Command.CommandElabM cols
  | `(game| $nc:color,*) => Command.liftTermElabM do
    let mut tot : cols := (0,0,0)
    for x in nc.getElems do
      tot := tot + (← stx_to_color_one x)
    unsafe do
    return tot
  | _  => throwUnsupportedSyntax

/-- Finally, we extend Lean's syntax once more by making each line of the input file
into the syntax of a command... -/
syntax "Game " num ":" sepBy(game, ";") : command

/-- ... and we elaborate the command.
For each game,
* we convert it into the corresponding vector;
* we update the upperbound `upbd` to be the `sup` of these vectors;
* we extract the `val`ue, that is the Game ID;
* we add a definition `def myGame<ID> : Nat := (<ID> or 0)`
  where we choose `<ID>` if the upper-bound `upbd` is at most `limit` and `0` otherwise.
 -/
elab_rules : command
  | `(command| Game $ID:num : $gms:game;*) => do
    let mut upbd : cols := (0, 0, 0)
    for x in gms.getElems do
      let nx := ← col_stx_to_colors x
      upbd := sup upbd nx
    let val := ← unsafe do Command.liftTermElabM do
      let vale ← Term.elabTermEnsuringType ID (some (.const `Nat []))
      Term.evalTerm Nat (← inferType vale) ID
    let na : Name := .str .anonymous ("myGame" ++ ⟨Nat.toDigits 10 val⟩)
    let val0 := if upbd ≤ limit then toExpr val else toExpr 0
    let decl := mkDefinitionValEx na [] (.const `Nat []) val0 .abbrev .safe []
    Command.liftCoreM <| addDecl (Declaration.defnDecl decl)

/-- `getVal` extracts the value of a definition.  We use it to go
from `myGams<ID>` to `<ID> or 0`. -/
def getVal : ConstantInfo → Expr
  | .defnInfo val => val.value
  | _ => default

/-- Finally, we define a custom heterogeneous addition between `Nat` and `Name`s of declarations.
We are going to use it when the `Name` refers to `myGame<ID>` and we add to `Nat` the
value of the corresponding definition. -/
def cadd (t : Nat) (na : Name) : MetaM Nat := do
  if let some nav := (← getEnv).find? na then
    let d := ← (evalNat <| getVal nav).run
    return t + d.getD 0
  else
    return t

/-- `addMyGames num` add all values of all definitions called `myGame<ID>` for
`<ID>` ranging from `0` to `num`. -/
def addMyGames (num : Nat) : MetaM Nat := do
  let mut tot := 0
  for i in [:num] do
    let tadd := ← cadd tot (Name.str .anonymous ("myGame" ++ ⟨Nat.toDigits 10 i⟩))
    tot := tadd
  return tot
