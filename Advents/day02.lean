import Advents.Utils
import Lean.Elab.Frontend

open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/i02.txt"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

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

/-- `one_color s` takes a string `s` as input and returns a `cols`.
If, ignoring spaces, the string is of the form
`<digits> <red/green/blue>`
then `one_color s` is the `cols` with a single non-zero component
with the appropriate value and colour.
-/
def one_color (s : String) : cols :=
  let no_spaces := s.toList.filter (· != ' ')
  let (number, color) := no_spaces.partition Char.isDigit
  let (number, color) : Nat × String := (String.toNat! ⟨number⟩, ⟨color⟩)
  match color with
    | "red"   => (number, 0, 0)
    | "green" => (0, number, 0)
    | "blue"  => (0, 0, number)
    | _ => default

--#assert one_color "  45   blue" == (0, 0, 45)

/-- `get_color_number s` takes a string `s` as input and it assumes that it is of the form
`Game <ds>: <ds> <colour>,... ; ...`
where
* `<ds>` is a sequence of cosecutive digits;
* `<colour>` is one of `red`, `green` or `blue`.

It returns the natural number corresponding to the label of `Game` and a list of tallies
of `cols`, one for each semi-colon (`;`) separated block.
-/
def get_color_number (s : String) : Nat × List cols :=
  let x := s.splitOn ":"
  let gameID := x[0]!
  let runs := ((x[1]!).splitOn ";").map (String.splitOn · ",")
  let colors_lists := runs.map fun x => x.map one_color
  (String.toNat! (gameID.dropWhile (! Char.isDigit ·)), colors_lists.map List.sum)

/-
#assert
  get_color_number "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" ==
    (1, [(4, 0, 3), (1, 2, 6), (0, 2, 0)])
--/

/-- `limit` is the maximum number of cubes of each colour allows for each game in part 1. -/
def limit : cols := (12, 13, 14)

/-- We tell Lean how to compare two different `cols`: `a` is less-than-or-equal-to `b`
if each component of `a` is less-than-or-equal-to the corresponding component of `b`. -/
instance : LE cols where
  le x y := (x.1 ≤ y.1) ∧ (x.2.1 ≤ y.2.1) ∧ (x.2.2 ≤ y.2.2)

/-- We state the obvious: the definition of `≤` for `cols`. -/
theorem cols.le_def {x y : cols} : x ≤ y ↔ (x.1 ≤ y.1) ∧ (x.2.1 ≤ y.2.1) ∧ (x.2.2 ≤ y.2.2) := Iff.rfl

/-- The previous obvious fact makes it easy to get Lean to `Decide` inequalities of `cols`. -/
instance : DecidableRel (LE.le : cols → cols → Prop) :=
  fun _ _ => decidable_of_iff' _ cols.le_def

-- Without the previous `DecidableRel` instance, this would not work
--#assert (((1, 2, 3) : cols) ≤ (4, 1, 3)) == false
--#assert (((0, 2, 13) : cols) ≤ (4, 2, 13)) == true

/-- The test string for part 1. -/
def test := "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

/-- `part1 rows` takes an array of strings `rows` as input and produces that natural number
that sums the Game IDs of all games that are possible assuming the number of cubes is bounded
by `limit`. -/
def part1 (rows : Array String) : Nat :=
  let games := rows.map get_color_number
  let small := games.filter fun ((_, gms) : Nat × List cols) => gms.all (· ≤ limit)
  let smallIDs := small.map Prod.fst
  smallIDs.sum

--#assert part1 (test.splitOn "\n").toArray == 8
--#assert part1 (← IO.FS.lines input) == 2169

#eval do IO.println <| "Day 2, part 1: " ++ f!"{part1 (← IO.FS.lines input)}"

/-!
#  Question 2
-/

/-- The `sup` of two `cols` is simply the component-wise `max`imum of the two `cols`. -/
def sup (x y : cols) : cols := (max x.1 y.1, max x.2.1 y.2.1, max x.2.2 y.2.2)

--#assert sup (sup (4, 0, 3) (1, 2, 6)) (0, 2, 0) == (4, 2, 6)

/-- `part2 rows` takes an array of strings `rows` as input and produces that natural number
obtained by
* computing, for each game, the smallest number of cubes of each colour
  that is compatible with the given data;
* multiplying the components of each such smallest possible number of cubes;
* adding together all these products.
-/
def part2 (rows : Array String) : Nat :=
  let games := rows.map get_color_number
  let sups := games.map fun ((_, gms) : Nat × List cols) => match gms with
    | []    => default
    | m::ms => ms.foldl sup m
  let powers := sups.map fun ((a, b, c) : cols) => a * b * c
  powers.sum

--#assert part2 (test.splitOn "\n").toArray == 2286
--#assert part2 (← IO.FS.lines input) == 60948

#eval do IO.println <| "Day 2, part 1: " ++ f!"{part2 (← IO.FS.lines input)}"


#check Syntax.TSepArray.getElems

declare_syntax_cat pilean
declare_syntax_cat parts

syntax (name := inList) "[" term,* "]" : pilean
syntax (name := inNull) "null" : pilean

open Lean Elab Expr Meta

def myStx : String :=
"
import Lean.Elab.Frontend

open Lean Elab Expr Meta

syntax \"Game \" num \":\" : command
elab_rules : command
  | `(command| Game $n:num:) => logInfo n"

def g : String := "Game 3:"

def h : String := "\n#eval first_digit? ['5']"

#eval show IO _ from do
  let _ ← runFrontend (myStx ++ "\n" ++ g ++ h) (.empty) "Advents.day02" (.str (.str (.str .anonymous "Advents") "day01") "lean")
  return ()
/-
syntax "Game " num ":" : command
elab_rules : command
  | `(command| Game $n:num:) => Command.liftTermElabM do
    let nu := ← Term.elabTerm n none
    logInfo nu
-/

--syntax num "red" : command
--syntax num "green" : command
--syntax num "blue" : command

--elab_rules : command
--  | `(command| $n:num red) => Command.liftTermElabM do
--    unsafe do
--    let nu ← Term.elabTerm n none
--    let nu ← Term.evalTerm Nat (← inferType nu) n
--    logInfo f!"{nu + 1}"

declare_syntax_cat color

--syntax "Game"   : color
syntax "red"   : color
syntax "green" : color
syntax "blue"  : color

syntax num color : command

def stx_to_cols (n : TSyntax `num) (col : TSyntax `color) : TermElabM cols := do
  unsafe do
  let nu ← Term.elabTerm n none
  let nu ← Term.evalTerm Nat (← inferType nu) n
  match col with
    | `(color| red)   => return (nu, 0, 0)
    | `(color| green) => return (0, nu, 0)
    | `(color| blue)  => return (0, 0, nu)
    | _  => throwUnsupportedSyntax

elab_rules : command
  | `(command| $n:num $col:color) => Command.liftTermElabM do
    unsafe do
    let c ← stx_to_cols n col
    logInfo m!"{c}"

declare_syntax_cat new

syntax (name := oneRun) (num color),* : new
syntax (name := onePlay) "Game" num ":" oneRun,* : color

#check `(color| Game 5: 4 red, 5 green, 6 blue)

--elab_rules : command
def parseArrows : TSyntax `new → TermElabM (Array (Expr × Bool × Syntax))
  | `(new| $[$x:oneRun],* ) => Command.liftTermElabM do
--  | `(color| rs:($(num color),*)) => Command.liftTermElabM do
    unsafe do
    --let c ← stx_to_cols n col
    logInfo m!"{c}"
    return default
  | _ => throwUnsupportedSyntax


#check Term.evalTerm
#check evalExpr

Game 3: 4 red
4 red 5 green 6 blue
unsafe
def Expr.toListNat : List Expr → TermElabM (List Nat)-- := do
--  let lc ← Term.elabTerm (← `(@List.cons Nat)) none
--  match l with
    | []    => return []
    | m::ms => return (← evalExpr' (Nat) ``Nat m) :: (← Expr.toListNat ms)


#check evalNat
def Expr.toListNat1 : List Expr → TermElabM (List Nat)-- := do
--  let lc ← Term.elabTerm (← `(@List.cons Nat)) none
--  match l with
    | []    => return []
    | m::ms => return  mkAppM ``List.cons #[m, ms]
