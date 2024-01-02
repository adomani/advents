import Std

section sums
variable {α} [Inhabited α]

/--  Sum the elements of a `List`. -/
def List.sum [Add α] : List α → α
  | []    => default
  | [m]   => m
  | m::ms => m + ms.sum

/--  Sum the elements of an `Array`. -/
def Array.sum [Add α] (l : Array α) : α :=
  l.toList.sum

/--  Multiply the elements of a `List`. -/
def List.prod [Mul α] : List α → α
  | []    => default
  | [m]   => m
  | m::ms => m * ms.prod

/--  Multiply the elements of an `Array`. -/
def Array.prod [Mul α] (l : Array α) : α :=
  l.toList.prod

end sums

section Instances_for_orders
/-!
# Instances for orders

We introduce here instances on products, so that `vol` acquires them.
-/

/-- the component-wise addition of pairs of integers. -/
instance {A B} [Add A] [Add B] : Add (A × B) where
  add x y := (x.1 + y.1, x.2 + y.2)

/-- the component-wise subtraction of two pairs of integers. -/
instance {A B} [Sub A] [Sub B] : Sub (A × B) where
 sub x y := (x.1 - y.1, x.2 - y.2)

variable {α β} [LT α] [LT β]
/-- The lexicographic order of a product. -/
instance : LT (α × β) where
  lt x y := (x.1 < y.1) ∨ ((x.1 = y.1) ∧ (x.2 < y.2))

theorem Prod.lt_iff {x y : α × β} : x < y ↔
    (x.1 < y.1) ∨ ((x.1 = y.1) ∧ (x.2 < y.2)) := Iff.rfl

variable [DecidableEq α] [∀ a b : α, Decidable (a < b)] [∀ a b : β, Decidable (a < b)] in
/-- If two ordered types have enough decidable assumptions, then the lexicographic
product of the two types also has decidable inequalities. -/
instance {a b : α × β} : Decidable (a < b) := decidable_of_iff' _ Prod.lt_iff

end Instances_for_orders

/-- `List.getNats l` takes as input a list of characters and returns the list of
`Nat` where each entry is the natural number corresponding to each consecutive
sequence of digits in `l`, in their order. -/
partial
def String.getNats (l : String) : List Nat :=
  let l1 := l.dropWhile (!Char.isDigit ·)
  if l1.length == 0 then [] else
    let d1 := String.toNat! ⟨l1.toList.takeWhile (Char.isDigit ·)⟩
    let fin := getNats (l1.dropWhile (Char.isDigit ·))
  d1 :: fin

/-- `String.getInts l` takes as input a string `l`, removes everything that is neither a digit,
not a minus sign (`-`) and interprets the rest as a list of integers. -/
partial
def String.getInts (l : String) : List Int :=
  let cond : Char → Bool := fun c => (Char.isDigit c) || (c == '-')
  let l1 := l.dropWhile (!cond ·)
  if l1.length == 0 then [] else
    let d1 := String.toInt! (l1.takeWhile cond)
    let fin := getInts (l1.dropWhile cond)
  d1 :: fin

/-- Transpose an array of strings. -/
def Array.transpose (s : Array String) : Array String :=
  let rows := s.map (List.toArray ∘ String.toList)
  let cols := rows[0]!.size
  Id.run do
    let mut ans := #[]
    for c in [:cols] do
      let mut row := ""
      for r in [:rows.size] do
        row := row.push (rows[r]!.getD c default)
      ans := ans.push row
    return ans

/-- A `pos`ition is a pair of integers. -/
abbrev pos := Int × Int

/-- the four directions `L`eft, `R`ight, `U`p, `D`own,
and... `S`tay. -/
inductive dir | L | R | U | D | S
  deriving BEq, DecidableEq, Inhabited, Repr, Hashable

/-- represent each direction by the corresponding arrow. -/
instance : ToString dir where
  toString | .L => "←" | .R => "→" | .U => "↑" | .D => "↓" | .S => "·"

/-- `dir.toPos` converts a `dir`ection to the corresponding unit vector. -/
def dir.toPos : dir →  pos
  | .D => (  1,   0)
  | .U => (- 1,   0)
  | .L => (  0, - 1)
  | .R => (  0,   1)
  | .S => (  0,   0)

/-- `Char.toDir` converts a single character to the corresponding unit vector. -/
def Char.toDir : Char → dir
  | '<' => .L
  | '>' => .R
  | '^' => .U
  | 'v' => .D
  | _ => .S

section meta
open Lean Elab Command

/-- `#assert x` takes a `Bool`ean `x` and fails if `x` is `false`.
It runs `run_cmd Elab.Command.liftTermElabM do guard x`-/
macro (name := cmdAssert) "#assert" cmd:term : command =>
  `(command| run_cmd Elab.Command.liftTermElabM do guard $cmd)

/-- `solve pt answer` runs function `part1` if `pt = 1` and function `part2` if `pt = 2`
on declaration `input`, expecting that it evaluates to `answer`.
If it does, then it prints a summary, otherwise it fails.

The variant `solve pt answer file` assumes that the code should be run on the whole string input,
rather than on its lines.

Finally, the `answer` argument is optional: if it is not provided, `solve` will not guard
for the computed value.

Example usage:
```lean
solve 1 15    -- parses the input as an array of strings, errors if answer does not match `15`
solve 2 629   -- parses the input as an array of strings, errors if answer does not match `629`

solve 1 15  file  -- parses the input as a string, errors if answer does not match `15`
solve 2 file      -- parses the input as a string, no error
```
-/
elab "solve" part:num n:(num)? f:("file")?: command => do
  let nn ← match n with
    | some stx =>  `((some $stx))
    | none =>  `((none))
  let p1 := mkIdent <| match part with
    | `(1) => `part1
    | `(2) => `part2
    | _ => default
  let inp := mkIdent `input
  let rf := mkIdent <| if f.isSome then `IO.FS.readFile else `IO.FS.lines
  elabCommand (← `(command|
    #eval show MetaM _ from do
      let day := ((System.FilePath.toString $inp).getNats)[0]!
      let answer := $p1 <| ← $rf $inp
      IO.println <| f!"Day {day}, part {$part}: {answer}"
      let ans := ($nn).getD answer
      guard (answer == ans) <|> throwError "Computed {answer}\nExpected {ans}"))

end meta

/-- a utility function to display arrays of strings.
It assumes that the strings all have the same length,
it also surrounds the data with dashes/vertical bars.
-/
def draws (ar : Array String) : IO Unit := do
  let sep := String.mk <| List.replicate (ar[0]!.length + 2) '-'
  IO.println <| sep
  for i in ar do
    IO.println s!"|{i}|"
  IO.println <| sep

/-- a utility function to display arrays of strings.
It assumes that the strings all have the same length,
it also surrounds the data with vertical bars and
a primitive row/column count, displaying the last
digit of each row/column.
-/
def draw (s : Array String) : IO Unit := do
  let width := s[0]!.length
  let length := s.size

  let ns := String.mk <| (List.range width).map fun n =>
    (Nat.toDigits 10 n).getLast!
  let pns := (if (10 < length) then " " else "") ++ "--" ++ ns ++ "-"
  IO.println pns
  for i in [:s.size] do
    let pad := (if (10 < length ∧ i < 10) then " " else "") ++ ⟨Nat.toDigits 10 i⟩
    IO.println s!"{pad}|{s[i]!}|"
  IO.println s!"{pns}\n"

/-- `toPic gr Nx Ny` takes as input
* an array of positions `gr`;
* a bound `Nx` for the largest `x`-coordinate of an entry of `gr`;
* a bound `Ny` for the largest `y`-coordinate of an entry of `gr`.

It returns an array of strings where
* a location appearing in `gr` features the `#` character;
* a location not appearing in `gr` features the `.` character.

This is useful to "visualise" `gr` as positions in a grid.
The output can be passed to `draw`. -/
def toPic (gr : Array pos) (Nx Ny : Nat) : Array String :=
  Id.run do
    let mut rows : Array String := #[]
    for i in [:Ny] do
      let mut str := ""
      for j in [:Nx] do
        if gr.contains (i, j) then str := str.push '#' else str := str.push '.'
      rows := rows.push str
    return rows

section tests

#assert "0 2 -3".getInts = [0, 2, -3]

end tests
