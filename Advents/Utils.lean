import Batteries

section sums
variable {α}

/--  Sum the elements of an `Array`. -/
def Array.sum [Add α] [OfNat α 0] (l : Array α) : α :=
  l.toList.sum

/--  Multiply the elements of a `List`. -/
def List.prod [Mul α] [OfNat α 1] : List α → α
  | []    => 1
  | [m]   => m
  | m::ms => m * ms.prod

/--  Multiply the elements of an `Array`. -/
def Array.prod [Mul α] [OfNat α 1] (l : Array α) : α :=
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

/-- `String.getNats l` takes as input a string and returns the list of `Nat`
where each entry is the natural number corresponding to each consecutive
sequence of digits in `l`, in their order. -/
partial
def String.getNats (l : String) : List Nat :=
  let l1 := l.dropWhile (!Char.isDigit ·)
  if l1.isEmpty then [] else
    let d1 := String.toNat! ⟨l1.toList.takeWhile (Char.isDigit ·)⟩
    let fin := getNats (l1.dropWhile (Char.isDigit ·))
  d1 :: fin

/-- `String.getInts l` takes as input a string `l`, removes everything that is neither a digit,
not a minus sign (`-`) and interprets the rest as a list of integers. -/
partial
def String.getInts (l : String) : List Int :=
  let cond : Char → Bool := fun c => (Char.isDigit c) || (c == '-')
  let l1 := l.dropWhile (!cond ·)
  if l1.isEmpty then [] else
    let d1 := String.toInt! (l1.takeWhile cond)
    let fin := getInts (l1.dropWhile cond)
  d1 :: fin

section Nats_and_Ints

/-- `Nat.factors n` returns the array of prime factors of `n`, with repetitions,
in decreasing order. -/
def Nat.factors (n : Nat) (p : Nat := 2) : Array Nat :=
  if p0 : p = 0 then #[0] else if p1 : p = 1 then #[n] else if pn : n ≤ p then #[n] else
  match n with
    | 0 => #[0]
    | 1 => #[]
    | n =>
      have : n / p < n := by
        apply Nat.div_lt_self (Nat.pos_of_ne_zero ?_)
        · apply Nat.lt_of_le_of_ne ?_ (Ne.symm p1)
          exact Nat.succ_le.mpr <| Nat.pos_of_ne_zero p0
        · apply Nat.ne_zero_iff_zero_lt.mpr
          exact Nat.lt_of_le_of_lt p.zero_le (Nat.not_le.mp pn)
      if n % p = 0 then ((n / p).factors p).push p
      else if n.sqrt < p then #[n]
      else
        have : n - p.succ < n - p := Nat.sub_lt_sub_left (Nat.not_le.mp pn) p.lt_succ_self
        n.factors p.succ
  termination_by (n, n - p)

/-- `Int.natFactors n` returns the array of prime factors of `n.natAbs`, with repetitions,
in decreasing order.
If you want to remember the sign of `n`, use `Int.factors` instead. -/
def Int.natFactors (n : Int) : Array Nat :=
  n.natAbs.factors

/-- `Int.factors n` returns the array of natural prime factors of `n`, with repetitions,
in decreasing order, preceded by `-1` if `n` is negative. -/
def Int.factors (n : Int) : Array Int :=
  let nf := n.natFactors.map Nat.cast
  if 0 ≤ n then nf else #[-1] ++ nf

/-- `Nat.factorial n` -- the factorial of `n`. -/
def Nat.factorial : Nat → Nat
  | 0 => 1
  | n + 1 => (n + 1) * n.factorial

/-- `Nat.binom n k` -- the binomial coefficient `n choose k`. `n` is allowed to be an integer. -/
def Nat.binom (n : Nat) (k : Nat) : Nat :=
  ((List.range k).map (n - ·)).prod / k.factorial

end Nats_and_Ints

/-- Transpose an array of arrays, possibly assumes that every
row and column has the same number of entries. -/
def Array.transpose [Inhabited α] (rows : Array (Array α)) : Array (Array α) :=
  let cols := rows[0]!.size
  Id.run do
    let mut ans := #[]
    for c in [:cols] do
      let mut row := #[]
      for r in [:rows.size] do
        row := row.push (rows[r]!.getD c default)
      ans := ans.push row
    return ans

#guard
  #[#[0, 1], #[2, 3]].transpose == #[#[0, 2], #[1, 3]]

/-- Transpose an array of strings. -/
def Array.transposeString (s : Array String) : Array String :=
  let rows := s.map (List.toArray ∘ String.toList)
  rows.transpose.map (String.mk ∘ Array.toList)

/-- A `pos`ition is a pair of integers. -/
abbrev pos := Int × Int

/-- the four directions `L`eft, `R`ight, `U`p, `D`own,
and... `X`tay. -/
inductive dir | L | R | U | D | X
  deriving BEq, DecidableEq, Inhabited, Repr, Hashable

/-- represent each direction by the corresponding arrow. -/
instance : ToString dir where
  toString | .L => "←" | .R => "→" | .U => "↑" | .D => "↓" | .X => "·"

/-- `dir.rev` reverses a `dir`ection. -/
def dir.rev : dir →  dir
  | .D => .U
  | .U => .D
  | .L => .R
  | .R => .L
  | .X => .X

/-- `dir.toPos` converts a `dir`ection to the corresponding unit vector. -/
def dir.toPos : dir →  pos
  | .D => (  1,   0)
  | .U => (- 1,   0)
  | .L => (  0, - 1)
  | .R => (  0,   1)
  | .X => (  0,   0)

/-- `Char.toDir` converts a single character to the corresponding unit vector. -/
def Char.toDir : Char → dir
  | '<' => .L
  | '>' => .R
  | '^' => .U
  | 'v' => .D
  | _ => .X

-- More functional, but less performant implementation
/-
def loadString (s : String) (r : Nat) : Std.HashMap pos Char :=
  .ofList <| (List.range s.length).map fun i : Nat => ((r, i), s.get ⟨i⟩)

def loadGrid (dat : Array String) : Std.HashMap pos Char :=
  (Array.range dat.size).foldl (fun i => ·.union (loadString dat[i]! i)) ∅
-/

/--
Converts the input strings into a `HashMap`.
Only the characters on which `toEntry` is `some` appear as keys.
-/
def sparseMap (dat : Array String) (toEntry : Char → Option α) : Std.HashMap pos α := Id.run do
  let mut h := {}
  for d in [0:dat.size] do
    let row := dat[d]!
    for c in [0:row.length] do
      match toEntry (row.get ⟨c⟩) with
        | none => continue
        | some a => h := h.insert (d, c) a
  return h

/--
Converts the input strings into a `HashMap`.
Uses *every* character in every string of `dat : Array String`.
-/
def loadGrid {α} (dat : Array String) (toEntry : Char → α) : Std.HashMap pos α :=
  sparseMap dat (some ∘ toEntry)

/--
Converts the input strings into a `HashMap`, assuming that the entries are natural numbers.
-/
def loadGridNats (dat : Array String) : Std.HashMap pos Nat := loadGrid dat (String.toNat! ⟨[·]⟩)

def sparseGrid (dat : Array String) (toEntry : Char → Bool) : Std.HashSet pos := Id.run do
  let mut h := {}
  for d in [0:dat.size] do
    let row := dat[d]!
    for c in [0:row.length] do
      if toEntry (row.get ⟨c⟩) then
        h := h.insert (d, c)
  return h

section meta
open Lean Elab Command

/-- `#assert x` takes a `Bool`ean `x` and fails if `x` is `false`.
It runs `run_cmd Elab.Command.liftTermElabM do guard x`-/
macro (name := cmdAssert) "#assert " cmd:term : command =>
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
elab "solve " part:num nn:(ppSpace term:max)? f:(&" file")?: command => do
  let nn ← match nn with
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
      let year := ((System.FilePath.toString $inp).getNats)[0]!
      let day := ((System.FilePath.toString $inp).getNats)[1]!
      let answer := $p1 <| ← $rf $inp
      IO.println <| f!"Day {day}, {year}, part {$part}: {answer}"
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

/-- A function to draw `HashMap`s. -/
def drawHash {α} [ToString α] (h : Std.HashMap pos α) (Nx Ny : Nat) : Array String := Id.run do
  let mut fin := #[]
  for i in [0:Nx] do
    let mut str := ""
    for j in [0:Ny] do
      match h.get? (i, j) with
        | some d => str := str ++ s!"{d}"
        | none => str := str.push ' '
    fin := fin.push str
  return fin

/-- A function to draw `HashSet`s. -/
def drawSparseWith (h : Std.HashSet pos) (Nx Ny : Nat)
    (yes : pos → String := fun _ => "#") (no : pos → String := fun _ => ".") :
    Array String := Id.run do
  let mut fin := #[]
  for i in [0:Nx] do
    let mut str := ""
    for j in [0:Ny] do
      match h.get? (i, j) with
        | some d => str := str ++ (yes d)
        | none => str := str ++ (no (i, j))
    fin := fin.push str
  return fin

/-- A function to draw `HashSet`s. -/
def drawSparse (h : Std.HashSet pos) (Nx Ny : Nat) (yes : String := "#") (no : String := "·") :
    Array String := Id.run do
  let mut fin := #[]
  for i in [0:Nx] do
    let mut str := ""
    for j in [0:Ny] do
      match h.get? (i, j) with
        | some _d => str := str ++ yes
        | none => str := str ++ no
    fin := fin.push str
  return fin

section tests

#assert "0 2 -3".getInts = [0, 2, -3]

#assert Id.run do
  let mut tots := true
  for n in [0:5000] do
    tots := tots && n == n.factors.prod
  tots

#assert Nat.binom 5 3 = 10
--#assert Int.binom (-5) 2 = 15

#assert (List.range 12).map (Nat.binom 10) == [1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1, 0]

end tests
