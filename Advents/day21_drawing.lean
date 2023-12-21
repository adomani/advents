import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day21.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- the four directions `L`eft, `R`ight, `U`p, `D`own. -/
inductive dir | L | R | U | D
  deriving BEq, DecidableEq, Inhabited, Repr

/-- represent each direction by the corresponding arrow. -/
instance : ToString dir where
  toString | .L => "←" | .R => "→" | .U => "↑" | .D => "↓"

/-- `dirs` is the list of directions `[.U, .D, .L, .R]`. -/
abbrev dirs : List dir := [.U, .D, .L, .R]

/-- converts a unit vector into the direction that is
obtained by a counter-clockwise rotation.
It is useful for defining orientations. -/
def toPos : dir → pos
  | .L => (  0, - 1)
  | .R => (  0,   1)
  | .U => (- 1,   0)
  | .D => (  1,   0)

/-- finds the character `S` in `dat`, returning its two integer coordinates. -/
def findS (dat : Array String) : pos :=
  let sx :=
    Id.run do
      let mut x := 0
      for i in [:dat.size] do
        if dat[i]!.contains 'S' then
          x := i
      return x
  (sx, (dat[sx]!.find (· == 'S')).byteIdx)

#assert
  let dat := atest
  findS dat == (↑(dat.size / 2), ↑(dat.size / 2))

#assert
  let dat := ← IO.FS.lines input
  findS dat == (↑(dat.size / 2), ↑(dat.size / 2))

/-- returns the position of the first available rock (`#`) in `dat`. -/
def findRk (dat : Array String) : pos :=
  match dat.findIdx? (String.contains · '#') with
    | none => dbg_trace "no rocks!"; default
    | some s => (s, (dat[s]!.find (· == '#')).byteIdx)

#assert findRk atest == (1, 5)
#assert findRk (← IO.FS.lines input) == (1, 19)

/-- creates the `HashSet` containing all the positions of all the rocks in `dat`. -/
def getRocks (dat : Array String) (c : Char := '#') : HashSet pos :=
  Id.run do
    let mut rks : HashSet pos := .empty
    for i in [:dat.size] do
      let row := dat[i]!.toList
      for j in [:row.length] do
        if row[j]! == c then
          rks := rks.insert (i, j)
    return rks

/-- `mvs rk gd bd f` takes as input
* `rk`, the locations of the rocks;
* `gd`, the possible locations of the gardener;
* `bd`, a layer of starting points for potential new positions;
* `f`, an auxilliary function to possibly modify the current position (this is useful for part 2).

It returns a pair consisting of
* the locations where the gardener could be in one more step and
* the array of those positions that were really added to `gd`.

The expectation is that `mvs` will be ran recursively, feeding back its output as new `gd` and
`bd` inputs to itself, until we built the whole `HashSet` of reachable positions. -/
def mvs (rk gd : HashSet pos) (bd : Array pos) (f : pos → pos) : HashSet pos × Array pos :=
  Id.run do
  let mut new : HashSet pos := gd
  let mut nbd := #[]
  for g in bd do
    let mvs := (dirs.map (toPos · + g)).filter fun x => (rk.find? (f x)).isNone
    for m in mvs do
      let (n, tf) := new.insert' m
      new := n
      if !tf then nbd := nbd.push m
  return (new, nbd)

/-- `parts dat n f` takes as input the input of the problem, an optional number of iterations
`n` and an optional function `f : pos → pos`.
It returns the possible locations that the gardener can reach in `n` steps.

It recursively applies `mvs` to its output, starting from the position of `S` and
the layout of the garden. -/
def parts (dat : Array String) (n : Nat := 64) (f : pos → pos := id) : HashSet pos :=
  let rk := getRocks dat
  Id.run do
  let init := findS dat
  let mut bd := #[init]
  let mut gd : HashSet pos := HashSet.empty.insert init
  for _ in [:n] do
    (gd, bd) := (mvs rk gd bd f)
  return gd

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1.

It uses `parts` as the main program. -/
def part1 (dat : Array String) (n : Nat := 64) : Nat :=
  ((parts dat n).toArray.filter fun (x, y) => (x + y) % 2 == n % 2).size

#assert part1 atest 6 == 16

solve 1 3764

/-!
#  Question 2
-/

/-!
We can attempt to solve the second part working with coordinates modulo the grid size.
-/
/- test that we can really solve the second part working modulo the size of the grid
#eval do show MetaM _ from do
  let dat := atest
  let sz := dat.size
  let mut tots := #[]
  for d in [6, 10, 50, 100, 500] do
    tots := tots.push <| part1 dat d (fun (x, y) => (x % sz, y % sz))
  IO.println <| tots
  guard (tots == #[16, 50, 1594, 6536, 167004])
-/

/-!
However, the test is already slow and I do not have the patience to even check the remaining
two data points, `d = 1000, 5000`.

Besides, we should reach up to `26501365`, so we proceed differently.

We are going to proceed in "bulk", counting all "full" gardens at once
and only resorting to evolving our moves for the fringes of the edge gardens.

Let's check that the row and column containing `S` contains only `.`s, besides `S` itself.
-/

/-- `onlyS dat` takes as input an array `dat` of strings and returns
* `true` if the row and column containing `S` consist of a single `S` and the rest is `.`s;
* `false` otherwise.

It is `true` for the puzzle input, as you can see from the `#assert` line below.
-/
abbrev onlyS (dat : Array String) : Bool :=
  let (sx, sy) := findS dat
  let s2 := [(sx, dat), (sy, dat.transpose)].map fun (c, t) =>
    String.mk (t[c.toNat]!.toList.filter (! · == '.'))
  s2 == ["S", "S"]

#assert onlyS (← IO.FS.lines input)

/-- `findUnreachable dat` finds all locations of the centres of

 `#`
`#.#`
 `#`

on the layout determined by `dat`.
They turn out to be the only inaccessible locations on the garden that are marked with a `.`.
-/
def findUnreachable (dat : Array String) : Array pos :=
  let rks := getRocks dat
  Id.run do
  let mut out := #[]
  for i in [:dat.size] do
    for j in [:dat.size] do
      let p : pos := (i, j)
      let cond := (dirs.map (rks.find? <| toPos · + p)).all Option.isSome
      if (rks.find? p).isNone ∧ cond then
        out := out.push p
  return out

#assert findUnreachable atest == #[]
#assert findUnreachable (← IO.FS.lines input) == #[(5, 31), (59, 91), (61, 68), (79, 78), (111, 116)]

def draw2 (dat : Array String) (mh : Nat) : Array String × Nat :=
  let unr := findUnreachable dat
  let S := findS dat
  let rk1 := findRk dat
  let r := getRocks dat
  let g := parts dat mh (fun p@(x, y) =>
    if max (x- S.1).natAbs (y- S.2).natAbs ≤ mh then p else rk1)
  Id.run do
  let mut tot := 0
  let mut rows := #[]
  for i in [:130] do
    let mut row := ""
    for j in [:130] do
      let p : pos := (i, j)
      let pd := p - ((65 : Int), (65 : Int))
      tot := tot + 1
      if mh < pd.1.natAbs + pd.2.natAbs then row := row.push ' '; tot := tot - 1 else
      if (r.find? p).isSome then row := row.push '#' else
      if (g.find? p).isSome then row := row.push 'O' else
      if p ∈ unr then row := row.push 'U' else
      row := row.push '.'
    rows := rows.push row
  return (rows, tot)

#eval show MetaM _ from do
  for mh in [8:12] do --let mh := 4
    let dat ← IO.FS.lines input
    let gr := (draw2 dat mh).1
    let gr := (#[gr[0]!] ++ gr.filter fun s => (String.contains s '#' || String.contains s 'O')).push gr[0]!
    let gr := gr.transpose
    let gr := (#[gr[0]!] ++ gr.filter fun s => (String.contains s '#' || String.contains s 'O')).push gr[0]!
    let gr := gr.transpose
    draw <| gr
