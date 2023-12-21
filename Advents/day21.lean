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

/-- creates the `HashSet` containing all the positions of all the rocks in `dat`. -/
def getRocks (dat : Array String) : HashSet pos :=
  Id.run do
    let mut rks : HashSet pos := .empty
    for i in [:dat.size] do
      let row := dat[i]!.toList
      for j in [:row.length] do
        if row[j]! == '#' then
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
    let mvs := ([.U, .D, .L, .R].map (toPos · + g)).filter fun x => (rk.find? (f x)).isNone
    for m in mvs do
      let (n, tf) := new.insert' m
      new := n
      if !tf then nbd := nbd.push m
  return (new, nbd)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1.

It recursively applies `mvs` to its output, starting from the position of `S` and
the layout of the garden. -/
def part1 (dat : Array String) (n : Nat := 64) (f : pos → pos := id) : Nat :=
  let rk := getRocks dat
  Id.run do
  let init := findS dat
  let mut bd := #[init]
  let mut gd : HashSet pos := HashSet.empty.insert init
  for _ in [:n] do
    (gd, bd) := (mvs rk gd bd f)
  return (gd.toArray.filter fun (x, y) => (x + y) % 2 == n % 2).size

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

/-- given the locations of the rocks and the possible locations of the gardener
returns the locations where the gardeners could be in one more step. -/
def mvsF (rk gd : HashSet pos) (f : pos → Bool) : HashSet pos :=
  Id.run do
  let mut new : HashSet pos := gd
  for g in gd do
    let mvs := ([.U, .D, .L, .R].map (toPos · + g)).filter fun x => f x && (rk.find? x).isNone
    for m in mvs do
      new := new.insert m
  return new

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def partF (dat : Array String) (f : pos → Bool) (n : Nat := 64) : Nat :=
  let rk := getRocks dat
  Id.run do
  let mut gd : HashSet pos := HashSet.empty.insert (findS dat)
  for _ in [:n] do
    gd := (mvsF rk gd f)
  return gd.size

def partFUpto (dat : Array String) (f : pos → Bool) : Nat × HashSet pos :=
  let rk := getRocks dat
  Id.run do
  let init := findS dat
  let mut boundary := #[init]
  let mut gd : HashSet pos := HashSet.empty.insert (init)
  let mut steps := 0
  let mut (past, curr) := (0, gd.size)
  while past ≠ curr do
    steps := steps + 1
    past := curr
    gd := (mvsF rk (mvsF rk gd f) f)
    curr := gd.size
  return (2 * (steps - 1), gd)

--  determine the number of steps and the number of locations that
--  the gardener can visit while staying in a single fundamental
--  domain for the L¹-norm.
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let res := partFUpto dat (fun (x, y) => x.natAbs + y.natAbs ≤ 65)
--  IO.println res

#eval 0

--  determine the number of steps and the number of locations that
--  the gardener can visit while staying in a single fundamental
--  domain for the L¹-norm.
#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let S := findS dat
  let res := partFUpto dat (fun (x, y) => max (x.natAbs- S.1) (y.natAbs- S.2) ≤ 6)
--  draw <| toPic res.2.toArray dat.size dat.size
--  IO.println (res.1, res.2.size)


#eval
  let steps := 26501365
  let (q, r) := (steps / 65, steps % 65)
  (q, r, q * 65 + r == steps)

#eval do
  let dat ← IO.FS.lines input
  draw dat

/-
After 74 steps, the gardener visits `4206` places in the garden.
This is the maximum number of places that gardener can visit in a single fundamental domain.
-/

/-
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let res := partF dat (fun (x, y) => x.natAbs + y.natAbs ≤ 65) 74
  IO.println res
-/

--def fillUp (dat : Array String)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2
