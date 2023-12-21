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

/-- given the locations of the rocks and the possible locations of the gardener
returns the locations where the gardeners could be in one more step. -/
def mvs (rk gd : HashSet pos) : HashSet pos :=
  Id.run do
  let mut new : HashSet pos := .empty
  for g in gd do
    let mvs := ([.U, .D, .L, .R].map (toPos · + g)).filter fun x => (rk.find? x).isNone
    for m in mvs do
      new := new.insert m
  return new

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) (n : Nat := 64) : Nat :=
  let rk := getRocks dat
  Id.run do
  let mut gd : HashSet pos := HashSet.empty.insert (findS dat)
  for _ in [:n] do
    gd := (mvs rk gd)
  return gd.size

#assert part1 atest 6 == 16

solve 1 3764

/-!
#  Question 2
-/

namespace Part_2_brute

/-- Similar to `mvs`, except that it reduces the coordinates modulo the size of the grid
before checking if they belong to `rk`.

This is good for a period configuration of rocks. -/
def mvsmod (rk gd : HashSet pos) (sz : Nat) : HashSet pos :=
  Id.run do
  let mut new : HashSet pos := .empty
  for g in gd do
    let mvs := ([.U, .D, .L, .R].map (toPos · + g)).filter fun x => (rk.find? (x.1 % sz, x.2 % sz)).isNone
    for m in mvs do
      new := new.insert m
  return new

/-- Similar to `part1`, except that it uses `mvsmod` instead of `mvs`.
Effectively, it reduces the coordinates modulo the size of the grid
before checking if they correspond to a rock.

This is good for a period configuration of rocks. -/
def part1mod (dat : Array String) (sz : Nat) (n : Nat := 64) : Nat :=
  let rk := getRocks dat
  Id.run do
  let mut gd : HashSet pos := HashSet.empty.insert (findS dat)
  for _ in [:n] do
    gd := (mvsmod rk gd sz)
  return gd.size

/-!
Test that we can really solve the second part working modulo the size of the grid.
-/
/-
#eval do show MetaM _ from do
  let mut tots := #[]
  for d in [6, 10, 50, 100] do
    tots := tots.push <| part1mod atest atest.size d
  IO.println <| tots
  guard (tots == #[16, 50, 1594, 6536])
--/

end Part_2_brute

/-!
However, we are going to proceed by counting in bulk, by couting all "full" gardens at once
and only resorting to evolving our moves for the fringes of the garden.
-/

/-- `onlyS dat` takes as input an array `dat` of strings and returns
* `true` if the row and column containing `S` consist of a single `S` and the rest is `.`s;
* `false` otherwise.

It is `true` for the puzzle input, as you can see from the `#assert` line below.
-/
def onlyS (dat : Array String) : Bool :=
  let (sx, sy) := findS dat
  let s2 := [(sx, dat), (sy, dat.transpose)].map fun (c, t) =>
    String.mk (t[c.toNat]!.toList.filter (! · == '.'))
  s2 == ["S", "S"]

#assert onlyS (← IO.FS.lines input)


/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2
