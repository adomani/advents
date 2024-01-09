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

/-- `dirs` is the list of directions `[.U, .D, .L, .R]`. -/
abbrev dirs : List dir := [.U, .D, .L, .R]

/-- converts a unit vector into the direction that is
obtained by a counter-clockwise rotation.
It is useful for defining orientations. -/
def toCCWPos : dir → pos
  | .L => (  0, - 1)
  | .R => (  0,   1)
  | .U => (- 1,   0)
  | .D => (  1,   0)
  | .X => (  0,   0)

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
    let mvs := (dirs.map (toCCWPos · + g)).filter fun x => (rk.find? (f x)).isNone
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
