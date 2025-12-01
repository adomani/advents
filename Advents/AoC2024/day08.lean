import Advents.Utils
open Std

namespace Day08

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2024"/"day08" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `test2` is another test string for the problem. -/
def test2 := "..........
...#......
..........
....a.....
..........
.....a....
..........
......#...
..........
.........."

/-- `atest2` is another test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/-- `test3` is another test string for the problem. -/
def test3 := "T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
.........."

/-- `atest3` is another test string for the problem, split into rows. -/
def atest3 := (test3.splitOn "\n").toArray

/-- Returns the locations of the antinodes with the given grid, for the input pair of positions. -/
def antinodeLocations (gr : HashSet pos) (p q : pos) : Array pos :=
  let diff := (p - q)
  #[p + diff, q - diff].filterMap gr.get?

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let kinds : HashSet Char := (loadGrid dat id).fold (init := {}) fun gr _p c =>
    if c != '.' then gr.insert c else gr
  let grid := sparseGrid dat (fun _ => true)
  let mut antinodes : HashSet pos := {}
  for c in kinds do
    let antennas := sparseGrid dat (· == c)
    let mut remaining := antennas
    for a1 in remaining do
      remaining := remaining.erase a1
      for a2 in remaining do
        antinodes := antinodes.union <| .ofArray <| antinodeLocations grid a1 a2
  antinodes.size

#assert part1 atest == 14

solve 1 252

/-!
#  Question 2
-/

/--
Returns the locations of the resonant harmonics with the given grid, for the input of positions.
-/
def antinodeResonant (gr : HashSet pos) (p q : pos) : HashSet pos := Id.run do
  if p == q then return {}
  let mut h := {}
  let diff := (p - q)
  let mut p := p
    while gr.contains p do
      h := h.insert p
      p := p + diff
    p := q
    while gr.contains p do
      h := h.insert p
      p := p - diff
  return h

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let kinds : HashSet Char := (loadGrid dat id).fold (init := {}) fun gr _p c =>
    if c != '.' then gr.insert c else gr
  let grid := sparseGrid dat (fun _ => true)
  let mut antinodes : HashSet pos := {}
  for c in kinds do
    let antennas := sparseGrid dat (· == c)
    let mut remaining := antennas
    for a1 in remaining do
      remaining := remaining.erase a1
      for a2 in remaining do
        antinodes := antinodes.union <| antinodeResonant grid a1 a2
  antinodes.size

#assert part2 atest == 34

solve 2 839

end Day08
