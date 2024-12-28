import Advents.Utils
open Lean

namespace Day20

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day20.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
The main data for the puzzle.
* `grid` is the collection of positions that a program can occupy.
* `E` is the final position.
-/
structure Race where
  /-- `grid` is the collection of positions that a program can occupy. -/
  grid : Std.HashSet pos
  /-- `E` is the final position. -/
  E : pos

/-- Returns all positions with L¹-distance at most `n` from the origin. -/
def posAtMost (n : Nat) : Std.HashSet pos :=
  (Array.range (n + 1)).foldl (init := ∅) fun tot p =>
    tot.insertMany <|
      (Array.range (n + 1 - p)).foldl (init := (∅ : Std.HashSet pos)) fun tot' q =>
        let p : Int := p.cast
        let q : Int := q.cast
        tot'.union ({(p, q), (p, - q), (- p, q), (- p, - q)})

/-- Returns the distances to `E` along the unique path that programs can follow without cheating. -/
def path (r : Race) : Std.HashMap pos Nat := Id.run do
  let mut curr := r.E
  let mut dists := {(curr, 0)}
  let mut gr := r.grid.erase curr
  let nbs := (posAtMost 1).erase default
  while !gr.isEmpty do
    for n in nbs do
      let newPos := curr + n
      if gr.contains newPos then
        gr := gr.erase newPos
        dists := dists.insert newPos (dists[curr]! + 1)
        curr := newPos
  return dists

/-- Converts the input data into a `Race`. -/
def inputToRace (dat : Array String) : Race where
  grid := sparseGrid dat "SE.".contains
  E    := (sparseGrid dat "E".contains).toArray[0]!

/-- A utility function to draw a `Race`. This was useful while figuring out the answer. -/
def drawRace (r : Race) : IO Unit := do
  let sz := if r.grid.size ≤ 1000 then 15 else 141
  draw <| drawSparse r.grid sz sz

/-- `parts dat` takes as input the input of the problem and returns the solution to part 2. -/
def parts (dat : Array String) (cheat : Nat) (saving : Nat) : Nat := Id.run do
  let r := inputToRace dat
  let path := path r
  let poss := posAtMost cheat
  let mut improve : Std.HashMap Nat Nat := ∅
  for (p, pToE) in path do
    for shift in poss do
      let q := p + shift
      let dist := shift.1.natAbs + shift.2.natAbs
      if let some qToE := path[q]? then
        improve := improve.alter (qToE - pToE - dist) (some <| ·.getD 0 + 1)
  return improve.fold (· + if saving ≤ · then · else 0) 0

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) (saving : Nat := 100) : Nat := parts dat 2 saving

-- The puzzle did not contain an example with savings of over 100ps, so I made up this test.
#assert part1 atest 20 == 5

solve 1 1445

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) (saving : Nat := 100) : Nat := parts dat 20 saving

-- The puzzle did not contain an example with savings of over 100ps, so I made up this test.
#assert part2 atest 50 == 285

--set_option trace.profiler true in solve 2 1008040  -- takes about 1 minute

end Day20
