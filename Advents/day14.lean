import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day14.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `getInfo s` takes as input a string `s` and returns
the list os pairs `(wt, pos)`, where
* `wt` is the number of `O`s contained between consecutive `#`s
  in `s`;
* `pos` is the position of the `#` that immediately precedes the
  current block of `O`s.
-/
def getInfo (s : String) : List (Nat × Nat) :=
  let pc := s.toList
  let lth := pc.length
  let breaks := lth :: (pc.findIdxs (· == '#')).map (lth - 1 - ·)
  let moving := ((pc.filter (! · == '.')).splitOn '#').map List.length
  moving.zip breaks

/-- `getLoad dat` takes as input a list of pairs of natural numbers
and returns the sum of a "binomial-like" count for each entry
of `s`.

This is the "total load" for one row in part 1.
-/
def getLoad (dat : List (Nat × Nat)) : Nat :=
  Id.run do
  let mut tot := 0
  for (wt, pos) in dat do
    tot := tot + ((List.range wt).map (pos - ·)).sum
  return tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let infos := dat.transpose.map getInfo
  (infos.map getLoad).sum

#assert part1 atest == 136

solve 1 110821

/-!
#  Question 2
-/

#exit

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
