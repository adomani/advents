import Advents.Utils
open Std

namespace AoC2025_Day09

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day09" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"

/-
........3←2
........↓.↑
.5←←←←←←4.↑
.↓........↑
.6→→→→7...↑
......↓...↑
......0→→→1
-/


/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
Converts the input to the `HashSet` of positions of the red tiles.
-/
def inputToPos (dat : Array String) : HashSet pos :=
  dat.foldl (init := ∅) fun tot s => match s.getNats with | [x, y] => tot.insert (x, y) | _ => tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let gr := inputToPos dat
  let mut maxArea := 0
  let mut left := gr
  for A@(a, b) in gr do
    left := left.erase A
    for (c, d) in left do
      let newArea := ((a - c).natAbs + 1) * ((b - d).natAbs + 1)
      maxArea := max maxArea newArea
  return maxArea


#assert part1 atest == 50

set_option trace.profiler true in solve 1 4767418746

/-!
#  Question 2
-/

#exit

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day09
