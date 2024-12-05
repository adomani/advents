import Advents.Utils
open Lean

namespace Day_newDay_

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoCYYYY/day_newDay_.input"

/-!
#  Day 5

-- ### Description
-/

/-- `test` is the test string for the problem. -/
def test := ""

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-!
# Part 1

--
-/

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--solve 1

/-!
# Part 2

--
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day_newDay_
