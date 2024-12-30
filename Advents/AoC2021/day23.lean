import Advents.Utils
open Lean

namespace Day23

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day23.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure AP where
  grid : Std.HashSet pos
  A : Std.HashSet pos
  B : Std.HashSet pos
  C : Std.HashSet pos
  D : Std.HashSet pos
  energy : Nat

def inputToAP (dat : Array String) : AP where
  grid := sparseGrid dat ("ABCD.".toList.contains ·)
  A := sparseGrid dat ("A".toList.contains ·)
  B := sparseGrid dat ("B".toList.contains ·)
  C := sparseGrid dat ("C".toList.contains ·)
  D := sparseGrid dat ("D".toList.contains ·)
  energy := 0

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--set_option trace.profiler true in solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day23
