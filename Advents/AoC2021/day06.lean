import Advents.Utils
open Lean

namespace Day06

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day06.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "3,4,3,1,2"

/-- Convert an array of lanterfish to what it becomes the following day. -/
def step (s : Array Nat) : Array Nat := Id.run do
  let mut t := s
  for i in [0:s.size] do
    if s[i]! == 0 then
      t := (t.modify i (fun _ => 6)).push 8
    else
      t := t.modify i (· - 1)
  return t

/-- Convert an array of lanterfish to what it becomes after a given number of days. -/
def stepMany (s : Array Nat) : Nat → Array Nat
  | 0 => s
  | n + 1 => step (stepMany s n)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let dat := dat.getNats.toArray
  (stepMany dat 80).size

#assert part1 test == 5934

solve 1 380758 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := sorry

--#assert part2 atest == ???

--solve 2

end Day06
