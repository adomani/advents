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

def shift (as : Array Nat) : Array Nat :=
  let first := as[0]!
  let rest := (as.erase first).push first
  rest.modify 6 (· + first)

def shiftMany (as : Array Nat) : Nat → Array Nat
  | 0 => as
  | n + 1 => shift (shiftMany as n)

#eval
  let dat := (Array.range 9).map test.getNats.count
  ((shiftMany dat 256).sum, (shiftMany dat 256))

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let dat := (Array.range 9).map dat.getNats.count
  (shiftMany dat 256).sum

#assert part2 test == 26984457539

solve 2 1710623015163 file

end Day06
