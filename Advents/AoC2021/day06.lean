import Advents.Utils
open Std

namespace Day06

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day06.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "3,4,3,1,2"

/-- Convert an array of tallied lanterfish to what it becomes the following day. -/
def shift (as : Array Nat) : Array Nat :=
  let first := as[0]!
  let rest := (as.erase first).push first
  rest.modify 6 (· + first)

/-- Convert an array of tallied lanterfish to what it becomes after a given number of days. -/
def shiftMany (as : Array Nat) : Nat → Array Nat
  | 0 => as
  | n + 1 => shiftMany (shift as) n

/--
Convert a list `as` of lanternfish and a `n` number of days to
the number of lanternfish that are present after `n` days.
-/
def tallyAndCountAfter (as : List Nat) (n : Nat) : Nat :=
  let dat := (Array.range 9).map as.count
  (shiftMany dat n).sum

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := tallyAndCountAfter dat.getNats 80

#assert part1 test == 5934

solve 1 380758 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := tallyAndCountAfter dat.getNats 256

#assert part2 test == 26984457539

solve 2 1710623015163 file

end Day06
