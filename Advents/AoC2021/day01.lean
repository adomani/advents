import Advents.Utils

namespace Day01

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2021"/"day01" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "199
200
208
210
200
207
240
269
260
263"

/--
With input a list of natural numbers and a shift, compute how often is an entry of the list strictly smaller
than the entry in the current position plus the shift.

Only counts the indices that are withing the range of the list.
-/
def compareShifts (dat : List Nat) (shift : Nat) : Nat :=
  Id.run do
  let mut previous := dat[0]!
  let mut increases := 0
  for i in [shift:dat.length] do
    if previous < dat[i]! then increases := increases + 1
    previous := dat[i - shift + 1]!
  return increases

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := compareShifts dat.getNats 1

#assert part1 test == 7

solve 1 1167 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := compareShifts dat.getNats 3

#assert part2 test == 5

solve 2 1130 file

end Day01
