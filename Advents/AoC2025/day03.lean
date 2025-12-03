import Advents.Utils

namespace AoC2025_Day03

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day03" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "987654321111111
811111111111119
234234234234278
818181911112111"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
Converts the input string to the list of digits composing it.

*Note*.  This function assumes that the initial list consists only of digits.
-/
def inputToDigits (s : String) : List Nat :=
  s.toList.map fun c => ("".push c).toNat!

/--
`getMaxBefore dat left` finds the earliest largest value in the input list `dat` that happens
at least `left` positions from the end.

It returns the largest value found, as well as the residual list that follows the value that was
found.
-/
def getMaxBefore (dat : List Nat) (left : Nat) : Nat × List Nat :=
  let cands := (dat.reverse.drop (left - 1)).reverse
  let m1 := cands.max?.getD 0
  let i1 := dat.findIdx (· == m1)
  let l2 := dat.drop (i1 + 1)
  (m1, l2)

/--
This is the cumulative version of `getMaxBefore`:
extract `left` consecutive maxima from the input list `dat`, storing them in `acc`,
*assuming that they are digits of a number to base 10*.
-/
partial
def getNMaxs (acc : Nat) (dat : List Nat) (left : Nat) : Nat :=
  if left == 0 then acc else
  let (newMax, newDat) := getMaxBefore dat left
  getNMaxs (10 * acc + newMax) newDat (left - 1)

/--
Convert the input data `dat` into an array of joltages.
Then sum the largest sublists of length `n` that can be extracted.

This is the common shape of the solutions to the two parts.
-/
def sols (dat : Array String) (n : Nat) : Nat :=
  let digs := dat.map inputToDigits
  (digs.map (getNMaxs 0 · n)).sum

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sols dat 2

#assert part1 atest == 357

solve 1 17100

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sols dat 12

#assert part2 atest == 3121910778619

solve 2

end AoC2025_Day03
