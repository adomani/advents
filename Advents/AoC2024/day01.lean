import Advents.Utils
open Lean

namespace Day01

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day01.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "3   4
4   3
2   5
1   3
3   9
3   3"

/-- Given the input string, extract the arrays of left and right potential locations. -/
def inputToArrays (i : String) : Array Nat × Array Nat :=
  let nats := i.getNats
  Id.run do
  let mut (left, right) : Array Nat × Array Nat := default
  for i in [:nats.length / 2] do
    left := left.push nats[2 * i]!
    right := right.push nats[2 * i + 1]!
  return (left, right)

/--
Given a pair of arrays of natural numbers, sort each, compute the distances of the corresponding
entries of the sorted lists and return the total sum of the distances.
-/
def dataToSol (dat : Array Nat × Array Nat) : Nat :=
  let (left, right) := dat
  let left := left.qsort (· < ·)
  let right := right.qsort (· < ·)
  (left.zip right).foldl (init := 0) fun s (l, r) => s + (l - r) + (r - l)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := dataToSol <| inputToArrays dat

#assert part1 test == 11

solve 1 2742123 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := Id.run do
  let (ls, rs) := inputToArrays dat
  -- merge the `rs` into a single `HashMap` collecting multiplicities
  let mut hm : Std.HashMap Nat Nat := {}
  for r in rs do
    if let some val := hm.get? r then hm := hm.insert r (val + 1) else hm := hm.insert r 1
  -- accumulate the similarity scores
  let tots := ls.map fun l => l * hm.getD l 0
  tots.sum

#assert part2 test == 31

solve 2 21328497 file

end Day01
