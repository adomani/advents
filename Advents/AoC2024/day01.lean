import Advents.Utils
open Lean

namespace Day01

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day01.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "3   4
4   3
2   5
1   3
3   9
3   3"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def inputToArrays (i : String) : Array Nat × Array Nat :=
  let nats := i.getNats
  Id.run do
  let mut (left, right) : Array Nat × Array Nat := default
  for i in [:nats.length / 2] do
    left := left.push nats[2 * i]!
    right := right.push nats[2 * i + 1]!
  return (left, right)

def dataToSol (dat : Array Nat × Array Nat) : Nat :=
  let (left, right) := dat
  let left := left.qsort (· < ·)
  let right := right.qsort (· < ·)
  let diffs := left.zipWith right fun l r => (l - r) + (r - l)
  diffs.sum

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := dataToSol <| inputToArrays dat

#assert part1 test == 11

solve 1 2742123 file

/-!
#  Question 2
-/

def similarityScore (rs : Array Nat) (l : Nat) : Nat :=
  rs.toList.count l

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let (ls, rs) := inputToArrays dat
  let tots := ls.map fun l => l * similarityScore rs l
  tots.sum

#assert part2 test == 31

solve 2 21328497 file

end Day01
