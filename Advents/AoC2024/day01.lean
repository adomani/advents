import Advents.Utils

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

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- Given the input string, extract the arrays of left and right potential locations. -/
def inputToArrays (dat : Array String) : Array Nat × Array Nat :=
  dat.foldl (init := default) fun (left, right) i =>
    match i.getNats with
      | [l, r] => (left.push l, right.push r)
      | _ => panic "Parsing error!"

/--
Given a pair of arrays of natural numbers, sort each, compute the distances of the corresponding
entries of the sorted lists and return the total sum of the distances.
-/
def dataToSol (dat : Array Nat × Array Nat) : Nat :=
  let (left, right) := dat
  let left := left.qsort (· < ·)
  let right := right.qsort (· < ·)
  (left.zip right).foldl (init := 0) fun s (l, r) => s + (l - r : Int).natAbs

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := dataToSol <| inputToArrays dat

#assert part1 atest == 11

solve 1 2742123

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let (ls, rs) := inputToArrays dat
  -- merge the `rs` into a single `HashMap` collecting multiplicities
  let hm : Std.HashMap Nat Nat := rs.foldl (·.alter · (some <| ·.getD 0 + 1)) {}
  -- accumulate the similarity scores
  ls.foldl (fun tot l => tot + l * hm.getD l 0) 0

#assert part2 atest == 31

solve 2 21328497

end Day01
