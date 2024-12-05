import Advents.Utils
open Lean

namespace Day02

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day02.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
Check that the entries of the list are either strictly increasing or strictly decreasing and that never
do they increase or decrease by more than 3.
-/
def isSafe (dat : List Nat) : Bool :=
  let (l, r) := ((List.range (dat.length - 1)).map fun i =>
    let di := dat[i]!
    let di' := dat[i+1]!
    (di < di' && di' ≤ di + 3, di' < di && di ≤ di' + 3)).unzip
  l.all id || r.all id

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  (dat.filter (isSafe ∘ String.getNats)).size

#assert part1 atest == 2

solve 1 534

/-!
#  Question 2
-/

/--
Check that the entries of the list are either strictly increasing or strictly decreasing and that never
do they increase or decrease by more than 3, possibly after dropping one of their entries.
-/
def isSafeish (dat : List Nat) : Bool :=
  isSafe dat || ((List.range dat.length).map (isSafe <| dat.eraseIdx ·)).any id

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  (dat.filter (isSafeish ∘ String.getNats)).size

#assert part2 atest == 4

solve 2 577

end Day02
