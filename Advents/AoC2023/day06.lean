import Advents.Utils
import Batteries.Data.Nat.Basic

namespace Day06

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day06.input"

/-!
#  Question 1

I could not figure out the correct rounding for the answer in part 1.
For this reason, I could not use the solution to part 2 also in part 1.
-/

/-- `test` is the test string for the problem. -/
def test := "Time:      7  15   30
Distance:  9  40  200"

/-- Extract the two lists of natural numbers from the input data. -/
def getData (s : String) : List (List Nat) :=
  (s.splitOn "\n").map String.getNats

/-- The solution to part 1. -/
def part1 (str : String) : Nat :=
  let dat := getData str |>.filter (· ≠ [])
  List.prod <| (List.range dat[0]!.length).map fun i =>
    let tgt := dat[1]![i]!
    let currTime := dat[0]![i]!
    ((List.range currTime).filter fun j => tgt < (currTime - j) * j).length

#assert part1 test == 288

solve 1 1312850 file

/-!
#  Question 2
-/

/-- Extract the pair of natural numbers from the input data. -/
def getData2 (s : String) : Nat × Nat :=
  match (String.mk (s.toList.filter (· != ' '))).getNats with
    | [a, b] => (a, b)
    | _ => dbg_trace "oh no!"; default

/-- The discriminant of the quadratic polynomial: it stands in for the difference of the roots. -/
def discRt (coeffs : Nat × Nat) : Nat :=
  Nat.sqrt (coeffs.1 ^ 2 - 4 * coeffs.2)

/-- The solution to part 2. -/
def part2 (s : String) : Nat := discRt (getData2 s)

#assert part2 test == 71503

solve 2 36749103 file

end Day06
