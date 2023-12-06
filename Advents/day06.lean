import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day06.input"

/-!
#  Question 1

I could not figure out the correct rounding for the answer in part 1.
For this reason, I could not use the solution to part 2 also in part 1.
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "Time:      7  15   30
Distance:  9  40  200"

/-- Extract the two lists of natural numbers from the input data. -/
def getData (s : String) : List (List Nat) :=
  let rows := s.splitOn "\n"
  rows.map (List.getNumbers ∘ String.toList)

/-- The solution to part 1. -/
def part1 (str : String) : Nat :=
  Id.run do
  let dat := getData str |>.filter (· ≠ [])
  let mut cands := #[]
  for i in [:dat[0]!.length] do
    let mut cand := #[]
    let tgt := dat[1]![i]!
    let currTime := dat[0]![i]!
    for j in [:currTime] do
      let tot := (currTime - j) * j
      if tgt < tot then
        cand := cand.push j
    cands := cands.push cand.size
  return cands.prod

--#assert part1 test == 288

#eval show MetaM _ from do
  let answer := part1 (← IO.FS.readFile input )
  IO.println f!"Day 6, part 1: {answer}"
  guard (answer == 1312850)

/-!
#  Question 2
-/

/-- Extract the pair of natural numbers from the input data. -/
def getData2 (s : String) : Nat × Nat :=
  match (s.toList.filter (· != ' ')).getNumbers with
    | [a, b] => (a, b)
    | _ => dbg_trace "oh no!"; default

/-- The discriminant of the quadratic polynomial: it stands in for the difference of the roots. -/
def discRt (coeffs : Nat × Nat) : Nat :=
  Nat.sqrt (coeffs.1 ^ 2 - 4 * coeffs.2)

--#assert discRt (getData2 <| test) == 71503

#eval show MetaM _ from do
  let answer := discRt <| getData2 <| ← IO.FS.readFile input
  IO.println <| f!"Day 6, part 2: {answer}"
  guard (answer == 36749103)
