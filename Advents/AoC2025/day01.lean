import Advents.Utils
open Std

namespace Day01

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2025/day01.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def parseOneLine (s : String) : Bool × Nat :=
  let dir := s.take 1
  let dist := s.drop 1 |>.toNat!
  match dir with
  | "L" => (true, dist)
  | "R" => (false, dist)
  | _   => panic! "Invalid direction"

def moveOne (start : Int) (h : Bool × Nat) : Int :=
  let (turnLeft, dist) := h
  let newDir := (if turnLeft then start - dist else start + dist) % 100
  newDir

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let start : Int := 50
  let parsed := (dat.map String.trim).map parseOneLine
  let (_, num) : Int × Nat := parsed.foldl (init := (start, 0)) fun (acc : Int × Nat) h =>
    let (currentPos, stepNum) := acc
    let newPos := moveOne currentPos h
    (newPos, stepNum + if newPos == 0 then 1 else 0)
  num

#assert part1 atest == 3

solve 1 1034

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day01
