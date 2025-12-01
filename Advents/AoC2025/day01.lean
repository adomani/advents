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

solve 1 1048

/-!
#  Question 2
-/

/--
Count the number of naturals congruent to `res` modulo `mod`
there are between `0` (inclusive) and `fin` (exclusive).
-/
def countResidues (res fin : Nat) (mod : Nat := 100) : Nat :=
  let res := res % mod
  let guess := fin / mod
  let correction := if res < (fin % mod) then 1 else 0
  guess + correction

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let start : Int := 50
  let parsed := dat.map parseOneLine
  let (_, num) : Int × Nat := parsed.foldl (init := (start, 0)) fun (currentPos, stepNum) h =>
    let newPos := moveOne currentPos h
    let signedDist := (if h.1 then 1 else - 1) * currentPos
    (newPos, stepNum + countResidues (signedDist % 100).natAbs h.2)
  num

#assert part2 atest == 6

solve 2 6498

end Day01
