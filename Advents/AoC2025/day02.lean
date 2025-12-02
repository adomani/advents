import Advents.Utils
open Std

namespace AoC2025_Day02

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day02" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

def inputToRanges (s : String) : Array (Nat × Nat) :=
  let pairsStr := s.splitOn ","
  let pairs := pairsStr.filterMap fun t =>
    match t.getNats with
    | [a, b] => some (a, b)
    | _       => none
  pairs.toArray

def countResidues (res fin : Nat) (mod : Nat) : Nat :=
  let res := res % mod
  let guess := fin / mod
  let correction := if res < (fin % mod) then 1 else 0
  guess + correction

-- Check that the pairs do not span ranges that are too big.
#eval do
  for dat in [test, ← IO.FS.readFile input] do
    let pairs := inputToRanges dat
    let twoOrdersOfMagnitude := pairs.filterMap fun ((a, b) : Nat × Nat) =>
      if (Nat.toDigits 10 a).length + 2 ≤ (Nat.toDigits 10 b).length then some (a, b) else none
    if !twoOrdersOfMagnitude.isEmpty then
      IO.println "No pairs with at least two orders of magnitude difference."

def oneOhOne (a : Nat) := 10 ^ ((Nat.toDigits 10 a).length / 2) + 1

/--
`mkMinMax a fn` writes `a` to base `10` and
* if `a` has an odd number of digits, then it returns `99...9`, where the number of `9`s is
  half the number of digits of `a` rounded down;
* if `a` has an even number of digits, then it splits the digits in half and returns the value
  of `fn` on two halves interpreted as numbers.

This is used only with `fn ∈ {min, max}`.
-/
def mkMinMax (a : Nat) (fn : Nat → Nat → Nat) : Nat :=
  let digs := (Nat.toDigits 10 a).length
  if digs % 2 == 1 then 10 ^ (digs / 2) - 1 else
  let len := digs / 2
  fn (a / 10 ^ len) (a % 10 ^ len)

/--
`mkMax a` writes `a` to base `10` and
* if `a` has an odd number of digits, then it returns `99...9`, where the number of `9`s is
  half the number of digits of `a` rounded down;
* if `a` has an even number of digits, then it splits the digits in half and returns the maximum
  of the two halves interpreted as numbers.
-/
def mkMax (a : Nat) : Nat := mkMinMax a max

#assert
  let as := [1123, 9, 123, 4321]
  as.map mkMax == [23, 0, 9, 43]

/--
`mkMin a` writes `a` to base `10` and
* if `a` has an odd number of digits, then it returns `99...9`, where the number of `9`s is
  half the number of digits of `a` rounded down;
* if `a` has an even number of digits, then it splits the digits in half and returns the minimum
  of the two halves interpreted as numbers.
-/
def mkMin (a : Nat) : Nat := mkMinMax a min

#assert
  let as := [1123, 9, 123, 4321]
  as.map mkMin == [11, 0, 9, 21]

def toBaseAndRange (a b : Nat) : Option (Nat × (Nat × Nat)) :=
  let aDigs := (Nat.toDigits 10 a).length
  let bDigs := (Nat.toDigits 10 b).length
  if aDigs % 2 == 0 then
    some (oneOhOne a, a, if bDigs == aDigs then b else 10 ^ aDigs - 1)
  else
  if bDigs % 2 == 0 then
    some (oneOhOne b, 10 ^ aDigs, b)
  else none

-- #[2, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0]
#eval do
  let dat ← IO.FS.readFile input
  let dat := test
  let pairs := inputToRanges dat
  let twoOrdersOfMagnitude := pairs.map fun ((a, b) : Nat × Nat) =>
    dbg_trace (mkMax a, mkMin b)
    if let some (base, (start, stop)) := toBaseAndRange a b
    then
      countResidues (base - (start % base)) (stop - start + 1) base
    else 0
    --if (Nat.toDigits 10 a).length + 2 ≤ (Nat.toDigits 10 b).length then some (a, b) else none
  dbg_trace twoOrdersOfMagnitude

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--set_option trace.profiler true in solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day02
