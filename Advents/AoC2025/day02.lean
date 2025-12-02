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

-- Check that the pairs do not span ranges that are too big.
#eval do
  for dat in [test, ← IO.FS.readFile input] do
    let pairs := inputToRanges dat
    let twoOrdersOfMagnitude := pairs.filterMap fun ((a, b) : Nat × Nat) =>
      if (Nat.toDigits 10 a).length + 2 ≤ (Nat.toDigits 10 b).length then some (a, b) else none
    if !twoOrdersOfMagnitude.isEmpty then
      IO.println "No pairs with at least two orders of magnitude difference."

/--
`mkMinMax a fn` writes `a` to base `10` and
* if `a` has an odd number of digits, then it returns `99...9`, where the number of `9`s is
  half the number of digits of `a` rounded down;
* if `a` has an even number of digits, then it splits the digits in half and returns the value
  of `fn` on two halves interpreted as numbers.

This is used only with `fn ∈ {min, max}`.
-/
def mkMinMax (a : Nat) (fn : Nat → Nat → Nat) : Option Nat :=
  let digs := (Nat.toDigits 10 a).length
  if digs % 2 == 1 then none else --10 ^ (digs / 2) - 1 else
  let len := digs / 2
  some (fn (a / 10 ^ len) (a % 10 ^ len))

/--
`mkMax a` writes `a` to base `10` and
* if `a` has an odd number of digits, then it returns `99...9`, where the number of `9`s is
  half the number of digits of `a` rounded down;
* if `a` has an even number of digits, then it splits the digits in half and returns the maximum
  of the two halves interpreted as numbers.
-/
def mkMax (a : Nat) : Nat :=
  (mkMinMax a (fun x y => if x < y then x + 1 else x)).getD <|
    let digs := ((Nat.toDigits 10 a).length) / 2
    10 ^ digs

#assert
  let as := [1123, 9, 123, 4321]
  as.map mkMax == [12, 1, 10, 43]

/--
`mkMin a` writes `a` to base `10` and
* if `a` has an odd number of digits, then it returns `99...9`, where the number of `9`s is
  half the number of digits of `a` rounded down;
* if `a` has an even number of digits, then it splits the digits in half and returns the minimum
  of the two halves interpreted as numbers.
-/
def mkMin (a : Nat) : Nat :=
  (mkMinMax a (fun x y => if x ≤ y then x else x - 1)).getD <|
    let digs := (Nat.toDigits 10 a).length / 2
    10 ^ digs - 1

#assert
  let as := [1123, 9, 123, 4321]
  as.map mkMin == [11, 0, 9, 42]

def countTo (a : Nat) : Nat := a * (a + 1) / 2

def countFromTo (a b : Nat) : Nat :=
  countTo b - countTo (a - 1)

#eval do
  let dat := test
  let dat ← IO.FS.readFile input
  let pairs := inputToRanges dat
  let sums := pairs.map fun ((a, b) : Nat × Nat) =>
    let small := mkMax a
    let smallDouble := 10 ^ ((Nat.toDigits 10 a).length / 2) + 1
    let large := mkMin b
    let largeDouble := 10 ^ ((Nat.toDigits 10 b).length / 2) + 1
    let mult := if (Nat.toDigits 10 a).length % 2 == 1 then
      largeDouble
    else
      smallDouble
    dbg_trace "{(small, large)} count: {countFromTo small large}"
    dbg_trace "a: {(a, small, smallDouble)}"
    dbg_trace "b: {(b, large, largeDouble)}"
    dbg_trace "{(small ≤ large : Bool)}, mult: {mult}, tot: {mult * countFromTo small large}"
    dbg_trace ""
    if small ≤ large then
      mult * countFromTo small large
    else 0
  dbg_trace "{(sums, sums.sum)}"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let pairs := inputToRanges dat
  let sums := pairs.map fun (a, b) =>
    let (small, large) := (mkMax a, mkMin b)
    let smallDouble := 10 ^ ((Nat.toDigits 10 a).length / 2) + 1
    let largeDouble := 10 ^ ((Nat.toDigits 10 b).length / 2) + 1
    let mult := if (Nat.toDigits 10 a).length % 2 == 1 then largeDouble else smallDouble
    if small ≤ large then mult * countFromTo small large else 0
  sums.sum

#assert part1 test == 1227775554

set_option trace.profiler true in solve 1 18952700150 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day02
