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
  if digs % 2 == 1 then none else
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

def splitEvery (l : List α) (n : Nat) : List (List α) :=
  if n = 0 then [l] else
  if l.length ≤ n then [l] else
  l.take n :: splitEvery (l.drop n) n

def replaceWithMultLower (lth a : Nat) : Option (Nat × Nat) := do
  if (Nat.toDigits 10 a).length % lth != 0 then none else
  let first::rest := (splitEvery (Nat.toDigits 10 a) lth).map (String.toNat! ∘ String.ofList) | failure
  let mult := ((List.range (rest.length + 1)).map fun i => (10 ^ (lth * i))).sum
  --dbg_trace (first::rest, mult)
  if rest.all (· ≤ first) then
    return (first, mult)
  else
    return (first + 1, mult)

/--
Splits the input natural number `a` into consecutive subsequences of `lth` digits each.
It returns
* the first subsequence, if its value is smaller than all the remaining extracted subsequences;
* the first subsequence decreased by `1`, otherwise.

The second output natural number is the largest number less than or equal to `a`
whose digits are `0` and `1`, and where `1` only occupies the positions divisible
by `lth`.
-/
def replaceWithMultUpper (lth a : Nat) : Option (Nat × Nat) := do
  if (Nat.toDigits 10 a).length % lth != 0 then none else
  let first::rest := (splitEvery (Nat.toDigits 10 a) lth).map (String.toNat! ∘ String.ofList) | failure
  let mult := ((List.range (rest.length + 1)).map fun i => (10 ^ (lth * i))).sum
  --dbg_trace (first::rest, mult)
  if rest.all (first ≤ ·) then
    return (first, mult)
  else
    return (first - 1, mult)

#assert
  (replaceWithMultLower 2 123456, replaceWithMultUpper 2 123456) ==
    (some (13, 10101), some (12, 10101))

#assert
  (replaceWithMultLower 3 123456, replaceWithMultUpper 3 123456) ==
    (some (124, 1001), some (123, 1001))

#assert
  (replaceWithMultLower 2 121212, replaceWithMultUpper 2 121212) ==
    (some (12, 10101), some (12, 10101))

--def rangesWithSize (lth a b : Nat) : Nat :=
--  let a := if

def next999 (a : Nat) : Nat :=
  10 ^ (Nat.toDigits 10 a).length - 1

#eval next999 1234 -- 9999

/-
* 2: `11-22` still has `two` invalid IDs,                   `11` and `22`.
* 2: `95-115` now has `two` invalid IDs,                   `99` and `111`.
* 2: `998-1012` now has `two` invalid IDs,               `999` and `1010`.
* 1: `1188511880-1188511890` still has `one` invalid ID,     `1188511885`.
* 1: `222220-222224` still has `one` invalid ID,                 `222222`.
* 0: `1698522-1698528` still contains no invalid IDs.
* 1: `446443-446449` still has `one` invalid ID,                 `446446`.
* 1: `38593856-38593862` still has `one` invalid ID,           `38593859`.
* 1: `565653-565659` now has `one` invalid ID,                   `565656`.
* 1: `824824821-824824827` now has `one` invalid ID,          `824824824`.
* 1: `2121212118-2121212124` now has `one` invalid ID,       `2121212121`.
-/

def processTwo (a b : Nat) (verbose? : Bool := false) :
    Array ((Nat × Nat) × Nat) := Id.run do
  let mut fin := #[]
  for i in [1:(Nat.toDigits 10 b).length] do
    match replaceWithMultLower i a, replaceWithMultUpper i b with
    -- neither the number of digits of `a` nor of `b` are multiples of `i`
    | none, none => continue
    | some (a, mult), none =>
      let bPair := (replaceWithMultUpper i (next999 a)).get!
      if verbose? then dbg_trace "{i}: {a} {bPair} -- {mult}"
      fin := fin.push ((a, bPair.1), mult)
    | none, some (b, mult) =>
      let aPair := (replaceWithMultLower i (1 + next999 a)).get!
      if verbose? then dbg_trace "{i}: {aPair} {b} -- {mult}"
      fin := fin.push ((aPair.1, b), mult)
    | some (a, multa), some (b, multb) =>
      if b < a then
        continue --dbg_trace "{i}: none ({a}, {b}) -- {multa} {multb}"
      else
        if verbose? then dbg_trace "{i}: {(a, b)} -- {multa} {multb}"
        fin := fin.push ((a, b), if multa != multb then panic! s!"{#[multa, multb]}" else multa)
  return fin

def mkReps (h : Array ((Nat × Nat) × Nat)) : Std.HashSet Nat :=
  h.foldl (init := ∅) fun acc ((a, b), mult) =>
    acc.insertMany ((List.range (b - a + 1)).map (fun x => (a + x) * mult))

def mkRepsArray (h : Array ((Nat × Nat) × Nat)) : Array Nat :=
  h.foldl (init := #[]) fun acc ((a, b), mult) =>
    acc ++ ((List.range (b - a + 1)).map (fun x => (a + x) * mult))

#eval do
  let v? := false
  let dat := test
  let dat ← IO.FS.readFile input
  let pairs := inputToRanges dat
  let mut tot := 0
  let mut allPrs := #[]
  for (a, b) in pairs do
  --let sums := pairs.map fun ((a, b) : Nat × Nat) => Id.run do
    let mid := next999 a
    let processed :=
      if b ≤ mid then processTwo a b v? else processTwo a mid v? ++ processTwo (1 + mid) b v?
    dbg_trace "---\n* {(a, b)}\n"
    if processed.isEmpty then dbg_trace "empty"; continue
    dbg_trace "{"\n".intercalate (processed.map (s!"{·}")).toList}"
    dbg_trace ""
    tot := tot + (processed.map fun (((a, b), _arr) : (Nat × Nat) × Nat) => b - a + 1).sum
    allPrs := allPrs ++ processed
  dbg_trace tot
  dbg_trace mkRepsArray allPrs

  --dbg_trace (mkReps allPrs).toArray.qsort
  ----dbg_trace allPrs
  --dbg_trace (mkReps allPrs).toArray.sum

-- 18980589997 too low
/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := Id.run do
  let pairs := inputToRanges dat
  let mut tot := 0
  let mut allPrs := #[]
  for (a, b) in pairs do
  --let sums := pairs.map fun ((a, b) : Nat × Nat) => Id.run do
    let mid := next999 a
    let processed :=
      if b ≤ mid then processTwo a b else processTwo a mid ++ processTwo (1 + mid) b
    --dbg_trace "---\n* {(a, b)}\n"
    if processed.isEmpty then
      --dbg_trace "empty"
      continue
    --dbg_trace "{"\n".intercalate (processed.map (s!"{·}")).toList}"
    --dbg_trace ""
    tot := tot + (processed.map fun (((a, b), _arr) : (Nat × Nat) × Nat) => b - a + 1).sum
    allPrs := allPrs ++ processed
  --dbg_trace tot
  --dbg_trace mkRepsArray allPrs

  --dbg_trace (mkReps allPrs).toArray.qsort
  ----dbg_trace allPrs
  (mkReps allPrs).toArray.sum

#assert part2 test == 4174379265

set_option trace.profiler true in solve 2 0 file

end AoC2025_Day02
