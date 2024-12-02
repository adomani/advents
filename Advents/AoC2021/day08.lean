import Advents.Utils
open Lean

namespace Day08

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day08.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
`digitByLength s` assigns a string to `some d`, if the length of the string uniquely determines the digit `d` among the seven-segment displays.
Otherwise, it returns `none`.
-/
def digitByLength (s : String) : Option Nat :=
  match s.length with
    | 2 => some 1
    | 4 => some 4
    | 3 => some 7
    | 7 => some 8
    | _ => none

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let tails := dat.map fun d : String => (d.splitOn " | ")[1]!
  let numbers := (tails.map fun n : String => n.splitOn " ").toList.flatten
  let easy := numbers.filter (digitByLength · |>.isSome)
  easy.length

#assert part1 atest == 26

solve 1 554

/-!
#  Question 2
-/

/-- Checks whether the string `s` is a substring of `t` *after* sorting both. -/
def isContainedIn (s t : String) : Bool :=
  (s.toList.toArray.qsort (· < ·)).toList.Sublist (t.toList.toArray.qsort (· < ·)).toList

/--
The labeling of the signal patterns, as a function from `[0..9] → String`.

First, we assign the natural numbers that are determined completely by `digitByLength` to their string.
Next, `3` corresponds to the only string left that extends        and has length 2 more than `seven`.
Next, `9` corresponds to the only string left that extends        and has length 1 more than `three`.
Next, `5` corresponds to the only string left that is extended by and has length 1 less than `nine`.
Next, `6` corresponds to the only string left that extends        and has length 1 more than `five`.
Next, `0` corresponds to the only string left that                and has length 6.
Next, `2` corresponds to the only string left.

Blunt, but works!
-/
def label (ns : List String) : Std.HashMap Nat String := Id.run do
  let mut h := {}
  let mut left := #[]
  for n in ns do
    if let some d := digitByLength n then
      h := h.insert d n
    else left := left.push n
  let seven := (h.get? 7).getD ""

  let three := left.filter fun s => s.length == seven.length + 2 && isContainedIn seven s
  let three := three[0]!
  h := h.insert 3 three
  left := left.erase three

  let nine := left.filter fun s => s.length == three.length + 1 && isContainedIn three s
  let nine := nine[0]!
  h := h.insert 9 nine
  left := left.erase nine

  let five := left.filter fun s => s.length == nine.length - 1 && isContainedIn s nine
  let five := five[0]!
  h := h.insert 5 five
  left := left.erase five

  let six := left.filter fun s => s.length == five.length + 1 && isContainedIn five s
  let six := six[0]!
  h := h.insert 6 six
  left := left.erase six

  let zero := left.filter fun s => s.length == 6
  let zero := zero[0]!
  h := h.insert 0 zero
  left := left.erase zero

  h := h.insert 2 left[0]!
  return h

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let mut tot := 0
  for str in dat do
    if let [inp, vals] := (str.splitOn " | ") then
      let dats := inp.splitOn " "
      let vals := vals.splitOn " "
      let labs := label dats
      let mut num := 0
      for v in vals do
        for (d, s) in labs do
          if s.length == v.length && isContainedIn s v then
            num := 10* num + d
      tot := tot + num
  return tot

#assert part2 atest == 61229

solve 2 990964

end Day08
