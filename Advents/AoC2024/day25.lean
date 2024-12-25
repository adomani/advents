import Advents.Utils
open Lean

namespace Day25

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day25.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
Assumes that `s` represents a puzzle input.
Returns the tally of how many characters `c` appear from the start of each column.

For locks, we use `c = '#'`.
For keys, we use `c = '.'` and then take the complement to 5 of the corresponding counts.
-/
def toCounts (s : String) (c : Char) : Array Nat :=
  let ss := Array.transposeString (s.splitOn "\n" ).toArray
  ss.foldl (init := ∅) fun ct s => ct.push <| (s.takeWhile (· == c)).length - 1

/-- The check to verify if a lock and a key fit together. -/
partial
def le (a b : Array Nat) : Bool :=
  if a.isEmpty then true else if b.isEmpty then true else
    a.back! + b.back! ≤ 5 && le a.pop b.pop

/-- Converts the input to the pair consists of all the locks and all the keys. -/
def inputToLocksAndKeys (s : String) : Std.HashSet (Array Nat) × Std.HashSet (Array Nat) :=
  let parts := s.splitOn "\n\n"
  let (l, k) :=
  parts.foldl (init := (∅, ∅)) fun (ls, ks) st =>
    let c := st.get 0
    let ct := toCounts st c
    if c == '#' then
      (ls.insert ct, ks)
    else
      (ls, ks.insert (ct.foldl (·.push <| 5 - ·) #[]))
  (l, k)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let (locks, keys) := inputToLocksAndKeys dat
  let mut tot := 0
  for l in locks do
    for k in keys do
      if le l k then tot := tot + 1
  return tot

#assert part1 test == 3

set_option trace.profiler true in solve 1 3483 file

end Day25
