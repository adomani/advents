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

structure off where
  locks : Std.HashSet (Array Nat)
  keys : Std.HashSet (Array Nat)
  deriving Inhabited

def toCounts (s : String) (c : Char) : Array Nat :=
  let ss := Array.transposeString (s.splitOn "\n" ).toArray
  ss.foldl (init := ∅) fun ct s => ct.push <| (s.takeWhile (· == c)).length - 1

def inputToOff (s : String) : off :=
  let parts := s.splitOn "\n\n"
  let (l, k) :=
  parts.foldl (init := (∅, ∅)) fun (ls, ks) st =>
    let c := st.get 0
    let ct := toCounts st c
    if c == '#' then
      (ls.insert ct, ks)
    else
      (ls, ks.insert (ct.foldl (·.push <| 5 - ·) #[]))
  ⟨l, k⟩

#eval do
  let dat := test
  let off := inputToOff dat
  IO.println off.locks.toArray
  IO.println off.keys.toArray


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

end Day25
