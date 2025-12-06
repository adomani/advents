import Advents.Utils
open Std

namespace AoC2025_Day06

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day06" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  "

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def stringToOp (s : String) : List (Nat → Nat → Nat) :=
  go s.toList
  where go : List Char → List (Nat → Nat → Nat)
    | '*'::rs => (· * ·) :: go rs
    | '+'::rs => (· + ·) :: go rs
    | ' '::rs => go rs
    | c::rs => panic s!"'{c}' is not an operation!" :: go rs
    | [] => []

def inputToArrays (dat : Array String) : Array (Array Nat) × Array (Nat → Nat → Nat) :=
  let ops := dat.back!
  ((dat.pop.map (List.toArray ·.getNats)), (stringToOp ops).toArray)

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let (nums, ops) := inputToArrays dat
  dbg_trace nums
  let adds := nums.pop.foldl (init := nums.back!) fun tot ns => (tot : Array Nat).zipWith (· + ·) ns
  let muls := nums.pop.foldl (init := nums.back!) fun tot ns => (tot : Array Nat).zipWith (· * ·) ns
  let tots := ((Array.range ops.size).filterMap fun i => if (ops[i]! : Nat → Nat → Nat) 1 1 == 2 then some adds[i]! else some muls[i]!)
  dbg_trace tots.sum

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

end AoC2025_Day06
