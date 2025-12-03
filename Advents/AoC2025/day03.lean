import Advents.Utils
open Std

namespace AoC2025_Day03

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day03" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "987654321111111
811111111111119
234234234234278
818181911112111"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def inputToDigits (s : String) : List Nat :=
  s.toList.map fun c => ("".push c).toNat!

def getMaxs (dat : List Nat) : Nat × Nat :=
  let m1 := dat.max?.getD 0
  let i1 := dat.findIdx (· == m1)
  if i1 < dat.length - 1 then
    let l2 := dat.drop (i1 + 1)
    (m1, l2.max?.getD 0)
  else
  let dat2 := dat.erase m1
  let m2 := dat2.max?.getD 0
  let i2 := dat2.findIdx (· == m2)
  if i1 ≤ i2 then
    (m1, m2)
  else
    (m2, m1)

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let digs := dat.map inputToDigits
  let maxs := digs.map getMaxs
  dbg_trace maxs.foldl (init := 0) fun tot (a, b) => tot + 10 * a + b

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

end AoC2025_Day03
