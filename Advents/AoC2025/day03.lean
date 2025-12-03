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

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let digs := dat.map inputToDigits
  let maxs := digs.map getMaxs
  maxs.foldl (init := 0) fun tot (a, b) => tot + 10 * a + b

#assert part1 atest == 357

solve 1 17100

/-!
#  Question 2
-/

def getMaxBefore (dat : List Nat) (left : Nat) : Nat × List Nat :=
  let cands := (dat.reverse.drop (left - 1)).reverse
  let m1 := cands.max?.getD 0
  let i1 := dat.findIdx (· == m1)
  let l2 := dat.drop (i1 + 1)
  (m1, l2)

partial
def getNMaxs (dat : Array Nat × List Nat) (left : Nat) : Array Nat :=
  let (acc, leftovers) := dat
  if left == 0 then acc else
  let (newMax, newLeftovers) := getMaxBefore leftovers left
  getNMaxs (acc.push newMax, newLeftovers) (left - 1)

def mkNat (as : Array Nat) : Nat :=
  (Array.range as.size).foldl (init := 0) fun tot i => 10 * tot + as[i]!

#assert mkNat #[1,2,3] == 123

#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let digs := dat.map inputToDigits
  --let maxs := digs.map (getMaxBefore · 2)
  let maxs := digs.map fun ds => (getNMaxs (#[], ds) 12)
  dbg_trace (maxs.map mkNat).sum --.foldl (init := 0) fun tot (a, b) => tot + 10 * a + b




/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let digs := dat.map inputToDigits
  --let maxs := digs.map (getMaxBefore · 2)
  let maxs := digs.map fun ds => (getNMaxs (#[], ds) 12)
  (maxs.map mkNat).sum --.foldl (init := 0) fun tot (a, b) => tot + 10 * a + b

#assert part2 atest == 3121910778619

set_option trace.profiler true in solve 2

end AoC2025_Day03
