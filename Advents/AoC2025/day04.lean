import Advents.Utils
open Std

namespace AoC2025_Day04

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day04" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

instance : Add (Int × Int) where
  add := fun (a, b) (c, d) => (a + c, b + d)

def neighs (h : HashSet pos) (p : pos) : HashSet pos := Id.run do
  let mut fin := ∅
  for ns in [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)] do
    let new := p + ns
    if h.contains new then
      fin := fin.insert new
  return fin

def accessible (h : HashSet pos) : HashSet pos :=
  h.filter fun p => (neighs h p).size < 4

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let gr := sparseGrid dat (· == '@')
  let le4 := accessible gr
  le4.size

#assert part1 atest == 13

solve 1 1409

/-!
#  Question 2
-/

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let gr := sparseGrid dat (· == '@')
  let mut old := gr
  let mut new := old.filter fun p => 4 ≤ (neighs old p).size
  let mut rems := old.size - new.size
  IO.println (rems, old.size - new.size)
  let mut con := 0
  while old != new do
    old := new
    con := con + 1
    IO.println s!"Step {con}"
    new := old.filter fun p => 4 ≤ (neighs old p).size
    --draw <| drawSparse new dat.size dat[0]!.length
    rems := rems + old.size - new.size
    IO.println (rems, old.size - new.size)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day04
