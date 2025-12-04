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

def neighs (h : HashSet pos) (p : pos) : HashSet pos := Id.run do
  let mut fin := ∅
  for ns in [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)] do
    let new := p + ns
    if h.contains new then
      fin := fin.insert new
  return fin

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let gr := sparseGrid dat (· == '@')
  let le4 := gr.filter fun p => (neighs gr p).size < 4
  le4.size

#assert part1 atest == 13

solve 1 1409

/-!
#  Question 2
-/

def getNbs (h rem : HashSet pos) : HashSet pos :=
  rem.fold (init := ∅) fun tot p => Id.run do
    let mut here : HashSet pos := ∅
    for n in [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)] do
      let shifted := p + n
      if shifted ∈ h then here := here.insert shifted
    tot.insertMany here

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let gr := sparseGrid dat (· == '@')
  let mut old := gr
  let mut (new, removed) := old.partition fun p => 4 ≤ (neighs old p).size
  let mut nearRemoved := getNbs old removed
  let mut rems := old.size - new.size

  IO.println (rems, old.size - new.size)
  let mut con := 0
  while old != new && con ≤ 55 do
    old := new
    con := con + 1
    IO.println s!"Step {con}"
    --nearRemoved := getNbs old removed
    let mut (new', removed') : HashSet pos × HashSet pos := (old, ∅)
    for p in nearRemoved do
      if (neighs old p).size < 4 then
        new' := new'.erase p
        removed' := removed'.insert p
    (new, removed) := (new', removed') --old.partition fun p => 4 ≤ (neighs old p).size
    nearRemoved := getNbs old removed

    --draw <| drawSparse new dat.size dat[0]!.length
    rems := rems + old.size - new.size
    IO.println (rems, old.size - new.size)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let gr := sparseGrid dat (· == '@')
  let mut old := gr
  let mut (new, removed) := old.partition fun p => 4 ≤ (neighs old p).size
  let mut nearRemoved := getNbs old removed
  let mut rems := old.size - new.size
  while old != new do
    old := new
    let mut (new', removed') : HashSet pos × HashSet pos := (old, ∅)
    for p in nearRemoved do
      if (neighs old p).size < 4 then
        new' := new'.erase p
        removed' := removed'.insert p
    (new, removed) := (new', removed')
    nearRemoved := getNbs old removed
    rems := rems + old.size - new.size
  return rems

#assert part2 atest == 43

set_option trace.profiler true in solve 2 8366

end AoC2025_Day04
