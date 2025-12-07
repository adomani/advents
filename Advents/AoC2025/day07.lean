import Advents.Utils
open Std

namespace AoC2025_Day07

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day07" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def inputToPos (dat : Array String) : HashSet pos :=
  sparseGrid dat (· == '^')

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  --let sp := inputToPos dat
  --draw <| drawSparse sp dat[0]!.length dat.size
  let spos := dat[0]!.length - (dat[0]!.dropWhile (· != 'S')).length
  dbg_trace "'S' is at position {spos}"
  let mut tacs : HashSet Nat := {spos}
  let mut splits := 0
  for d in dat do
    --dbg_trace tacs.toArray.qsort
    let inds := sparseGrid (d.toList.toArray.map ("".push)) (· == '^')
    if false && !inds.isEmpty then dbg_trace
      s!"{tacs.toArray.qsort}\n{(inds.fold (·.push ·.1 : Array Int → Int × Int → Array Int) #[]).qsort} ← splitters"
    (tacs, splits) := (Array.range dat[0]!.length).foldl (init := (∅, splits)) fun (tot, ss) n =>
      if tacs.contains n then
        if inds.contains (n, 0) then

          (tot.insertMany [n - 1, n + 1], ss + 1)
        else (tot.insert n, ss)
      else (tot, ss)
  dbg_trace "\n{splits} {tacs.size}"
  dbg_trace "\n{tacs.toArray.qsort}"

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

end AoC2025_Day07
