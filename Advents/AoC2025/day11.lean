import Advents.Utils
open Std

namespace AoC2025_Day11

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day11" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def inputToMap (dat : Array String) : HashMap String (Array String) :=
  dat.foldl (init := ∅) fun tot s =>
    if let [src, tgts] := s.splitOn ": " then
      tot.insert src (tgts.splitOn " ").toArray
    else
      tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let mut mp := inputToMap dat
  let mut cts : HashMap String Nat := {("you", 1)}
  let mut con := 0
  let mut total := 0
  while (!cts.isEmpty) do
    cts := cts.fold (init := ∅) fun tot src val =>
      if let some tgts := mp.get? src then
        tgts.foldl (init := tot) fun intot tgt =>
          intot.alter tgt (some <| ·.getD 0 + val)
      else
        tot
    total := total + cts["out"]?.getD 0
    con := con + 1
  total


#assert part1 atest == 5

set_option trace.profiler true in solve 1 714

/-!
#  Question 2
-/

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut mp := inputToMap dat
  --dbg_trace mp.toArray
  let mut cts : HashMap String Nat := {("you", 1)}
  let mut con := 0
  let mut total := 0
  while (!cts.isEmpty) do
    cts := cts.fold (init := ∅) fun tot src val =>
      if let some tgts := mp.get? src then
        tgts.foldl (init := tot) fun intot tgt =>
          intot.alter tgt (some <| ·.getD 0 + val)
      else
        tot
    total := total + cts["out"]?.getD 0
    dbg_trace "Step {con}: {total}"--": {cts.toArray}"
    con := con + 1
  dbg_trace "{total}"

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day11
