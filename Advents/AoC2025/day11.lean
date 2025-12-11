import Advents.Utils
open Std

namespace AoC2025_Day11

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day11" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test1` is the test string for the problem. -/
def test1 := "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"

/-- `test2` is the second test string for the problem. -/
def test2 := "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out"

/-- `atest1` is the test string for the problem, split into rows. -/
def atest1 := (test1.splitOn "\n").toArray

/-- `atest2` is the second test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/--
Converts the input to a `HashMap` assigning to each node, the array of nodes reachable
from the first entry.
-/
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


#assert part1 atest1 == 5

set_option trace.profiler true in solve 1 714

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let mp := inputToMap dat
  let mut cts : HashMap _ _ := {("svr", (1, 0, 0, 0))}
  let mut total := 0
  while (!cts.isEmpty) do
    cts := cts.fold (init := ∅) fun tot src (val, dac?, fft?, both) =>
      if let some tgts := mp.get? src then
        tgts.foldl (init := tot) fun intot tgt =>
          intot.alter tgt fun x =>
            let (ct, isDac, isFft, isBoth) := x.getD (0, 0, 0, 0)
            if tgt == "dac"
            then
              some (0, ct + isDac + val + dac?, 0, isFft + both + fft? + isBoth)
            else if tgt == "fft"
            then
              (0, 0, isFft + val + fft?, isBoth + both + dac? + isDac)
            else
              (ct + val, isDac + dac?, isFft + fft?, isBoth + both)
      else
        tot
    total := total + if let some (_, _, _, t) := cts["out"]? then t else 0
  return total

#assert part2 atest2 == 2

solve 2 333852915427200

end AoC2025_Day11
