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

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let spos := dat[0]!.length - (dat[0]!.dropWhile (· != 'S')).length
  let mut (tacs, splits) : HashSet Nat × Nat:= ({spos}, 0)
  for d in dat do
    let inds := sparseGrid (d.toList.toArray.map ("".push)) (· == '^')
    if inds.isEmpty then continue
    (tacs, splits) := (Array.range dat[0]!.length).foldl (init := (∅, splits))
      fun (tot, ss) n =>
        if tacs.contains n then
          if inds.contains (n, 0) then
            (tot.insertMany [n - 1, n + 1], ss + 1)
          else (tot.insert n, ss)
        else (tot, ss)
  splits

#assert part1 atest == 21

solve 1 1553

/-!
#  Question 2
-/

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  --let sp := inputToPos dat
  --draw <| drawSparse sp dat[0]!.length dat.size
  let spos := dat[0]!.length - (dat[0]!.dropWhile (· != 'S')).length
  dbg_trace "'S' is at position {spos}"
  let mut tacs : HashSet Nat := {spos}
  let mut pathsTo := List.replicate dat[0]!.length 0
  pathsTo := pathsTo.modify spos (· + 1)
  dbg_trace pathsTo
  let mut splits := 1
  for d in dat do
    --dbg_trace tacs.toArray.qsort
    let inds := sparseGrid (d.toList.toArray.map ("".push)) (· == '^')
    if inds.isEmpty then continue
    let (newtacs, newpathsTo) := (Array.range dat[0]!.length).foldl (init := (∅, List.replicate dat[0]!.length 0))
      fun ((tot : HashSet Nat), (ss : List Nat)) n =>
        if tacs.contains n then
          if inds.contains (n, 0) then
            (tot.insertMany [n - 1, n + 1], (ss.modify (n - 1) (· + pathsTo[n]!)).modify (n + 1) (· + pathsTo[n]!))
          else (tot.insert n, ss.modify n (· + pathsTo[n]!))
        else (tot, ss)
    if true then dbg_trace
      s!"{(tacs.toArray.qsort)} {(splits, newpathsTo)}\n{(inds.fold (·.push ·.1 : Array Int → Int × Int → Array Int) #[]).qsort} ← splitters"
    tacs := newtacs
    pathsTo := newpathsTo
  dbg_trace "\n{pathsTo.sum} {pathsTo}\n{tacs.size}"
  --dbg_trace "\n{tacs.size}"
  dbg_trace "\n{tacs.toArray.qsort}"

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let spos := dat[0]!.length - (dat[0]!.dropWhile (· != 'S')).length
  let p0 := List.replicate dat[0]!.length 0
  let mut pathsTo := p0
  pathsTo := pathsTo.modify spos (· + 1)
  for d in dat do
    let inds := sparseGrid (d.toList.toArray.map ("".push)) (· == '^')
    if inds.isEmpty then continue
    pathsTo := (Array.range dat[0]!.length).foldl (init := p0)
      fun ss (n : Nat) =>
        if pathsTo[n]! != 0 then
          if inds.contains (n, 0) then
            (ss.modify (n - 1) (· + pathsTo[n]!)).modify (n + 1) (· + pathsTo[n]!)
          else ss.modify n (· + pathsTo[n]!)
        else ss
  pathsTo.sum

#assert part2 atest == 40

solve 2 15811946526915

end AoC2025_Day07
