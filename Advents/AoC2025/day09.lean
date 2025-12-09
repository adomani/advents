import Advents.Utils
open Std

namespace AoC2025_Day09

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day09" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def inputToPos (dat : Array String) : HashSet pos :=
  dat.foldl (init := ∅) fun tot s => match s.getNats with | [x, y] => tot.insert (x, y) | _ => tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let gr := inputToPos dat
  let mut maxArea := 0
  let mut left := gr
  for A@(a, b) in gr do
    left := left.erase A
    for (c, d) in left do
      let newArea := ((a - c).natAbs + 1) * ((b - d).natAbs + 1)
      maxArea := max maxArea newArea
  return maxArea


#assert part1 atest == 50

set_option trace.profiler true in solve 1 4767418746

/-!
#  Question 2
-/

def inputToArray (dat : Array String) : Array pos :=
  dat.foldl (init := #[]) fun tot s => match s.getNats with | [x, y] => tot.push (x, y) | _ => tot

-- Check that the input is a snake
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let gr := inputToArray dat
  let switch := if dat.size ≤ 100 then 0 else 1
  for i in [:gr.size] do
    let (a, b) := gr[i]!
    let (c, d) := gr[i + 1]?.getD gr[0]!
    if i % 2 == switch && b != d then dbg_trace "Consecutive second coordinates do not match!"
    if i % 2 != switch && a != c then dbg_trace "Consecutive first coordinates do not match!"
  let (xs, ys) := gr.unzip
  if (HashSet.ofArray xs).size * 2 != dat.size then dbg_trace "Coincidence among first coordinates!"
  if (HashSet.ofArray ys).size * 2 != dat.size then dbg_trace "Coincidence among second coordinates!"

#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let gr := inputToArray dat
  --let (mx, Mx) : Option Int × Option Int := gr.fold (init := (none, none)) fun (mx, Mx) ((a, _) : pos) =>
  --  match mx, Mx with
  --  | some m, some M => (min a m, max a M)
  --  | none, some M => (a, max a M)
  --  | some m, none => (min a m, a)
  --  | none, none => (a, a)

  --let (my, My) : Option Int × Option Int := gr.fold (init := (none, none)) fun (my, My) ((_, b) : pos) =>
  --  match my, My with
  --  | some m, some M => (min b m, max b M)
  --  | none, some M => (b, max b M)
  --  | some m, none => (min b m, b)
  --  | none, none => (b, b)

  --let (mx, MY) : Option Int × Option Int := gr.fold (init := (none, none)) fun (mx, MY) ((a, b) : pos) =>
  --  match mx, MY with
  --  | some m, some M => (min a m, max b M)
  --  | none, some M => (a, max b M)
  --  | some m, none => (min a m, b)
  --  | none, none => (a, b)
  --let (mx, Mx) := (mx.getD 0, Mx.getD 0)
  --let (my, My) := (my.getD 0, My.getD 0)
  --let shifted : HashSet pos := gr.fold (init := ∅) fun tot (nx, ny) => tot.insert ((ny, nx) - (my, mx) + (1, 1))
  --dbg_trace (shifted.size, [mx, Mx, my, My])
  --dbg_trace shifted.toArray
  let mut areas : HashSet Nat := ∅
  let mut maxArea := 0
  let mut left := gr
  let switch := if dat.size ≤ 100 then 0 else 1
  for i in [:gr.size] do
    let (a, b) := gr[i]!
    let (c, d) := gr[i + 1]?.getD gr[0]!
    if i % 2 == switch && b != d then dbg_trace "Fail!"
    if i % 2 != switch && a != c then dbg_trace "Fail!"
    --if i % 2 != switch && a != c then dbg_trace "Fail!"
    for (c, d) in left do
      let newArea := ((a - c).natAbs + 1) * ((b - d).natAbs + 1)
      maxArea := max maxArea newArea
      areas := areas.insert newArea

  for A@(a, b) in gr do
    left := left.erase A
    for (c, d) in left do
      let newArea := ((a - c).natAbs + 1) * ((b - d).natAbs + 1)
      maxArea := max maxArea newArea
      areas := areas.insert newArea
  dbg_trace maxArea
  --dbg_trace (maxArea, areas.toArray.qsort)
  --draw <| drawSparse shifted ((My - my + 1).toNat + 2) ((Mx - mx + 1).toNat + 2)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day09
