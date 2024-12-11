import Advents.Utils
open Lean

namespace Day09

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day09.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "2333133121414131402"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `F`ree or `F`ile -/
structure ff where
  /-- The location where the entry starts. -/
  pos    : Nat
  /-- `none` for a free block, `some id` for a file with ID `id`. -/
  id     : Option Nat
  /-- How many blocks the entry spans. -/
  length : Nat
  deriving Inhabited

instance : ToString ff where
  toString ff := s!"pos: {ff.pos}, length: {ff.length}, id: {ff.id}"

/--
For each disk location, we provide the number of consecutive blocks that it spans and
the corresponding ID.
At each location, the ID is `none` for free space and `some n` for files.
-/
structure DiskMap where
  /-- At each location, the ID is `none` for free space and `some n` for files. -/
  posAndIDs : Std.HashMap Nat ff
  s : Nat
  t : Nat
  tot : Array ff

def mkDiskMap (i : String) : DiskMap where
  posAndIDs :=
    let (_, _, _, pids) : Nat × Nat × Nat × _ := i.trim.toList.foldl (init := (0, 0, 0, {}))
      fun (seq, sz, free?, ps) c =>
        let lth := ("".push c).toNat!
        let sz' := sz + lth
        let blocks : ff := {
          pos := sz
          id := if free? % 2 == 1 then none else some (free? / 2)
          length := lth
        }
        (seq + 1, sz', free? + 1, ps.insertIfNew seq blocks)
    pids
  s := 1
  t := i.trim.length - 1
  tot := #[]

instance : ToString DiskMap where
  toString dm := String.intercalate "\n" <|
    ((dm.posAndIDs.toArray.qsort (·.1 < ·.1)).toList.map
      fun (d, ff) => s!"seq: {d}, start: {ff.pos}, length: {ff.length}, ID: {ff.id}") ++
    ["", s!"(s, t) = {(dm.s, dm.t)}", "", s!"tot: {dm.tot}", "---"]

def collapse (dm : DiskMap) : DiskMap := Id.run do
  let mut mp := dm.posAndIDs
  let mut tot := dm.tot
  --let mut new := dm
  let mut s := dm.s
  let mut t := dm.t
  match dm.posAndIDs.get? dm.s, dm.posAndIDs.get? dm.t with
    | none, _ | _, none | some ({id := some _, ..}), _ | _, some ({id := none, ..}) =>
      panic s!"{dm.s} and {dm.t} should be in range!"
    | some (fs@{length := lFree, id := none, ..}), some (ft@{length := lFile, id := some _, ..}) =>
      if lFree < lFile then
        mp := mp.insert t {ft with length := lFile - lFree}
        tot := tot.push {ft with length := lFree}
        -- insert the possibly skipped "`t`"-entry `s + 1`
        if let some fil := mp.get? (s + 1) then
          tot := tot.push fil
        s := s + 2
      if lFile < lFree then
        mp := mp.insert s {fs with length := lFree - lFile}
        t := t - 2
        tot := tot.push ft
      if lFile == lFree then
        tot := tot.push ft
        -- insert the possibly skipped "`t`"-entry `s + 1`
        if let some fil := mp.get? (s + 1) then
          tot := tot.push fil
        s := s + 2
        t := t - 2
  return {dm with posAndIDs := mp, s := s, t := t, tot := tot}

def tallyTot (tot : Array ff) : Nat :=
  let mid := tot.foldl (init := (0, 0)) fun (pos, tot) ({length := mult, id := id..}) =>
    (pos + mult, tot + id.get! * (mult * (pos - 1) + (mult * (mult + 1)) / 2))
  mid.2

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let mut DM := mkDiskMap dat
  while DM.s < DM.t do
    DM := collapse DM
  tallyTot <| #[DM.posAndIDs.get! 0] ++ DM.tot

#assert part1 test == 1928

solve 1 6435922584968 file

/-!
#  Question 2
-/
--#exit
#check Array.findIdx?
def findS (dm : DiskMap) (lFile : Nat) : Option (Nat × ff) :=
  --let cands := ((dm.posAndIDs.toArray.map Prod.snd).qsort (·.pos < ·.pos))
  --let idx := cands.findIdx? fun ff => ff.id.isNone && lFile ≤ ff.length
  --  --qsort filter fun _seq ff => ff.id.isNone && lFile ≤ ff.length
  --idx.map fun c => (c, cands.get! c)
  --/-
  let cands := dm.posAndIDs.filter fun _seq ff => ff.id.isNone && lFile ≤ ff.length
  --dbg_trace "Found {cands.size} places"
  cands.fold (init := (none : Option (Nat × ff))) fun c seq ff =>
    if let some (oldSeq, _oldMin) := c
    then
      if seq < oldSeq then --|| (ff.pos == oldMin.pos || ff.length < oldMin.length) then
        some (seq, ff)
      else
        c
    else
      some (seq, ff)
  --/

/-
#eval do
  let dat := "12345"
  let dat ← IO.FS.readFile input
  let dat := test
  let mut DM := mkDiskMap dat
  --IO.println DM --.posAndIDs.toArray
  let mut i := 0
  while DM.s < DM.t do --for i in [0:3] do
    i := i + 1
    IO.println s!"DM.t: {DM.t}, ID: {(DM.posAndIDs.get! DM.t).id}\nlth: {(DM.posAndIDs.get! DM.t).length}\nfindS: {findS DM (DM.posAndIDs.get! DM.t).length}\n"
    --IO.println s!"* {i + 1}"
    DM := collapse DM
    IO.println DM
  IO.println <| tallyTot <| (List.replicate ("".push <| dat.get ⟨0⟩).toNat! (1, 0)).toArray ++ DM.tot
  IO.println DM
--/

def collapse2 (dm : DiskMap) : DiskMap := Id.run do
  let mut mp := dm.posAndIDs
  let mut tot := dm.tot
  --let mut new := dm
  let mut t := dm.t
  match dm.posAndIDs.get? dm.t with
    | none | some {id := none, ..} => return dm --panic s!"{dm.t} should be in range!"
    | some {length := lFile, id := some id, ..} =>
      let S? := findS dm lFile
      --dbg_trace "processing {f}"
      if S?.isNone
      then
        --dbg_trace "(lFile, id): {(lFile, id)}\nnot found"
        t := t - 2
      else
        let (s, S) := S?.get!
        --dbg_trace "(lFile, id): {(lFile, id)}\nfound {S}"
        let lFree := S.length
        --let s := mp.get! seq
        if lFile < lFree then
          --dbg_trace "replacing\n  {S}\nwith\n  {{S with length := lFree - lFile}}\nat {s}\n"
          mp := (mp.insert s ({S with length := lFree - lFile, pos := S.pos + lFile})).erase t
          --dbg_trace mp.toArray
          t := t - 2
          tot := tot.push {pos := S.pos, length := lFile, id := id}
        if lFile == lFree then
          --dbg_trace "file id: {id}"
          mp := (mp.erase s).erase t
          tot := tot.push {pos := S.pos, length := lFree, id := id}
          --if let some {length := p, id := some id', ..} := mp.get? (s + 1) then
          --  tot := tot.push (p, id')
          t := t - 2
  return {dm with posAndIDs := mp, t := t, tot := tot}

def tallyMerged (tot : Array ff) : Nat :=
  tot.foldl (init := 0) fun tot ({pos := pos, length := length, id := id}) =>
    tot + id.get! * (length * (pos - 1) + (length * (length + 1)) / 2)

/--
info: tallyMerged merged: 2858
---
warning: unused variable `dat`
note: this linter can be disabled with `set_option linter.unusedVariables false`
-/
#guard_msgs in
#eval do
  let _dat := "12345"
  let dat ← IO.FS.readFile input
  let dat := test
  let mut DM := mkDiskMap dat
  --IO.println DM --.posAndIDs.toArray
  let mut i := 0
  let mut oldT := DM.t + 1
  while DM.t < oldT do --for i in [0:3] do
    i := i + 1
    --IO.println s!"DM.t: {DM.t}, ID: {(DM.posAndIDs.get! DM.t).id}\nlth: {(DM.posAndIDs.get! DM.t).length}\nfindS: {findS DM (DM.posAndIDs.get! DM.t).length}\n"
    oldT := DM.t
    DM := collapse2 DM
    --IO.println s!"* {i + 1}"
    --IO.println DM
  --IO.println <| tallyTot <| (List.replicate ("".push <| dat.get ⟨0⟩).toNat! (1, 0)).toArray ++ DM.tot.map Prod.snd
  let fixed := DM.posAndIDs.filterMap (fun _ (fil : ff) => fil.id.map fun _ => fil)
  let merged := fixed.toArray.map Prod.snd ++ DM.tot
  --IO.println {DM with posAndIDs := DM.posAndIDs.filter fun _ i => i.id.isSome, tot := DM.tot.qsort (·.1 < ·.1)}--.qsort (·.1 < ·.1)}
  --IO.println s!"\nmerged:\n{merged.qsort (·.1 < ·.1)}"
  --IO.println s!"\nmerged:\n{merged.qsort (·.1 < ·.1) |>.map Prod.snd}"
  IO.println s!"tallyMerged merged: {tallyMerged merged}"

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day09
-- 8654184283366 -- too high
