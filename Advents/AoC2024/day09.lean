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
  tot : Array (Nat × Nat)

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
    ["", s!"(s, t) = {(dm.s, dm.t)}", s!"tot: {dm.tot}", "---"]

def collapse (dm : DiskMap) : DiskMap := Id.run do
  let mut mp := dm.posAndIDs
  let mut tot := dm.tot
  --let mut new := dm
  let mut s := dm.s
  let mut t := dm.t
  match dm.posAndIDs.get? dm.s, dm.posAndIDs.get? dm.t with
    | none, _ | _, none | some ({id := some _, ..}), _ | _, some ({id := none, ..}) =>
      panic s!"{dm.s} and {dm.t} should be in range!"
    | some (fs@{length := lFree, id := none, ..}), some (ft@{length := lFile, id := some id, ..}) =>
      if lFree < lFile then
        mp := (mp.insert s {ft with length := lFree}).insert t {ft with length := lFile - lFree}
        --dbg_trace "here {(lFree, id)}"
        tot := tot.push (lFree, id)
        -- insert the possibly skipped "`t`"-entry `s + 1`
        if let some ({length := p, id := some id', ..}) := mp.get? (s + 1) then
          tot := tot.push (p, id')
        s := s + 2
      if lFile < lFree then
        mp := (mp.insert s {fs with length := lFree - lFile}).erase t
        t := t - 2
        tot := tot.push (lFile, id)
      if lFile == lFree then
        mp := (mp.insert s ft).erase t
        tot := tot.push (lFree, id)
        -- insert the possibly skipped "`t`"-entry `s + 1`
        if let some ({length := p, id := some id', ..}) := mp.get? (s + 1) then
          tot := tot.push (p, id')
        s := s + 2
        t := t - 2
  return {dm with posAndIDs := mp, s := s, t := t, tot := tot}

def tallyTot (tot : Array (Nat × Nat)) : Nat :=
  let rtot := (tot.map fun (m, id) => List.replicate m id).foldl (· ++ ·) []
  let lth := rtot.length
  (Array.range lth).foldl (init := 0) fun arr v =>
    --let (id) := rtot[v]!
    arr + v * rtot[v]!

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let mut DM := mkDiskMap dat
  let mut i := 0
  while DM.s < DM.t do --for i in [0:3] do
    i := i + 1
    DM := collapse DM
  let tot := DM.posAndIDs.toArray.qsort (·.1 < ·.1)
  let retot := tot.filterMap fun (_p, {length := l, id := id..}) => id.map (l, ·)

  dbg_trace DM
  dbg_trace retot
  dbg_trace tallyTot <| (List.replicate ("".push <| dat.get ⟨0⟩).toNat! (1, 0)).toArray ++ retot
  tallyTot <| (List.replicate ("".push <| dat.get ⟨0⟩).toNat! (1, 0)).toArray ++ DM.tot

#assert part1 test == 1928

--solve 1 6435922584968 file

/-!
#  Question 2
-/
--#exit
#eval do
  let dat := "12345"
  let dat ← IO.FS.readFile input
  let dat := test
  let mut DM := mkDiskMap dat
  --IO.println DM --.posAndIDs.toArray
  let mut i := 0
  while DM.s < DM.t do --for i in [0:3] do
    i := i + 1
    --IO.println s!"* {i + 1}"
    DM := collapse DM
    --IO.println DM
  IO.println <| tallyTot <| (List.replicate ("".push <| dat.get ⟨0⟩).toNat! (1, 0)).toArray ++ DM.tot
  IO.println DM


/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day09
