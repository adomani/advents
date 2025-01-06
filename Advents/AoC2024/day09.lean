import Advents.Utils

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

/-- `F`ree or `F`ile: the main structure for encoding information about the blocks of a file. -/
structure ff where
  /-- The location where the entry starts. -/
  pos    : Nat
  /-- `none` for a free block, `some id` for a file with ID `id`. -/
  id     : Option Nat
  /-- How many blocks the entry spans. -/
  length : Nat
  deriving Inhabited

/-- Useful for printing, while working with the solution. -/
instance : ToString ff where
  toString ff := s!"pos: {ff.pos}, length: {ff.length}, id: {ff.id}"

/--
For each disk location, we provide
* the data of a `ff` -- `F`ile or `F`ree,
* the currently left-most free block,
* the currently right-most file block,
* the currently accumulated array of files in their final positions.
-/
structure DiskMap where
  /-- The data of a `ff` -- `F`ile or `F`ree. -/
  posAndIDs : Std.HashMap Nat ff
  /-- The currently left-most free block. -/
  s : Nat
  /-- The currently right-most file block. -/
  t : Nat
  /-- The currently accumulated array of files in their final positions. -/
  tot : Array ff

/-- Converts the input into a valid `DiskMap`, to start (de-)fragmenting. -/
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

/-- Useful for printing, while working with the solution. -/
instance : ToString DiskMap where
  toString dm := String.intercalate "\n" <|
    ((dm.posAndIDs.toArray.qsort (·.1 < ·.1)).toList.map
      fun (d, ff) => s!"seq: {d}, start: {ff.pos}, length: {ff.length}, ID: {ff.id}") ++
    ["", s!"(s, t) = {(dm.s, dm.t)}", "", s!"tot: {dm.tot}", "---"]

/--
Performs one move of a consecutive sequence of blocks in a file to a consecutive block of
available free space.
-/
def collapse (dm : DiskMap) : DiskMap := Id.run do
  let mut mp := dm.posAndIDs
  let mut tot := dm.tot
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

/--
The checksum for the first part.
While it should be the same as `tallyMerged` (the one for the second part),
it assumes a different encoding of the files, so it is actually a different program.
-/
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

/--
The checksum for the second part.
While it should be the same as `tallyTot` (the one for the first part),
it assumes a different encoding of the files, so it is actually a different program.
-/
def tallyMerged (tot : Array ff) : Nat :=
  tot.foldl (init := 0) fun tot ({pos := pos, length := length, id := id}) =>
    tot + id.get! * (length * (pos - 1) + (length * (length + 1)) / 2)

/--
Produces the merging obtained by scanning the files from the right and inserting in the
left-most possible free space that is not later than their current position.
-/
def interleave (a : Array ff) : Array ff := Id.run do
  let mut fin := #[]
  let mut (frees, files) := a.partition (·.id == none)
  while files.back?.isSome do
    let last := files.back!
    files := files.pop
    match frees.findIdx? fun r => (last.length ≤ r.length && r.pos < last.pos) with
      | none => fin := fin.push last
      | some idx =>
        fin := fin.push {last with pos := frees[idx]!.pos}
        frees := frees.modify idx fun s =>
          {s with length := s.length - last.length, pos := s.pos + last.length}
  return fin

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let DM := mkDiskMap dat
  let il := (interleave (DM.posAndIDs.toArray.map Prod.snd)).qsort (·.pos < ·.pos)
  tallyMerged il

#assert part2 test == 2858

--set_option trace.profiler true in solve 2 6469636832766 file  -- takes approximately 1 minute

end Day09
