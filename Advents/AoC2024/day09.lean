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
  tot : Array (Nat × Option Nat)

def mkDiskMap (i : String) : DiskMap where
  posAndIDs :=
    let (_, _, pids) : Nat × Nat × _ := i.trim.toList.foldl (init := (0, 0, {}))
      fun (sz, free?, ps) c =>
        let lth := ("".push c).toNat!
        let sz' := sz + lth
        let blocks : ff := {
          pos := sz
          id := if free? % 2 == 1 then none else some (free? / 2)
          length := lth
        }
        (sz', free? + 1, ps.insertIfNew sz' blocks)
    pids
  s := 1
  t := i.trim.length - 1
  tot := #[]

instance : ToString DiskMap where
  toString dm := String.intercalate "\n" <|
    ((dm.posAndIDs.toArray.qsort (·.1 < ·.1)).toList.map
      fun d => s!"start: {d.1}, length: {d.2.1}, ID: {d.2.2}") ++
    ["", s!"(s, t) = {(dm.s, dm.t)}", s!"{dm.tot}"]

def collapse (dm : DiskMap) : DiskMap := Id.run do
  let mut mp := dm.posAndIDs
  let mut tot := dm.tot
  --let mut new := dm
  let mut s := dm.s
  let mut t := dm.t
  match dm.posAndIDs.get? dm.s, dm.posAndIDs.get? dm.t with
    | none, _ | _, none | some (_, some _), _ | _, some (_, none) =>
      panic s!"{dm.s} and {dm.t} should be in range!"
    | some (lFree, none), some (lFile, some id) =>
      let diff := (lFree - lFile) + (lFile - lFree)
      let min := min lFree lFile
      tot := tot.push (id, min)
      if lFree < lFile then
        mp := (mp.erase s).insert t (diff, some id)
        s := s + 2
        if let some (p, some id) := mp.get? (s + 1) then
          tot := tot.push (id, p)
        -- insert the possibly skipped "`t`"-entry `s + 1`
      if lFile < lFree then
        mp := (mp.insert s (diff, none)).erase t
        t := t - 2
      if lFile == lFree then
        mp := (mp.erase s).erase t
        s := s + 2
        t := t - 2
        if let some (p, some id) := mp.get? (s + 1) then
          tot := tot.push (id, p)
  return {dm with posAndIDs := mp, s := s, t := t, tot := tot}


#eval do
  let dat ← IO.FS.readFile input
  let dat := test
  let dat := "12345"
  let DM := mkDiskMap dat
  IO.println DM --.posAndIDs.toArray
  IO.println (collapse DM) --.posAndIDs.toArray

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day09
