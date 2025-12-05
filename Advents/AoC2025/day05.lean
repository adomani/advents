import Advents.Utils
open Std

namespace AoC2025_Day05

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day05" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "3-5
10-14
16-20
12-18

1
5
8
11
17
32"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
The `Database` stores in `HashSet`s
* the `ranges` of the fresh ingredient IDs as pairs of natural numbers;
* the `ids` of the ingredients as natural numbers.
-/
structure Database where
  ranges : HashSet (Nat × Nat)
  ids : HashSet Nat
  deriving Inhabited

/-- A convenience instance to display `HashSet`s. -/
local instance [BEq α] [Hashable α] [ToString α] [LT α] [DecidableRel (α := α) (· < ·)] :
    ToString (HashSet α) where
  toString as := s!"{as.toArray.qsort (· < ·)}"

/-- A convenience instance to display `Database`s. -/
local instance : ToString Database where
  toString := fun {ranges := rs, ids := ids} => s!"Ranges:\n{rs}\n\nIDs:\n{ids}"

/-- Converts the input data into a `Database`. -/
def inputToDatabase (dat : Array String) : Database :=
  dat.foldl (init := {ranges := ∅, ids := ∅}) fun tot s => match s.getNats with
    | [id] => {tot with ids := tot.ids.insert id}
    | [a, b] => {tot with ranges := tot.ranges.insert (a, b)}
    | _ => if s.isEmpty then tot else panic s!"Invalid input line {s}!"

/-- Check whether `id` is fresh, that is it lies in at least one of the ranges of `st`. -/
def isFresh (st : HashSet (Nat × Nat)) (id : Nat) : Bool :=
  st.any fun (a, b) => a ≤ id && id ≤ b

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let st := inputToDatabase dat
  st.ids.fold (init := 0) fun tot n => if isFresh st.ranges n then tot + 1 else tot

#assert part1 atest == 3

solve 1 563

/-!
#  Question 2
-/

/--
Check is two ranges `a` and `b` overlap, by checking if one of the endpoints of either is contained
in the range of the other.
-/
def overlap (a b : Nat × Nat) : Bool :=
  (a.1 ≤ b.1 && b.1 ≤ a.2) || (a.1 ≤ b.2 && b.2 ≤ a.2) ||
  (b.1 ≤ a.1 && a.1 ≤ b.2) || (b.1 ≤ a.2 && a.2 ≤ b.2)

/--
Returns the smallest first coordinate and the largest second coordinate in `rs`.

It exploits that `max` on `Option Nat` considers `none` to be smaller than `some 0`.

Panics if `rs` is empty.
-/
def mergeOverlaps (rs : HashSet (Nat × Nat)) : Nat × Nat :=
  let (sa, sb) := rs.fold (init := (none, none)) fun
    | (some m, M), (a, b) => (min m a, max M b)
    | (none, M), (a, b) => (a, max M b)
  (sa.get!, sb.get!)

def addOneRange (rs : HashSet (Nat × Nat)) (new : Nat × Nat) : HashSet (Nat × Nat) :=
  let (overlaps, outside) := rs.partition (overlap · new)
  let merged := mergeOverlaps (overlaps.insert new)
  if merged == (0, 0) then outside.insert new else outside.insert merged

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let st := inputToDatabase dat
  let ranges := st.ranges.fold (init := ∅) fun (tot : HashSet (Nat × Nat)) new => addOneRange tot new
  ranges.fold (init := 0) fun tot (a, b) => (tot + b - a + 1)

#assert part2 atest == 14

solve 2 338693411431456

end AoC2025_Day05
