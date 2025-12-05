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

structure State where
  ranges : HashSet (Nat × Nat)
  ids : HashSet Nat
  deriving Inhabited

instance [BEq α] [Hashable α] [ToString α] [LT α] [DecidableRel (LT.lt (α := α))] :
    ToString (HashSet α) where
  toString as := s!"{as.toArray.qsort (· < ·)}"

instance : ToString State where
  toString := fun {ranges := rs, ids := ids} => s!"Ranges:\n{rs}\n\nIDs:\n{ids}"

def inputToState (dat : Array String) : State :=
  dat.foldl (init := {ranges := ∅, ids := ∅}) fun tot s => match s.getNats with
    | [id] => {tot with ids := tot.ids.insert id}
    | [a, b] => {tot with ranges := tot.ranges.insert (a, b)}
    | _ => if s.isEmpty then tot else panic s!"Invalid input line {s}!"

def isFresh (st : State) (id : Nat) : Bool :=
  st.ranges.any fun (a, b) => a ≤ id && id ≤ b

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let st := inputToState dat
  st.ids.fold (init := 0) fun tot n => if isFresh st n then tot + 1 else tot

#assert part1 atest == 3

solve 1 563

/-!
#  Question 2
-/

def overlap (a b : Nat × Nat) : Bool :=
  (a.1 ≤ b.1 && b.1 ≤ a.2) || (a.1 ≤ b.2 && b.2 ≤ a.2) ||
  (b.1 ≤ a.1 && a.1 ≤ b.2) || (b.1 ≤ a.2 && a.2 ≤ b.2)

def mergeOverlaps (rs : HashSet (Nat × Nat)) : Nat × Nat :=
  let (sa, sb) := rs.fold (init := (none, none)) fun
    | (some m, some M), (a, b) => (min m a, max M b)
    | (some m, none), (a, b) => (min m a, b)
    | (none, some M), (a, b) => (a, max M b)
    | (none, none), (a, b) => (a, b)
  (sa.getD 0, sb.getD 0)

def addOneRange (rs : HashSet (Nat × Nat)) (new : Nat × Nat) : HashSet (Nat × Nat) :=
  let (overlaps, outside) := rs.partition (overlap · new)
  let merged := mergeOverlaps (overlaps.insert new)
  --dbg_trace "adding {new}"
  --dbg_trace "outside {outside}"
  --dbg_trace "overlaps {overlaps}"
  --dbg_trace "merged {merged}"
  let final := if merged == (0, 0) then outside.insert new else outside.insert merged
  --dbg_trace "final {final}\n"
  final

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let st := inputToState dat
  --dbg_trace st.ranges
  --dbg_trace ""
  let ranges := st.ranges.fold (init := ∅) fun (tot : HashSet (Nat × Nat)) new => addOneRange tot new
  dbg_trace ranges.fold (init := 0) fun tot (a, b) => (tot + b - a + 1)
  dbg_trace ranges
  --let freshes := st.ids.fold (init := #[]) fun (tot : Array Nat) n =>
  --  if isFresh st n then tot.push n else tot
  --dbg_trace freshes.size
  --dbg_trace freshes

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day05
