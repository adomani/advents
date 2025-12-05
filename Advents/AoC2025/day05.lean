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

instance : ToString State where
  toString := fun {ranges := rs, ids := ids} =>
    s!"Ranges:\n{rs.toArray.qsort}\n\nIDs:\n{ids.toArray.qsort}"

def inputToState (dat : Array String) : State :=
  dat.foldl (init := {ranges := ∅, ids := ∅}) fun tot s => match s.getNats with
    | [id] => {tot with ids := tot.ids.insert id}
    | [a, b] => {tot with ranges := tot.ranges.insert (a, b)}
    | _ => if s.isEmpty then tot else panic s!"Invalid input line {s}!"

def isFresh (st : State) (id : Nat) : Bool :=
  st.ranges.any fun (a, b) => a ≤ id && id ≤ b

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let st := inputToState dat
  dbg_trace st
  dbg_trace ""
  let freshes := st.ids.fold (init := #[]) fun (tot : Array Nat) n =>
    if isFresh st n then tot.push n else tot
  dbg_trace freshes.size
  dbg_trace freshes

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--set_option trace.profiler true in solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day05
