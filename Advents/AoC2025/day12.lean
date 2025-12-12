import Advents.Utils
open Std

namespace AoC2025_Day12

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day12" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

abbrev present := HashSet pos

def inputToPresent (dat : Array String) : present :=
  sparseGrid dat (· == '#')

instance : ToString present where
  toString p := "\n".intercalate (drawSparse p 3 3).toList

structure state where
  /-- The height of the `region` -/
  h : Nat
  /-- The width of the `region` -/
  w : Nat
  /-- `pres` assigns to an index, the corresponding `present` -/
  pres : HashMap Nat present
  /-- `grs` assigns to each index the number of presents of that shape that still need placing -/
  grs : HashMap Nat Nat
  deriving Inhabited

instance : ToString state where
  toString := fun
    | {h, w, pres, grs} => s!"H&W: {(h, w)}, remaining: {grs.toArray.qsort}"

def inputToState (dat : String) : Array state :=
  let parts := dat.splitOn "\n\n" |>.toArray
  let (prs, sts) := (parts.pop, parts.back!)
  let pres := prs.foldl (init := ∅) fun tot s =>
    let split := s.splitOn "\n"
    tot.insert split.head!.getNats[0]! (sparseGrid split.tail.toArray (· == '#'))
  (sts.splitOn "\n").foldl (init := ∅) fun tot n =>
    let (dims, grsString) := match n.splitOn ": " with
      | [dims, w] => (dims, w)
      | _ => panic s!"'{n}' does not contain a single ': '!"
    let (h, w) := match dims.getNats with
      | [h, w] => (h, w)
      | _ => panic s!"'{dims}' does not contain two nats!"
    let nums := grsString.getNats
    tot.push <|
    { h := h
      w := w
      pres := pres
      grs := HashMap.ofList <| (List.range nums.length).zipWith (·, ·) nums
      }

#eval do
  let dat := test
  let tot := inputToState dat
  let pres := tot.back!.pres
  let lefts : Array (Array (Nat × Nat)) := tot.foldl (init := #[]) fun ts (n : state) => (ts.push n.grs.toArray)
  dbg_trace String.intercalate "\n\n" (lefts.map fun (as : Array (Nat × Nat)) => (s!"{as.map (Prod.snd ·)}")).toList
  dbg_trace String.intercalate "\n\n" ((pres.toArray.qsort (·.1 < ·.1)).map fun ((i, p) : Nat × present) => s!"{i}\n{p}").toList

structure region where
  h : Nat
  w : Nat
  sh : HashSet pos

structure state where
  shapes : HashMap Nat (HashSet pos)
  grs : HashMap Nat region
  deriving Inhabited

def inputToRegion (dat : Array String) : region :=
  let (h, w) := match dat[0]!.getNats with
    | [a, b] => (a, b)
    | _ => panic s!"{dat[0]!} does not consist of two nats!"
  { h := h
    w := w
    sh := sparseGrid (dat.drop 1) (· == '#') }



def inputToState (dat : String) : Array state :=
  let parts := dat.splitOn "\n\n" |>.toArray
  let (rs, grs) := (parts.pop, parts.back!)
  let regions := rs.foldl
  { shapes := default
    }

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

end AoC2025_Day12
