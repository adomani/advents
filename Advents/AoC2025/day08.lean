import Advents.Utils
open Std

namespace AoC2025_Day08

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day08" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

abbrev vol := Int × Int × Int

def inputToPos (dat : Array String) : HashSet vol :=
  dat.foldl (init := ∅) fun tot s => match s.getNats with
    | [a, b, c] => tot.insert (a, b, c)
    | _ => panic s!"{s} is not of the required form!"

def dist (v w : vol) : Int := (v.1 - w.1) ^ 2 + (v.2.1 - w.2.1) ^ 2 + (v.2.2 - w.2.2) ^ 2

def mdis (d : Array String) : Int := if d.size == 20 then 124564 else 63390489

def mergeOne (h : HashSet (Array vol)) (v : vol × vol) : HashSet (Array vol) :=
  let (a, b) := v
  let withAB := h.filter fun as => as.contains a || as.contains b
  let h_ab : HashSet (Array vol) := withAB.fold (·.erase ·) h
  let merged : HashSet vol := withAB.fold (·.insertMany ·) ∅
  h_ab.insert merged.toArray

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let vs := inputToPos dat
  let mut (left) := vs.toArray
  let mut edges : HashSet (vol × vol) := ∅
  while !left.isEmpty do
    let curr := left.back!
    left := left.pop
    for n in left do
      if dist n curr ≤ mdis dat then
        edges := edges.insert (n, curr)
  let verts : HashSet (Array vol) := edges.fold (init := ∅) fun tot (a, b) => tot.insertMany #[#[a], #[b]]
  let comps : HashSet (Array vol) := edges.fold (init := verts) mergeOne
  let sizes : Array Nat := comps.fold (init := #[]) fun tot (n : Array vol) => (tot.push n.size)
  let sorted := sizes.qsort (· > ·)
  (sorted.take 3).prod

#assert part1 atest == 40

set_option trace.profiler true in solve 1 50760

/-!
#  Question 2
-/

#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let close := if dat.size == 20 then 10 else 1000
  let vs := inputToPos dat
  let mut (left, visited) : Array vol × Array vol := (vs.toArray, #[])
  let mut dists : Array Int := #[]
  let mut edges : HashSet (vol × vol) := ∅
  while !left.isEmpty do
    let curr := left.back!
    left := left.pop
    for n in left do
      if dist n curr ≤ mdis dat then
        edges := edges.insert (n, curr)
    --dists := dists ++ left.foldl (init := (#[] : Array Int)) fun (tot : Array Int) n => (tot.push (dist n curr))

  --let dists :=
  let verts : HashSet (Array vol) := edges.fold (init := ∅) fun tot (a, b) => tot.insertMany #[#[a], #[b]]
  let comps : HashSet (Array vol) := edges.fold (init := verts) mergeOne
  let sizes : Array Nat := comps.fold (init := #[]) fun tot (n : Array vol) => (tot.push n.size)
  let sorted := sizes.qsort (· > ·)
  dbg_trace sorted.take 3
  dbg_trace (sorted.take 3).prod
  --dbg_trace edges.toArray
  --dbg_trace dists.qsort.take close
  --dbg_trace ((HashSet.ofArray dists).size, vs.size)
  --dbg_trace (dists.size, vs.size)



/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day08
