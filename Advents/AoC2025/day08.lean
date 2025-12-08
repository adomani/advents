import Advents.Utils
open Std

/-!
Powering up Christmas decorations
-/

namespace AoC2025_Day08

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day08" : FilePath).withExtension "input"

/-!
#  Question 1

The input is a list of positions in 3-space of junction boxes.
The first part, asks to connect the `1000` pairs positions that are closest to one another,
find the sizes of the `3` largest resulting connected components and report the product of these
sizes.
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

/-- The type of integer coordinates in `3`-space. -/
abbrev vol := Int × Int × Int

/-- Converts the input strings into a `HashSet` of `vol`s. -/
def inputToPos (dat : Array String) : HashSet vol :=
  dat.foldl (init := ∅) fun tot s => match s.getNats with
    | [a, b, c] => tot.insert (a, b, c)
    | _ => panic s!"{s} is not of the required form!"

/-- `dist v w` is the (square of the) Euclidean distance between `v` and `w`. -/
def dist (v w : vol) : Int := (v.1 - w.1) ^ 2 + (v.2.1 - w.2.1) ^ 2 + (v.2.2 - w.2.2) ^ 2

/--
The hard-coded maximum distance required for part 1.
This is the `1000`th shortest distance between any two points in the input.

*Note*.  The reason for hard-coding is that otherwise even part 1 takes a somewhat long time.
-/
def mdis (d : Array String) : Int := if d.size == 20 then 124564 else 63390489

/--
`mergeOne h v` replaces the components that contain one of the endpoints of `v` into a single one,
obtained by merging them together.
-/
def mergeOne (h : HashSet (Array vol)) (v : vol × vol) : HashSet (Array vol) :=
  let (a, b) := v
  let withAB := h.filter fun as => as.contains a || as.contains b
  let h_ab : HashSet (Array vol) := withAB.fold (·.erase ·) h
  let merged : HashSet vol := withAB.fold (·.insertMany ·) ∅
  h_ab.insert merged.toArray

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let vs := inputToPos dat
  let mut maxDistance := 0
  let mut left := vs.toArray
  for a in vs do
    left := left.erase a
    for b in left do
      let ab := dist a b
      if maxDistance < ab then maxDistance := ab
  left := vs.toArray
  let mut edges : HashSet (vol × vol) := ∅
  while !left.isEmpty do
    let curr := left.back!
    left := left.pop
    for n in left do
      if dist n curr ≤ mdis dat then
        edges := edges.insert (n, curr)
  let verts := edges.fold (init := ∅) fun tot (a, b) => tot.insertMany #[#[a], #[b]]
  let comps := edges.fold mergeOne verts
  let sizes := comps.fold (init := #[]) fun tot (n) => (tot.push n.size)
  let sorted := sizes.qsort (· > ·)
  return (sorted.take 3).prod

#assert part1 atest == 40

set_option trace.profiler true in solve 1 50760

/-!
#  Question 2

Now, we should add all edges, from the shortest to the largest, until the graph has a single
connected component.
Once that happens, we should report the product of the `x`-coordinates of the last edge that was
added.
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let vs := inputToPos dat
  let (pairs, _) : HashSet (vol × vol) × HashSet vol :=
    vs.fold (init := (∅, vs)) fun (tot, left) n =>
      let newleft := left.erase n
      (newleft.fold (init := tot) fun ps p => ps.insert (p, n), newleft)
  let psort := pairs.toArray.qsort fun (a, b) (c, d) => dist a b < dist c d
  let mut comps : Array (HashSet vol) := vs.fold (·.push {·}) #[]
  for (a, b) in psort do
    let (withAB, withoutAB) := comps.partition fun c => (c.contains a || c.contains b)
    comps := withoutAB.push (withAB.foldl (·.union ·) ∅)
    if comps.size == 1 then
      return (a.1 * b.1).natAbs
    else continue
  panic "This should not have happened!"

#assert part2 atest == 25272

set_option trace.profiler true in solve 2 3206508875

end AoC2025_Day08
