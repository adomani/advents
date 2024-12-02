import Advents.Utils
open Lean

namespace Day09

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day09.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "2199943210
3987894921
9856789892
8767896789
9899965678"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
`lowPoints dat` takes as input the array of strings that are the input to the problem.
It computes
* the `HashSet` of the locations of the "low points" -- these are the points on the grid
  that have value less than or equal to all of their neighbours *and* actually strictly
  smaller than some neighbour (useful for part 2);
* the sum of the risk levels (useful for part 1).
-/
def lowPoints (dat : Array String) : Std.HashSet (Nat × Nat) × Nat := Id.run do
  let mut heights := 0
  let mut lowPoints : Std.HashSet (Nat × Nat) := {}
  for d in [0:dat.size] do
    let prev := dat[d-1]!
    let next := dat[d+1]?.getD dat[d]!
    let row := dat[d]!
    for c in [0:row.length] do
      let curr := row.get ⟨c⟩
      let rownext := if c == row.length - 1 then curr else row.get ⟨c+1⟩
      let rowprev := if c == 0              then curr else row.get ⟨c-1⟩
      if (curr ≤ rownext && curr ≤ rowprev && curr ≤ prev.get ⟨c⟩ && curr ≤ next.get ⟨c⟩) &&
        ! (curr == rownext && curr == rowprev && curr == prev.get ⟨c⟩ && curr == next.get ⟨c⟩)
      then
        lowPoints := lowPoints.insert (d, c)
        heights := heights + 1 + ("".push curr).toNat!
  (lowPoints, heights)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := (lowPoints dat).2

#assert part1 atest == 15

solve 1 603

/-!
#  Question 2
-/

/--
`ExpandingDomain` is a structure to help "growing" from a "low point" to a whole basin.
It represents a potentially intermediate situation, where
* `visited` are the positions that have already been verified to be inside the basin and
* `front` which are the positions that were last added to `visited`.

Cf `growOnce` and `grow`: at each iteration, from each entry in `front`,
they scan the neighbours and expand `visited`
-/
structure ExpandingDomain where
  visited : Std.HashSet (Nat × Nat)
  front : Std.HashSet (Nat × Nat)
  deriving Inhabited

/-- Expands an `ExpandingDomain` once, using the reference `grid`. -/
def growOnce (domain : ExpandingDomain) (grid : Array String) : ExpandingDomain := Id.run do
  let mut frt := {}
  let mut vis := domain.visited
  for p in domain.front do
    let row := grid[p.1]!
    let mut nbs := #[]
    if p.1 != 0 then nbs := nbs.push (p.1 - 1, p.2)
    if p.1 != grid.size -1 then nbs := nbs.push (p.1 + 1, p.2)
    if p.2 != 0 then nbs := nbs.push (p.1, p.2 - 1)
    if p.2 != row.length -1 then nbs := nbs.push (p.1, p.2 + 1)
    for q@(x, y) in nbs do
      if vis.contains q then continue
      if grid[x]!.get ⟨y⟩ == '9' then continue
      vis := vis.insert q
      frt := frt.insert q
  return { visited := vis, front := frt }

/-- Expands an `ExpandingDomain` until it no longer grows. -/
partial
def grow (domain : ExpandingDomain) (grid : Array String) : ExpandingDomain :=
  let vis := domain.visited
  let new := growOnce domain grid
  if new.visited == vis then new else grow new grid

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let (roots, _) := lowPoints dat
  let mut basins := #[]
  for r in roots do
    let seed := {r}
    let gr := grow {visited := seed, front := seed} dat
    let vis := gr.visited
    basins := basins.push vis.size
  let sortedBasins := basins.qsort (· > ·)
  (sortedBasins.take 3).prod

#assert part2 atest == 1134

solve 2 786780

end Day09
