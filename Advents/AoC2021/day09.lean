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

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  --let mut lows := 0
  let mut heights := 0
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
        heights := heights + 1 + ("".push curr).toNat!
  IO.println heights

def lowPoints (dat : Array String) : Std.HashSet (Nat × Nat) := Id.run do
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
  lowPoints



/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let mut heights := 0
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
        heights := heights + 1 + ("".push curr).toNat!
  heights

#assert part1 atest == 15

solve 1 603

/-!
#  Question 2
-/

structure ExpandingDomain where
  visited : Std.HashSet (Nat × Nat)
  front : Std.HashSet (Nat × Nat)
  deriving Inhabited

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

partial
def grow (domain : ExpandingDomain) (grid : Array String) : ExpandingDomain := Id.run do
  let mut vis := domain.visited
  let new := growOnce domain grid
  if new.visited == vis then new else grow new grid

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let roots := lowPoints dat
  let mut basins := #[]
  for r in roots do
    let seed := {r}
    let gr := grow {visited := seed, front := seed} dat
    let vis := gr.visited
    --let picture := toPic (vis.toArray.map (fun a : Nat × Nat => (a.1, a.2))) 100 100
    basins := basins.push vis.size
    --dbg_trace s!"{vis.size}"
    --draw picture
  let sortedBasins := basins.qsort (· > ·)
  IO.println <| sortedBasins
  IO.println <| (sortedBasins.take 3).prod
  --let _ ← draws picture
  --IO.println <| draw <| toPic (gr.visited.toArray.map (fun a => (a.1, a.2))) 10 10

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let roots := lowPoints dat
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
