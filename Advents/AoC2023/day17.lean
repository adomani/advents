import Advents.Utils
open Std

namespace Day17

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day17.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure crux where
  grid : HashMap pos Nat
  front : HashMap pos (Nat × Array pos)
  visited : HashMap pos (Nat × (Array (Array pos)))

def inputToCrux (dat : Array String) : crux where
  grid := loadGridNats dat
  front := {((0, 0), (0, #[]))}
  visited := {((0, 0), (0, #[]))}

def numToBold : Nat → String
  | 1 => "𝟙"
  | 2 => "𝟚"
  | 3 => "𝟛"
  | 4 => "𝟜"
  | 5 => "𝟝"
  | 6 => "𝟞"
  | 7 => "𝟟"
  | 8 => "𝟠"
  | 9 => "𝟡"
  | _ => ""

def drawCrux (c : crux) : IO Unit := do
  let (mx, my) := c.grid.fold (init := (0, 0))
    fun (mx, my) (nx, ny) _ => (max mx nx.natAbs, max my ny.natAbs)
  let regrid : HashMap pos String := c.grid.fold (init := ∅) fun h p v =>
    h.insert p (if c.visited.contains p then numToBold v else s!"{v}")
  draw <| drawHash regrid (mx + 1) (my + 1)

#eval do
  let dat := atest
  let c := inputToCrux dat
  let regrid : HashMap pos String := c.grid.fold (init := ∅) fun h p v =>
    h.insert p (if c.visited.contains p then numToBold v else s!"{v}")
  draw <| drawHash regrid dat.size dat.size

def updatePast {α} (ps : Array α) (a : α) (n : Nat := 3) : Array α :=
  --(if ps.size < n then ps else ps.eraseIdx 0).push a
  ps.push a

def rotL (p : pos) : pos := (- p.2,   p.1)
def rotR (p : pos) : pos := (  p.2, - p.1)

def availableSteps (ps : Array pos) : Array pos :=
  match ps.back? with
    | none => #[(0, 1), (1, 0)]
    | some last =>
      let lr := #[rotL last, rotR last]
      if ps.size < 3
      then lr.push last else
      let plast := #[ps.pop.pop.back!, ps.pop.back!, ps.back!]
      if (HashSet.ofArray plast).size != 1 then lr.push last else lr

#eval do
  let mut ps := #[]
  for i in [:10] do
    IO.println ps
    ps := updatePast ps i

def step (c : crux) (exit : pos) : crux := Id.run do
  let currMax := c.visited[exit]?.map Prod.fst
  let mut front := ∅
  let mut visited := c.visited
  for (p, cost, past) in c.front do
    let avSteps := availableSteps past
    --dbg_trace "{p}\n{past} <-- past\n{avSteps} <-- avSteps\n{avSteps.filter (c.grid.contains <|p + ·)} <-- actual\n"
    for d in avSteps do
      let cand := p + d
      if let some val := c.grid[cand]? then
        let newCost := cost + val
        --let t := if cand == exit then dbg_trace newCost; true else true
        --if t then
        if currMax.getD newCost < newCost then
          --dbg_trace "eliminating {newCost}"
          continue
        let newPast := updatePast past d 45
        --front := front.insert cand (newCost, newPast)
        front := front.insert cand (newCost, newPast)
        match visited[cand]? with
          | none =>
            --front := front.insert cand (newCost, newPast)
            visited := visited.insert cand (newCost, #[newPast])
          | some (oldCost, pasts) =>
            visited := visited.insert cand (min oldCost newCost, pasts.push newPast)
            --if --newCost ≤ oldCost +27 ||
            --  !pasts.contains newPast then
            --front := front.insert cand (newCost, newPast)
            --else
            --if !pasts.contains newPast then
            --  front := front.insert cand (newCost, newPast)

  return {c with front := front, visited := visited}

set_option trace.profiler true in
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let mut c := inputToCrux dat
  let exit : pos := c.grid.fold (init := (0, 0))
    fun (mx, my) ((nx, ny) : pos) _ => (max mx nx.natAbs, max my ny.natAbs)
  --drawCrux {c with visited := {(exit, default)}}
  let mut i := 0
  while !c.front.isEmpty do
    i := i + 1
    c := step c exit
    --IO.println c.front.size
    if let some (val, _) := c.visited[exit]? then IO.println s!"Step {i}, cost: {val}"
    if i % 9 == 0 then
      --IO.println s!"Step {i+1}"
      drawCrux {c with visited := c.front.fold (init := ∅) fun (h : HashMap _ _) p _n => h.insert p default}
  IO.println <| s!"Step {i}: there are {c.front.size} open fronts"

  IO.println <| (c.front.filter (fun p _ => p == exit)).toArray
  let vis := c.visited[exit]!
  --let pth := vis.2[1]!
  for pth in vis.2 do
    let news : List (pos × (Nat × Array (Array pos))) := pth.foldl (init := [((0, 0), default)]) fun h n =>
      h ++ [((h.getLast!.1 + n), default)]
    c := {c with visited := .ofList news}
    dbg_trace "news.length: {news.length}"
    drawCrux c


/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry
-- 1407  -- ??
-- 1413  -- not correct
-- 1443  -- not correct
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

end Day17
