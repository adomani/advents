import Advents.Utils
open Lean

namespace Day15

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day15.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def loadMap (dat : Array String) : Std.HashMap pos Nat := Id.run do
  let mut h := {}
  for d in [0:dat.size] do
    let row := dat[d]!
    for c in [0:row.length] do
      h := h.insert (d, c) <| String.toNat! ⟨[row.get ⟨c⟩]⟩
  return h

/-- A function to draw the answer to the second part of the puzzle. -/
def drawHash (h : Std.HashMap pos Nat) (Nx Ny : Nat) : Array String := Id.run do
  let mut fin := #[]
  for i in [0:Nx] do
    let mut str := ""
    for j in [0:Ny] do
      match h.get? (i, j) with
        | some d => str := str ++ s!"{d}"
        | none => str := str.push ' '
    fin := fin.push str
  return fin

structure ChitonState where
  grid   : Std.HashMap pos Nat
  dists  : Std.HashMap pos Nat
  crawls : Std.HashMap pos Nat

def crawlOnce (c : ChitonState) : ChitonState := Id.run do
  let mut ds := c.dists
  let mut cs := {}
  for (p, val) in c.crawls do
    for v in #[(1, 0), (-1, 0), (0, 1), (0, - 1)] do
      let q := p + v
      match (c.grid.get? q) with
        | none => continue -- positions outside the grid
        | some wt => -- here we are inside the grid
          let newCand := val + wt -- accumulated expected value for the new location
          match ds.get? q with
            | none => -- if there was no value at this location, add the computed one
              cs :=cs.insert q newCand
              ds := ds.insert q newCand
            | some curr => -- otherwise, we compare the values and keep the new one only if it is smaller
              if newCand < curr then
                cs := cs.insert q newCand
                ds := ds.insert q newCand
              else
                continue
  return {grid := c.grid, dists := ds, crawls := cs}

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut c : ChitonState := {grid := loadMap dat, dists := {}, crawls := {((0, 0), 0)}}
  --draws <| drawHash c.crawls 100 100
  let mut i := 0
  while !c.crawls.isEmpty do
    i := i + 1
    c := crawlOnce c
    --if i % 10 == 0 then
    --  IO.println s!"\n{i+1} -- dists:"
    --  draws <| drawHash c.dists 100 100
    --IO.println "crawls:"
    --draws <| drawHash c.crawls 10 10
  IO.println s!"{i} steps: {c.dists.get! (dat.size - 1, dat.size - 1)}"

structure Path where
  past : Array pos
  val  : Nat
  excl : Array pos
  deriving BEq, Hashable, Inhabited

def extendPath (mz : Std.HashMap pos Nat) (p : Path) : Std.HashSet Path := Id.run do
  let mut h := {}
  let mut cmin := 46
  let nbs := #[(1, 0), (-1, 0), (0, 1), (0, -1)].map (· + p.past.back!) |>.filter fun d =>
    (mz.contains d && (!p.excl.contains d))
  for n in nbs do
    let bef := (p.excl ++ nbs).size
    let hs : Std.HashSet pos := .ofArray <| p.excl ++ nbs
    if bef != hs.size then
      dbg_trace bef - hs.size
    let npth := {past := p.past.push n, val := p.val + mz.getD n 0, excl := hs.toArray}
    if cmin ≤ npth.val + (9 - n.1) + (9 - n.2) then continue
    if npth.val + (9 - n.1) + (9 - n.2) ≤ cmin then
      h := h.insert npth
      if n == (9, 9) then
        cmin := min cmin npth.val
        dbg_trace "found {npth.val} cmin={cmin} {npth.past}"
      --h := h.insert npth
  return h

def extendPaths (mz : Std.HashMap pos Nat) (ps : Std.HashSet Path) : Std.HashSet Path :=
  ps.fold (init := {}) fun acc p => acc.union (extendPath mz p)

def extendMany (mz : Std.HashMap pos Nat) (ps : Std.HashSet Path) : Nat → Std.HashSet Path
  | 0 => ps
  | n + 1 => extendMany mz (extendPaths mz ps) n
set_option trace.profiler true
#eval do
  let mz := loadMap atest
  let p : Path := {past := #[(0,0)], val := 0, excl := #[(0,0)]}
  let l3 := extendMany mz {p} 20
  dbg_trace (l3.size, (l3.filter fun d : Path => d.val ≤ 41 && d.past.back! == (9, 9)).size)
  --for d in l3 do
  --  IO.println d.val
#eval 1+1+6+3+7+5+1+7+4+2+2+8+9+1+7+1+9+1+1

structure Maze where
  maze : Std.HashMap pos Nat
  min  : Option Nat
  paths : Std.HashSet Path
  --completed : Std.HashSet (Array pos)

#eval do
  draws <| drawHash (loadMap atest) 10 10


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

end Day15
