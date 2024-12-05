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

/-- `ChitonState` is the state maintained while finding the minimum sum values of weighted distances
along the grid.

* `grid` is the data of the risk level at each position.
* `dists` are the weighted distances between the top-left corner and any location of the map
  *as currently computed by the process*.
* `crawls` are the positions of the bots that are crawling through the `grid`,
  accumulating their perceived distance from the start position.
-/
structure ChitonState where
  /-- `grid` is the data of the risk level at each position. -/
  grid   : Std.HashMap pos Nat
  /-- `dists` are the weighted distances between the top-left corner and any location of the map
  *as currently computed by the process*. -/
  dists  : Std.HashMap pos Nat
  /-- `crawls` are the positions of the bots that are crawling through the `grid`,
  accumulating their perceived distance from the start position. -/
  crawls : Std.HashMap pos Nat

/-- One step of the crawlers: each crawler moves in each available direction, compares its computed value
with the value that could already be stored in `dists`.
If there is no value in `dists`, then it adds its own value and adds itself to the list of crawlers for the
next round.
If there is a higher value in `dists`, then it replaces with its own value and again adds itself to the
future crawlers.
If there is a recorded value and it is not bigger than the value of the crawler, then the crawler does nothing,
including *not* adding itself to the crawlers of the following round.
-/
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

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let mut c : ChitonState := {grid := loadGridNats dat, dists := {}, crawls := {((0, 0), 0)}}
  while !c.crawls.isEmpty do
    c := crawlOnce c
  c.dists.get! (dat.size - 1, dat.size - 1)

#assert part1 atest == 40

solve 1 717

/-!
#  Question 2
-/

/-- For the second part of the puzzle, the grid is rescaled to be 5 times larger and the entries
slightly modified: the necessary adjustments are performed by `newGrid`. -/
def newGrid (g : Std.HashMap pos Nat) (sz : Nat) : Std.HashMap pos Nat := Id.run do
  let mut newG := g
  let mut dist := 0
  for i in [0:5] do
    for j in [0:5] do
      if (i, j) == (0, 0) then continue
      dist := i + j
      for (p, v) in g do
        let newP := p + ((sz * i, sz * j) : pos)
        newG := newG.insert newP ((v + (i + j - 1)) % 9 + 1)
  return newG

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let grid := loadGridNats dat
  let grid := newGrid grid dat.size
  let mut c : ChitonState := {grid := grid, dists := {}, crawls := {((0, 0), 0)}}
  let mut i := 0
  while !c.crawls.isEmpty do
    i := i + 1
    c := crawlOnce c
  c.dists.get! (5 * dat.size - 1, 5 * dat.size - 1)

#assert part2 atest == 315

solve 2 2993

end Day15
