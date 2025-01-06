import Advents.Utils
open Std

namespace Day12

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day12.input"

/-!
#  Question 1
-/

/-- `test1` is the test string for the problem. -/
def test1 := "AAAA
BBCD
BBCC
EEEC"

/-- `atest1` is the test string for the problem, split into rows. -/
def atest1 := (test1.splitOn "\n").toArray

/-- `test2` is the test string for the problem. -/
def test2 := "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"

/-- `atest2` is the test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/-- `test3` is the test string for the problem. -/
def test3 := "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

/-- `atest3` is the test string for the problem, split into rows. -/
def atest3 := (test3.splitOn "\n").toArray

/-- `testEX` is the test string for the problem. -/
def testEX := "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"

/-- `atestEX` is the test string for the problem, split into rows. -/
def atestEX := (testEX.splitOn "\n").toArray

/-- `testAB` is the test string for the problem. -/
def testAB := "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"

/-- `atestAB` is the test string for the problem, split into rows. -/
def atestAB := (testAB.splitOn "\n").toArray

/--
The main structure to create connected components.
* `grid` is the underlying grid.
* `growing` is the set of vertices that we have so far added to our "connected component".
* `front` is the front of the expansion: at the next step, we add the neighbours of `front` to
  `growing`.
* `edges` is the set of entries of `growing` that have a neighbour not in `grid`.
-/
structure OneComp where
  /-- `grid` is the underlying grid. -/
  grid : HashSet pos
  /-- `growing` is the set of vertices that we have so far added to our "connected component". -/
  growing : HashSet pos
  /-- `front` is the front of the expansion: at the next step, we add the newighbours of `front` to
  `growing`. -/
  front : HashSet pos
  /-- `edges` is the set of entries of `growing` that have a neighbour not in `grid`. -/
  edges : HashMap pos Nat := {}
  deriving Inhabited

/-- `nbs` is the set of neighbours, namely, the 4 coordinate directions. -/
abbrev nbs : HashSet pos := {(1, 0), (- 1, 0), (0, 1), (0, - 1)}

/--
Creates a "live" `OneComp`: this is not yet a connected component, just something that can
`growComp` to be one.
-/
def mkOneComp {α} [BEq α] (g : HashMap pos α) (c : α) (st : pos) : OneComp :=
  let gr : HashSet pos := g.fold (init := {}) fun h p val => if val == c then h.insert p else h
  if gr.contains st then
    {grid := gr, growing := {st}, front := {st}}
  else
    dbg_trace "'{st}' does not belong to the grid!"
    {grid := gr, growing := {st}, front := {st}}

/-- Expands `growing` by each possible neighbour of `front`, updating also `edges` as necessary. -/
def grow (c : OneComp) : OneComp := Id.run do
  let mut newR := c.growing
  let mut edges := c.edges
  let mut newF : HashSet pos := {}
  for f in c.front do
    for p in nbs do
      let newpos := f + p
      if ! c.grid.contains newpos then
        edges := edges.alter f fun v => some <| v.getD 0 + 1
      else
      if !newR.contains newpos then
        newR := newR.insert newpos
        newF := newF.insert newpos
  return {c with growing := newR, front := newF, edges := edges}

/--
Creates the connected component "encoded" by its input,
by growing the set `growing` as far as possible.
-/
def growComp (h : OneComp) : OneComp := Id.run do
  let mut grown := h
  while ! grown.front.isEmpty do
    grown := grow grown
  return grown

/-- The area of the connected component. -/
def area (h : OneComp) : Nat := h.growing.size

/--
Finds the perimeter of a connected component by tallying, for each edge, the directions that
leave the component: these are already stored in the values of the `edges` `HashMap`.
-/
def perimeter (h : OneComp) : Nat :=
  h.edges.fold (fun tot _ v => tot + v) 0

/--
For each character that appears as a value in `tot`, determine the connected components of the
set of positions with that character and return them all as an array,
each with the corresponding character.
-/
def getComponents (tot : HashMap pos Char) : Array (Char × OneComp) := Id.run do
  let mut tot := tot
  let mut fd := #[]
  while !tot.isEmpty do
    let (st, p) := tot.toArray.back!
    let gr := mkOneComp tot p st
    let comp := growComp gr
    tot := tot.filter fun p _ => ! comp.growing.contains p
    fd := fd.push (p, comp)
  return fd

/-- Computes the sum of `area * perimeter` over all the connected components in `tot`. -/
def tallyAll (tot : HashMap pos Char) : Nat :=
  let init := getComponents tot
  init.foldl (fun t (_, h) => t + (h.growing.size * perimeter h)) 0

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := tallyAll <| loadGrid dat id

#assert part1 atest1 == 140
#assert part1 atest2 == 772
#assert part1 atest3 == 1930

--set_option trace.profiler true in solve 1 1483212  -- slow, takes approx 55s

/-!
#  Question 2
-/

/--
Computes the `HashSet`s of the entries of `h` whose first coordinate is `i` and that do not have
a left/right neighbour, storing them separately into the two output `HashSet`s.
-/
def leftRightBounds (h : OneComp) (i : Int) : HashSet Int × HashSet Int :=
  h.edges.fold (fun new@(newl, newr) p _ =>
    if p.1 == i
    then
      match !h.grid.contains (p + (0, - 1)), !h.grid.contains (p + (0, 1)) with
        | true, true => (newl.insert p.2, newr.insert p.2)
        | true, false => (newl.insert p.2, newr)
        | false, true => (newl, newr.insert p.2)
        | false, false => new
    else
      new
    ) default

/--
Computes the corners in the component encoded in `h`, withing the bounds `m` and `M`.

TODO: find the values of `m` and `M` and generally make this more efficient.
-/
def corners (h : OneComp) (m M : Nat) : Nat := Id.run do
  let mut diffs := 0
  let mut (cl, cr) : HashSet Int × HashSet Int := default
  for i in [m:M] do
    let lr@(l, r) := leftRightBounds h i
    diffs := diffs +
      (cl.filter (!l.contains ·)).size + (l.filter (!cl.contains ·)).size +
      (cr.filter (!r.contains ·)).size + (r.filter (!cr.contains ·)).size
    (cl, cr) := lr
  return diffs

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let tot := loadGrid dat id
  (getComponents tot).foldl (fun (_, comp) => · + area comp * corners comp 0 (dat.size + 1)) 0

#assert part2 atest1 == 80
#assert part2 atest2 == 436
#assert part2 atestEX == 236
#assert part2 atestAB == 368
#assert part2 atest3 == 1206

--set_option trace.profiler true in solve 2 897062  -- slow, takes approx 60s

end Day12
