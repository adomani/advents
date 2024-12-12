import Advents.Utils
open Lean

namespace Day12

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day12.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

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

structure OneComp where
  gr : Std.HashSet pos
  growing : Std.HashSet pos
  front : Std.HashSet pos
  edges : Std.HashSet pos := {}
  nbs : Std.HashSet pos := {(1, 0), (- 1, 0), (0, 1), (0, - 1)}
  deriving Inhabited

/-
instance : ToString OneComp where
  toString g :=
    drawSparse g.growing
-/

def mkOneComp {α} [BEq α] (g : Std.HashMap pos α) (c : α) (st : pos) : OneComp :=
  let oneVar := g.filter (fun _pos val => val == c)
  let gr : Std.HashSet pos := oneVar.fold (init := {}) fun h p _ => h.insert p
  if gr.contains st then
    {gr := gr, growing := {st}, front := {st}}
  else
    dbg_trace "'{st}' does not belong to the grid!"
    {gr := gr, growing := {st}, front := {st}}

def grow (c : OneComp) : OneComp := Id.run do
  let mut newR := c.growing
  let mut edges := c.edges
  let mut newF : Std.HashSet pos := {}
  for f in c.front do
    for p in c.nbs do
      let newpos := f + p
      if ! c.gr.contains newpos then
        edges := edges.insert f
      else
      if !newR.contains newpos then
        newR := newR.insert newpos
        newF := newF.insert newpos
  return {c with growing := newR, front := newF, edges := edges}

def growComp (h : OneComp) : OneComp := Id.run do
  let mut grown := h
  while ! grown.front.isEmpty do
    grown := grow grown
  return grown

def connectedComponent (h : OneComp) : Std.HashSet pos :=
  growComp h |>.growing

def area (h : OneComp) : Nat :=
  h.growing.size

def perimeter (h : OneComp) : Nat := Id.run do
  let mut tot := 0
  for e in h.edges do
    for n in h.nbs do
      if !h.growing.contains (e + n) then
        tot := tot + 1
  return tot

/-
 12
 4
 14
 10
 13
 11
 1
 13
 14
 5
 3
-/

/--
info:
Start (1, 1), value: R, area : 12
Start (6, 4), value: I, area : 14
Start (2, 6), value: C, area : 14
Start (0, 8), value: F, area : 10
Start (4, 0), value: V, area : 13
Start (6, 6), value: J, area : 11
Start (2, 6), value: C, area : 14
Start (6, 9), value: E, area : 13
Start (6, 4), value: I, area : 14
Start (8, 0), value: M, area : 5
Start (9, 4), value: S, area : 3
-/
#guard_msgs in
#eval do
  let dat := atest3
  let tot := loadGrid dat id
  let plants := #['R', 'I', 'C', 'F', 'V', 'J', 'C', 'E', 'I', 'M', 'S']
  for p in plants do
    let gr := sparseGrid dat (· == p)
    let st := gr.toArray.back!
    let gr := mkOneComp tot p st
    let comp := connectedComponent gr
    IO.println s!"Start {st}, value: {p}, area : {comp.size}"

#eval do
  let dat := atest3
  let tot := loadGrid dat id
  let plants := #['R', 'I', 'C', 'F', 'V', 'J', 'C', 'E', 'I', 'M', 'S']
  for p in plants do
    let gr := sparseGrid dat (· == p)
    let st := gr.toArray.back!
    let gr := mkOneComp tot p st
    let compInfo := growComp gr
    let perim := perimeter compInfo
    let area := area compInfo
    IO.println s!"Start {st}, value: {p}, area * perimeter: {area} * {perim} = {area * perim}"

/-
Start (7, 3), value: I, area : 14
Start (9, 2), value: M, area : 5
Start (3, 4), value: C, area : 14
Start (3, 8), value: F, area : 10
Start (5, 3), value: V, area : 13
Start (9, 7), value: E, area : 13
Start (0, 4), value: I, area : 4
Start (1, 3), value: R, area : 12
Start (8, 4), value: S, area : 3
Start (6, 7), value: J, area : 11
Start (4, 7), value: C, area : 1
-/
def getComponents (tot : Std.HashMap pos Char) : Array (Char × OneComp) := Id.run do
  let mut tot := tot
  let mut fd := #[]
  while !tot.isEmpty do
    let (st, p) := tot.toArray.back!
    let gr := mkOneComp tot p st
    let comp := growComp gr
    tot := tot.filter fun p _ => ! comp.growing.contains p
    fd := fd.push (p, comp)
  return fd

def tallyAll (tot : Std.HashMap pos Char) : Nat :=
  let init := getComponents tot
  init.foldl (fun t (_, h) => t + (h.growing.size * perimeter h)) 0 --· + ·.2.size)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := tallyAll <| loadGrid dat id

#assert part1 atest1 == 140
#assert part1 atest2 == 772
#assert part1 atest3 == 1930

--solve 1 1483212  -- slow, takes approx 80s

/-!
#  Question 2
-/

def rot (p : pos) : pos := (p.2, - p.1)

def breaks (h : OneComp) : Nat := Id.run do
  let mut tot := 0
  for e in h.edges do
    for n in h.nbs do
      if (!h.growing.contains (e + n)) && ((!h.growing.contains ((e + n + rot n))) || (!h.growing.contains ((e + n - rot n)))) then
        tot := tot + 1
  return tot

def breakAll (tot : Std.HashMap pos Char) : Nat :=
  let init := getComponents tot
  init.foldl (fun t (_, h) => t + (h.growing.size * breaks h)) 0 --· + ·.2.size)

--set_option trace.profiler true in
#eval do
  let dat ← IO.FS.lines input
  let dat := atest3
  let tot := loadGrid dat id
  let init := getComponents tot
  for (c, h) in init do
    if c == 'R' then
      IO.println <| s!"'{c}': area {area h}, breaks {breaks h}"

/-!
-/

-- 1483212 -- 76s
/--
info:
Value: I, area : 14
Value: M, area : 5
Value: C, area : 14
Value: F, area : 10
Value: V, area : 13
Value: E, area : 13
Value: I, area : 4
Value: R, area : 12
Value: S, area : 3
Value: J, area : 11
Value: C, area : 1
-/
#guard_msgs in
#eval do
  let dat := atest3
  let mut tot := loadGrid dat id
  for (p, comp) in getComponents tot do
    IO.println s!"Value: {p}, area : {area comp}"

def corner (g : OneComp) (p : pos) : Nat :=
  --let ld := #[(1, 0), (0, - 1)].map (g.growing.contains <| p + ·)
  let angles := #[id, rot, rot ∘ rot, rot ∘ rot ∘ rot].map (#[(- 1, 0), (0, 1)].map ·)
  let ru := (#[(- 1, 0), (0, 1)].map fun d => !g.growing.contains (p + d)).all id
  let cond := angles.map fun dirs => (dirs.map fun d => !g.growing.contains (p + d)).all id
  dbg_trace "{p}\nru: {ru}\ncond: {cond}\n"
  default

def dist (p q : pos) : Nat :=
  let pq := p - q
  (pq.1 ^ 2 + pq.2 ^ 2).natAbs

def mkPath (front : Std.HashSet pos) : Array pos := Id.run do
  let mut fd := #[]
  let mut curr := front.toArray[0]!
  let mut left := front.erase curr
  while ! left.isEmpty do
    let close := left.filter (dist · curr ≤ 2)
    if close.isEmpty then return fd
    let closer := close.filter (dist · curr ≤ 1)
    curr := if closer.isEmpty then close.toArray[0]! else closer.toArray[0]!
    fd := fd.push curr
    left := left.erase curr
  return fd

#eval do
  let dat := atest3
  let dat ← IO.FS.lines input
  let mut tot := loadGrid dat id
  let st : pos := (0, 0)
  let p := tot[st]!
  let comp := mkOneComp tot p st
  let comp := growComp comp
  let f := #[(0, 7), (1, 8), (5, 12), (8, 12)].map (corner comp)
  dbg_trace f
  dbg_trace mkPath comp.edges
  let mut mk := comp.growing
  for m in comp.growing do
    for n in #[(1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2)] do
      mk := mk.insert (n + m)
  draw <| drawSparse mk 20 20

  IO.println s!"Value: {p}, area : {area comp}"
  draw <| drawSparse comp.edges 20 20
  draw <| drawSparse comp.growing 20 20

#eval do
  let dat := atest2
  let dat := atest1
  let dat := atest3
  let tot := loadGrid dat id
  --let st : pos := (0, 0)
  let plants : Std.HashSet Char := .ofArray tot.valuesArray
  for p in plants do
    let gr := sparseGrid dat (· == p)
    let st := gr.toArray.back!
    let gr := mkOneComp tot p st
    --IO.println s!"initial grid, growing and grown for {p}"
    --draw <| drawSparse gr.gr dat.size dat.size
    --draw <| drawSparse gr.growing dat.size dat.size
    let comp := connectedComponent gr
    IO.println s!"Start {st}, value: {p}, area : {comp.size}"
    draw <| drawSparse (connectedComponent gr) dat.size dat.size


/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day12
