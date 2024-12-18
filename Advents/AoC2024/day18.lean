import Advents.Utils
open Lean

namespace Day18

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day18.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- The exit position for the main puzzle. -/
def exit := (70, 70)

/-- The number of corrupted byte for the main puzzle. -/
def firstKb := 1024

/-- The exit position for the test. -/
def exit_test := (6, 6)

/-- The number of corrupted byte for the test. -/
def firstKb_test := 12

structure MS where
  fallen : Std.HashSet pos
  steps : Nat := 0
  size : Nat
  frontHistorians : Std.HashSet pos := {(0, 0)}
  visitedHistorians : Std.HashSet pos := {(0, 0)}
  frontCorrupted : Std.HashSet pos := fallen.filter fun (p, q) => p == 0 || q == size
  visitedCorrupted : Std.HashSet pos := fallen.filter fun (p, q) => p == 0 || q == size


def MS.available (ms : MS) (p : pos) : Bool :=
  (! ms.fallen.contains p) && 0 ≤ min p.1 p.2 && max p.1 p.2 ≤ ms.size

def inputToMS (s : Array String) (init sz : Nat) : MS :=
  { fallen := (s.take init).foldl (fun h p =>
          if let [x, y] := p.getNats then h.insert (y, x) else panic "wrong input") ∅
    size := sz }

def drawMS (ms : MS) : IO Unit := do
  IO.println s!"Steps: {ms.steps}"
  draw <| drawSparseWith (ms.visitedHistorians.union ms.fallen) (ms.size + 1) (ms.size + 1) (fun p =>
    if ms.fallen.contains p && ms.visitedCorrupted.contains p then "*" else
    if ms.fallen.contains p then "#" else if ms.visitedHistorians.contains p then "O" else "·")

abbrev nbs : Array pos := #[(0, 1), (0, - 1), (1, 0), (- 1, 0)]

def move (ms : MS) : MS :=
  let (newFront, newVisited) := ms.frontHistorians.fold (init := (∅, ms.visitedHistorians)) fun (hf, hv) p =>
    let new := nbs.filterMap fun n =>
      let pNew := p + n
      if ms.fallen.contains pNew || ! ms.available pNew then none else
      if !hv.contains pNew then some pNew else none
    (hf.insertMany new, hv.insertMany new)
  {ms with
    frontHistorians := newFront
    visitedHistorians := newVisited
    steps := ms.steps + 1 }

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let (sz, ex) := if dat.size ≤ 1000 then (firstKb_test, exit_test) else (firstKb, exit)
  let mut ms := inputToMS dat sz ex.1
  while (! ms.visitedHistorians.contains ((ex.1, ex.2) : pos)) do
    ms := move ms
  ms.steps

#assert part1 atest == 22

solve 1 380

/-!
#  Question 2
-/

abbrev blockNbs : Array pos := nbs ++ #[(1, 1), (1, - 1), (- 1, 1), (- 1, 1)]

def moveBoth (ms : MS) : MS :=
  -- expanding the accessible locations
  let (newFrontH, newVisitedH) :=
    ms.frontHistorians.fold (init := (∅, ms.visitedHistorians)) fun (hf, hv) p =>
      let new := nbs.filterMap fun n =>
        let pNew := p + n
        if ms.fallen.contains pNew || ! ms.available pNew then none else
        if !hv.contains pNew then some pNew else none
    (hf.insertMany new, hv.insertMany new)
  -- expanding the inaccessible locations
  let (newFrontC, newVisitedC) :=
    ms.frontCorrupted.fold (init := (∅, ms.visitedCorrupted)) fun (hf, hv) p =>
      let new := blockNbs.filterMap fun n =>
        let pNew := p + n
        if (! ms.fallen.contains pNew) || hv.contains pNew then none else
        some pNew
    (hf.insertMany new, hv.insertMany new)
  {ms with
    frontHistorians := newFrontH
    visitedHistorians := newVisitedH
    frontCorrupted := newFrontC
    visitedCorrupted := newVisitedC
    steps := ms.steps + 1 }

def HorC (dat : Array String) (one : Nat) : Bool := Id.run do
    let ex := if dat.size ≤ 1000 then exit_test else exit
    let mut ms := inputToMS dat one ex.1
    let newTarget : Std.HashSet pos :=
      ms.fallen.filter fun (p, q) => q == 0 || p == ex.1
    while ((newTarget.filter ms.visitedCorrupted.contains).isEmpty &&
          !(ms.visitedHistorians.contains ((ex.1, ex.2) : pos))) do
      ms := moveBoth ms
    return ms.visitedHistorians.contains (ex.1, ex.2)

partial
def search (fin : Nat) (cond : Nat → Bool) (st : Nat := 0) : Option Nat :=
  if st == fin then (if cond st then none else some st) else
  let mid := (st + fin) / 2
  if cond mid then search fin cond (mid + 1) else search mid cond st

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : pos :=
  let low := (search (dat.size + 1) (HorC dat ·)).get!
  let cs := dat[low - 1]!.getNats
  (cs[0]!, cs[1]!)

#assert part2 atest == (6, 1)

--set_option trace.profiler true in solve 2 --(26, 50)  -- takes approximately 30s

end Day18
