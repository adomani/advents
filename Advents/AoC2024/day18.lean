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

/--
The main state of the puzzle.
* `fallen` contains the position of the corrupted memory spaces.
* `steps` is the number of steps that have passed since the beginning.
* `size` is the width of the grid -- it is equal to the height.
* `frontHistorians` contains the positions of the historians at distance roughly `steps`
  movements from the origin.
* `visitedHistorians` contains the positions of the historians at distance at most `steps`
  movements from the origin.
* `frontCorrupted` contains the corrupted memory spaces that can be reached in roughly `steps`
  movements from the top or right edges of the grid.
* `visitedCorrupted` contains the corrupted memory spaces that can be reached with at most `steps`
  movements from the top or right edges of the grid.
-/
structure MemorySpace where
  /-- `fallen` contains the position of the corrupted memory spaces. -/
  fallen : Std.HashSet pos
  /-- `steps` is the number of steps that have passed since the beginning. -/
  steps : Nat := 0
  /-- `size` is the width of the grid -- it is equal to the height. -/
  size : Nat
  /-- `frontHistorians` contains the positions of the historians at distance roughly `steps`
  movements from the origin. -/
  frontHistorians : Std.HashSet pos := {(0, 0)}
  /-- `visitedHistorians` contains the positions of the historians at distance at most `steps`
  movements from the origin. -/
  visitedHistorians : Std.HashSet pos := {(0, 0)}
  /-- `frontCorrupted` contains the corrupted memory spaces that can be reached in roughly `steps`
  movements from the top or right edges of the grid. -/
  frontCorrupted : Std.HashSet pos := fallen.filter fun (p, q) => p == 0 || q == size
  /-- `visitedCorrupted` contains the corrupted memory spaces that can be reached with at most
  `steps` movements from the top or right edges of the grid. -/
  visitedCorrupted : Std.HashSet pos := fallen.filter fun (p, q) => p == 0 || q == size

/-- Checks whether a position is not corrupted and within the bounds of the puzzle. -/
def MemorySpace.available (ms : MemorySpace) (p : pos) : Bool :=
  (! ms.fallen.contains p) && 0 ≤ min p.1 p.2 && max p.1 p.2 ≤ ms.size

/-- Converts the input data into a `MemorySpace` -/
def inputToMemorySpace (s : Array String) (init sz : Nat) : MemorySpace :=
  { fallen := (s.take init).foldl (fun h p =>
          if let [x, y] := p.getNats then h.insert (y, x) else panic "wrong input") ∅
    size := sz }

/--
A utility function to draw a `MemorySpace`: useful for development.
* A position reachable in at most `steps` movement by the historians is `O`.
* A corrupted memory space reachable in at most `steps` movement starting from the
  top or right edges is `*`.
* An empty space is `.` or `O`.
* An corrupted memory space space is `#` or `*`.
-/
def drawMS (ms : MemorySpace) : IO Unit := do
  IO.println s!"Steps: {ms.steps}"
  draw <| drawSparseWith (ms.visitedHistorians.union ms.fallen) (ms.size + 1) (ms.size + 1)
    fun p =>
      if ms.fallen.contains p && ms.visitedCorrupted.contains p then "*" else
      if ms.fallen.contains p then "#" else if ms.visitedHistorians.contains p then "O" else "·"

/-- The steps that a historian can take. -/
abbrev historianNeighbours := #[(0, 1), (0, - 1), (1, 0), (- 1, 0)]

/-- The steps that a corrupted memory space can take. -/
abbrev corruptedNeighbours := historianNeighbours ++ #[(1, 1), (1, - 1), (- 1, 1), (- 1, 1)]

/--
Expands both the positions reachable by the historians and the positions reachable by the
corrupted memory spaces from `ms` by one step.

For part 1, only the movements of the historians are needed, but both are useful for part 2.
-/
def move (ms : MemorySpace) : MemorySpace :=
  -- expanding the accessible locations
  let (newFrontH, newVisitedH) :=
    ms.frontHistorians.fold (init := (∅, ms.visitedHistorians)) fun (hf, hv) p =>
      let new := historianNeighbours.filterMap fun n =>
        let pNew := p + n
        if ms.fallen.contains pNew || ! ms.available pNew then none else
        if !hv.contains pNew then some pNew else none
    (hf.insertMany new, hv.insertMany new)
  -- expanding the inaccessible locations
  let (newFrontC, newVisitedC) :=
    ms.frontCorrupted.fold (init := (∅, ms.visitedCorrupted)) fun (hf, hv) p =>
      let new := corruptedNeighbours.filterMap fun n =>
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

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let (sz, ex) := if dat.size ≤ 1000 then (firstKb_test, exit_test) else (firstKb, exit)
  let mut ms := inputToMemorySpace dat sz ex.1
  while ! ms.visitedHistorians.contains ((ex.1, ex.2) : pos) do
    ms := move ms
  ms.steps

#assert part1 atest == 22

solve 1 380

/-!
#  Question 2
-/

/--
Starting with the input data `data` and a line number `l`, produce the grid whose
corrupted memory spaces are the ones contained up to line `l` in `dat`.

It returns
* `true` if the historians can escape;
* `false` if the historian cannot escape.
-/
def historiansEscape? (dat : Array String) (l : Nat) : Bool := Id.run do
    let ex := if dat.size ≤ 1000 then exit_test else exit
    let mut ms := inputToMemorySpace dat l ex.1
    let newTarget := ms.fallen.filter fun (p, q) => q == 0 || p == ex.1
    while (newTarget.filter ms.visitedCorrupted.contains).isEmpty &&
          ! ms.visitedHistorians.contains (ex.1, ex.2) do
      ms := move ms
    return ms.visitedHistorians.contains (ex.1, ex.2)

/--
Performs a binary search on the predicate `cond` in the range `[st, fin]`.
The underlying assumption is that `cond` as soon as `cond` becomes `false`, it stays `false`
after that (at least in the given range).

With this constraint, the function returns
* `none`, if `con` is constantly equal to `true` in `[st, fin]`;
* `some n`, if `con n` is `false` and `cond (n - 1)` is `true`.

Note. If `n` is `0` in the above, then `cond` is contantly `false` in `[st, fin]`
-/
partial
def firstFalse? (fin : Nat) (cond : Nat → Bool) (st : Nat := 0) : Option Nat :=
  if st == fin then (if cond st then none else some st) else
  let mid := (st + fin) / 2
  if cond mid then firstFalse? fin cond (mid + 1) else firstFalse? mid cond st

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : pos :=
  let low := (firstFalse? (dat.size + 1) (historiansEscape? dat ·)).get!
  let cs := dat[low - 1]!.getNats
  (cs[0]!, cs[1]!)

#assert part2 atest == (6, 1)

--set_option trace.profiler true in solve 2 (26, 50)  -- takes approximately 30s

end Day18
