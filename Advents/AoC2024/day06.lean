import Advents.Utils
open Std

namespace Day06

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day06.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- Rotation by 90⁰ clockwise. -/
def rot (p : pos) : pos := (p.2, - p.1)

/--
The state for the grid.
* `mz` is the location of the obstacles.
* `S` is the current location of the guard.
* `d` is the current direction of the guard.
* `visited` is the record of all the pairs position-direction through which the guard passed.
* `bd` is the "boundary": it is the location of every point in the grid and it is used to find
  out when the guard leaves it.
* `loop` records whether the guard is known to be in a loop.
-/
structure GuardMoves where
  /-- `mz` is the location of the obstacles. -/
  mz : HashSet pos
  /-- `S` is the current location of the guard. -/
  S  : pos
  /-- `d` is the current direction of the guard. .-/
  d  : pos
  /-- `visited` is the record of all the pairs position-direction through which the guard passed. -/
  visited : HashSet (pos × pos)
  /-- `bd` is the "boundary": it is the location of every point in the grid and
  it is used to find out when the guard leaves it. -/
  bd : HashSet pos
  /-- `loop` records whether the guard is known to be in a loop.-/
  loop : Bool := false
  deriving Inhabited

/-- Creates the initial state of the maze, with the given input. -/
def mkGuardMoves (dat : Array String) : GuardMoves where
  mz := sparseGrid dat (· == '#')
  S  := (sparseGrid dat (· == '^')).toArray[0]!
  d  := (-1, 0)
  visited := (sparseGrid dat (· == '^')).fold (init := {}) (·.insert <| Prod.mk · (-1, 0))
  bd := sparseGrid dat (fun _ => true)

/-- Updates the `GuardMoves` state, by taking one more step (which could be just to rotate). -/
def move (gm : GuardMoves) : GuardMoves :=
  let candPos := (gm.S + gm.d, gm.d)
  if gm.visited.contains candPos then {gm with loop := true} else
  if ! gm.bd.contains candPos.1 then {gm with d := (0, 0)} else
  if ! gm.mz.contains candPos.1
  then
    {gm with S := candPos.1, visited := gm.visited.insert candPos}
  else
    {gm with d := rot gm.d}

/-- Updates the `GuardMoves` state, by taking `n` steps -- the iteration of `move`. -/
def moveN (gm : GuardMoves) : Nat → HashSet pos × Bool
  | 0 => (gm.visited.fold (init := {}) (·.insert ·.1), gm.loop)
  | n + 1 => moveN (move gm) n

/--
Repeatedly take one more step until either the guard exits the grid, or it repeats a
position-direction pair.
-/
def moveUntil (gm : GuardMoves) : HashSet pos × Bool := Id.run do
  let mut gm := gm
  while gm.d != (0, 0) && !gm.loop do
    gm := move gm
  return (gm.visited.fold (init := {}) (·.insert ·.1), gm.loop)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let gm := mkGuardMoves dat
  let (fin, _) := moveUntil gm
  fin.size

#assert part1 atest == 41

solve 1 5086

/-!
#  Question 2
-/

/--
Moves from `p` by increments of `d` until it either finds an element of `hashes` (the position of
a character `#` in the input) or leaves `grid`.

It returns the pair consisting of the final position and,
* if it reached a `hash`, then the rotation of `d`;
* if it exited `grid`, then `(0, 0)`.
-/
def findNext (grid hashes : HashSet pos) (p d : pos) : pos × pos := Id.run do
  let mut curr := p
  while grid.contains curr && !hashes.contains curr do
    curr := curr + d
  return (curr - d, if hashes.contains curr then rot d else (0, 0))

/--
Moves from `p` by increments of `d` until it either finds an element of `hashes` (the position of
a character `#` in the input) or leaves `grid`.
Meanwhile, it keeps track of all the elements of `hashes` that are to the right of the steps that
it takes.

It returns the collection of all positions that have a `hash` to their right.

These are the paths that need to break, if we add `p` as a `hash`.
-/
def findNextAll (grid hashes : HashSet pos) (p d : pos) : HashSet pos := Id.run do
  let mut curr := p
  let mut sides : HashSet pos := ∅
  while grid.contains curr && !hashes.contains curr do
    curr := curr + d
    if hashes.contains (curr + rot d) then
      sides := sides.insert curr
  return sides

/--
`Edges` maps each position-with-direction to the following position-with-direction.

If `(p, d)` maps to `(p', d')`, then this means that `p'` is the latest position, starting
from `p` and going in the direction `d` that is not a hash (`#`), and is contained in the grid.
* If `p'` is followed by a hash (`#`), then `d'` is the 90⁰ clockwise rotation of `d`.
* If `p` is on the edge of the grid, then the returned direction `d'` is `(0, 0)`.
-/
abbrev Edges := HashMap (pos × pos) (pos × pos)

/-- Adds an edge from `(p, d)` to the input `Edges`, using the provided information. -/
def addEdge (grid hashes : HashSet pos) (edgs : Edges) (p d : pos) : Edges :=
  edgs.insert (p, d) (findNext grid hashes p d)

/-- Similar to `addEdge`, except that it adds all edges from `p`, pointing in all directions. -/
def addEdgeAllDirs (grid hashes : HashSet pos) (edgs : Edges) (p : pos) : Edges :=
  [(0, 1), (0, -1), (1, 0), (-1, 0)].foldl (init := edgs) fun h' d =>
    addEdge grid (hashes.insert p) h' (p - d) (rot d)

/--
Similar to `addEdge` and `addEdgeAllDirs`, except that it adds all edges from all positions
in `new`.
-/
def addEdges (grid hashes new : HashSet pos) (edgs : Edges) : Edges :=
  new.fold (init := edgs) (addEdgeAllDirs grid hashes)

/--
Breaks the edges that are already present in `edgs`, according to what they should be
after we add the extra `wall`.
-/
def addNewWall (grid hashes : HashSet pos) (edgs : Edges) (wall : pos) : Edges :=
  [(0, 1), (0, -1), (1, 0), (-1, 0)].foldl (init := edgs) fun e del =>
    let toBreak := findNextAll grid hashes wall del
    toBreak.fold (init := e) fun e p =>
      let negDel := rot (rot del)
      e |>.insert (p, negDel) (wall + del, rot negDel)
        |>.insert (wall + del, rot negDel) (findNext grid hashes (wall + del) (rot negDel))

/--
Moves one step from `(p, d)` following the `Edges`.  The input `memo` records the visited pairs
position-and-direction, so that we can detect loops.
-/
def nextWithMemo (memo : HashSet (pos × pos)) (e : Edges) (p d : pos) :
    pos × pos × HashSet (pos × pos) :=
  match e[(p, d)]? with
      | none => ((0, 0), (0, 0), memo)
      | some (p', d') => (p', d', memo.insert (p, d))
/--
Determines if starting from `p` and moving with direction `d` along the `Edges` in `e`
we ever enter a loop.

The input `memo` is expected to be `∅` at the start and increases by the visited positions at each
step.
-/
def loops? (memo : HashSet (pos × pos)) (grid hashes : HashSet pos)
    (e : Edges) (p d : pos) : Bool := Id.run do
  let (firstWall, _) := findNext grid hashes p d
  let mut memo := memo
  let mut (p1, d1) := (firstWall, rot d)
  while d1 != (0, 0) && !memo.contains (p1, d1) do
    (p1, d1, memo) := nextWithMemo memo e p1 d1
  return d1 != (0, 0)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let gm := mkGuardMoves dat
  let path : HashSet pos := (moveUntil gm).1.erase gm.S
  let grid := sparseGrid dat (fun _ => true)
  let Spos := sparseGrid dat (· == '^') |>.toArray[0]!
  let S : pos × pos := (Spos, (-1, 0))
  let hashes := sparseGrid dat (· == '#')
  let edgs := addEdges grid hashes hashes ⟨∅⟩

  let obsts : HashSet pos := path.fold (init := ∅) fun h obst =>
    let newEdges := addNewWall grid hashes edgs obst
    let loop? := loops? ∅ grid (hashes.insert obst) newEdges S.1 S.2
    if loop? then
      h.insert obst
    else h
  obsts.size

#assert part2 atest == 6

--set_option trace.profiler true in
solve 2 1770  -- takes approx 24s

end Day06
