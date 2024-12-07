import Advents.Utils
open Lean

namespace Day06

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day06.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

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
* `bd` is the "boundary": it is the location of every point in the grid and it is used to find
  out when the guard leaves it.
* `loop` records whether the guard is known to be in a loop.
-/
structure GuardMoves where
  mz : Std.HashSet pos
  S  : pos
  d  : pos
  visited : Std.HashSet (pos × pos)
  bd : Std.HashSet pos
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
def moveN (gm : GuardMoves) : Nat → Std.HashSet pos × Bool
  | 0 => (gm.visited.fold (init := {}) (·.insert ·.1), gm.loop)
  | n + 1 => moveN (move gm) n

/--
Repeatedly take one more step until either the guard exits the grid, or it repeats a
position-direction pair.
-/
def moveUntil (gm : GuardMoves) : Std.HashSet pos × Bool := Id.run do
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

def project [Hashable α] [Hashable β] [BEq α] [BEq β] (h : Std.HashSet (α × β)) : Std.HashSet α :=
  h.fold (·.insert ·.1) {}

def findFirstRepetition (gm : GuardMoves) : Std.HashSet pos := Id.run do
  let S := gm.S
  let mut gm := gm
  --let mut oldPos := project gm.visited
  let mut cond := true
  while cond do
    let sz := (project gm.visited).size
    let oldDir := gm.d
    gm := move gm
    cond := sz != (project gm.visited).size || oldDir != gm.d || gm.S == S
  return (project gm.visited).erase S

set_option linter.unusedVariables false in
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let gm := mkGuardMoves dat
  let path : Std.HashSet pos := (moveUntil gm).1
  let path := findFirstRepetition gm
  dbg_trace "path size: {path.size}"
  draw <| drawSparse path dat.size dat.size
  let mut obsts : Std.HashSet pos := {}
  let mut con := 0
  let pos := (121, 0)
  let gmo := {gm with mz := gm.mz.insert pos}
  let (fin, lp) := moveUntil gmo
  let steps := 5683
  draw <| drawSparse fin dat.size dat.size
  --draw <| drawSparse (moveN gmo steps).1 dat.size dat.size
  IO.println fin.size
  --draw <| drawSparse fin dat.size dat.size
  dbg_trace "{pos} is in maze: {gm.bd.contains pos}"
  dbg_trace "{pos} is in path: {path.contains pos}"
  --IO.println s!"{(path.filter (! (gm.mz).contains ·)).toArray.qsort (fun a b => a.1 < b.1 || a.1 == b.1 && a.2 < b.2) == path.toArray.qsort (fun a b => a.1 < b.1 || a.1 == b.1 && a.2 < b.2)}"
--#exit

  for obst in path do
    if obst == gm.S then continue
    con := con + 1
  --while ! obsts.isEmpty do
    let gmo := {gm with mz := gm.mz.insert obst}
    let (fin, loop?) := moveUntil gmo
    if loop? then
      obsts := obsts.insert obst
    --if 1000 ≤ con then IO.println s!"Found {obsts.size} obstacles. I am done!"; return
  IO.println <| s!"{obsts.size} positions form a loop."
  draw <| drawSparse obsts dat.size dat.size
  --draw <| drawSparse fin dat.size dat.size
/-
set_option linter.unusedVariables false in
set_option trace.profiler true in
#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let gm := mkGuardMoves dat
  let path : Std.HashSet pos := (moveUntil gm).1
  let path := findFirstRepetition gm
  dbg_trace "path size: {path.size}"
#exit
  draw <| drawSparse path dat.size dat.size
  let mut obsts : Std.HashSet pos := {}
  let mut con := 0
  let pos := (121, 0)
  let gmo := {gm with mz := gm.mz.insert pos}
  let (fin, lp) := moveUntil gmo
  let steps := 5683
  draw <| drawSparse fin dat.size dat.size
  --draw <| drawSparse (moveN gmo steps).1 dat.size dat.size
  IO.println fin.size
  --draw <| drawSparse fin dat.size dat.size
  dbg_trace "{pos} is in maze: {gm.bd.contains pos}"
  dbg_trace "{pos} is in path: {path.contains pos}"
  --IO.println s!"{(path.filter (! (gm.mz).contains ·)).toArray.qsort (fun a b => a.1 < b.1 || a.1 == b.1 && a.2 < b.2) == path.toArray.qsort (fun a b => a.1 < b.1 || a.1 == b.1 && a.2 < b.2)}"
--#exit

  for obst in path do
    if obst == gm.S then continue
    con := con + 1
  --while ! obsts.isEmpty do
    let gmo := {gm with mz := gm.mz.insert obst}
    let (fin, loop?) := moveUntil gmo
    if loop? then
      obsts := obsts.insert obst
    --if 1000 ≤ con then IO.println s!"Found {obsts.size} obstacles. I am done!"; return
  IO.println <| s!"{obsts.size} positions form a loop."
  draw <| drawSparse obsts dat.size dat.size
  --draw <| drawSparse fin dat.size dat.size
#exit
-/
/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let gm := mkGuardMoves dat
  let path : Std.HashSet pos := (moveUntil gm).1.erase gm.S
  let mut obsts : Std.HashSet pos := {}
  let mut con := 0
  for obst in path do
    con := con + 1
    let gmo := {gm with mz := gm.mz.insert obst}
    let (_, loop?) := moveUntil gmo
    if (loop?) then
      obsts := obsts.insert obst
  obsts.size

#assert part2 atest == 6
--solve 2 1770

end Day06
