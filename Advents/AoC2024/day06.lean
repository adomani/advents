import Advents.Utils
open Lean

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
  mz : Std.HashSet pos
  /-- `S` is the current location of the guard. -/
  S  : pos
  /-- `d` is the current direction of the guard. .-/
  d  : pos
  /-- `visited` is the record of all the pairs position-direction through which the guard passed. -/
  visited : Std.HashSet (pos × pos)
  /-- `bd` is the "boundary": it is the location of every point in the grid and
  it is used to find out when the guard leaves it. -/
  bd : Std.HashSet pos
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

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let gm := mkGuardMoves dat
  let path : Std.HashSet pos := (moveUntil gm).1.erase gm.S
  let obsts : Std.HashSet pos := path.fold (init := ∅) fun h obst =>
    let gmo := {gm with mz := gm.mz.insert obst}
    let (_, loop?) := moveUntil gmo
    if loop? then
      h.insert obst
    else h
  obsts.size

#assert part2 atest == 6

--set_option trace.profiler true in solve 2 1770  -- takes almost 10 minutes

end Day06
