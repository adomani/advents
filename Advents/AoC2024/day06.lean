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

def rot (p : pos) : pos := (p.2, - p.1)

structure GuardMoves where
  mz : Std.HashSet pos
  S  : pos
  d  : pos
  visited : Std.HashSet pos
  bd : Std.HashSet pos
  deriving Inhabited

def mkGuardMoves (dat : Array String) : GuardMoves where
  mz := sparseGrid dat (· == '#')
  S  := (sparseGrid dat (· == '^')).toArray[0]!
  d  := (-1, 0)
  visited := sparseGrid dat (· == '^')
  bd := sparseGrid dat (fun _ => true)

def move (gm : GuardMoves) : GuardMoves :=
  let candPos := gm.S + gm.d
  if ! gm.bd.contains candPos then {gm with d := (0, 0)} else
  if ! gm.mz.contains candPos
  then
    {gm with S := candPos, visited := gm.visited.insert candPos}
  else
    {gm with d := rot gm.d}

def moveUntil (gm : GuardMoves) : Std.HashSet pos := Id.run do
  let mut gm := gm
  while gm.d != (0, 0) do
    gm := move gm
  return gm.visited

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let gm := mkGuardMoves dat
  let fin := moveUntil gm
  let mz := gm.mz
  draw <| drawSparse mz 10 10 --(fun c => if c == '#')
  let _S := gm.S
  draw <| drawSparse (yes := "^") gm.visited 10 10 --(fun c => if c == '#')
  IO.println <| s!"Visited {fin.size} positions"
  draw <| drawSparse (yes := "^") fin 10 10 --(fun c => if c == '#')

  IO.println mz.size

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let gm := mkGuardMoves dat
  let fin := moveUntil gm
  fin.size

#assert part1 atest == 41

solve 1 5086

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day06
