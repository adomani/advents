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
  visited : Std.HashSet (pos × pos)
  bd : Std.HashSet pos
  loop : Bool := false
  deriving Inhabited

def mkGuardMoves (dat : Array String) : GuardMoves where
  mz := sparseGrid dat (· == '#')
  S  := (sparseGrid dat (· == '^')).toArray[0]!
  d  := (-1, 0)
  visited := (sparseGrid dat (· == '^')).fold (init := {}) (·.insert <| Prod.mk · (-1, 0))
  bd := sparseGrid dat (fun _ => true)

def move (gm : GuardMoves) : GuardMoves :=
  let candPos := (gm.S + gm.d, gm.d)
  if gm.visited.contains candPos then {gm with loop := true} else
  if ! gm.bd.contains candPos.1 then {gm with d := (0, 0)} else
  if ! gm.mz.contains candPos.1
  then
    {gm with S := candPos.1, visited := gm.visited.insert candPos}
  else
    {gm with d := rot gm.d}

def moveN (gm : GuardMoves) : Nat → Std.HashSet pos × Bool
  | 0 => (gm.visited.fold (init := {}) (·.insert ·.1), gm.loop)
  | n + 1 => moveN (move gm) n

variable [Hashable α] [Hashable β] [BEq α] [BEq β] in
def _root_.Std.HashSet.map (h : Std.HashSet α) (f : α → β) : Std.HashSet β :=
  h.fold (·.insert <| f ·) {}

def project [Hashable α] [Hashable β] [BEq α] [BEq β] (h : Std.HashSet (α × β)) : Std.HashSet α :=
  h.map Prod.fst

def moveUntilWithDirs (gm : GuardMoves) : Std.HashSet (pos × pos) × Bool := Id.run do
  let mut gm := gm
  while gm.d != (0, 0) && !gm.loop do
    gm := move gm
  return (gm.visited, gm.loop)

def moveUntil (gm : GuardMoves) : Std.HashSet pos × Bool :=
  let (p, l) := (moveUntilWithDirs gm)
  (project p, l)

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
set_option trace.profiler true in
#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut gm := mkGuardMoves dat
  let mut tried : Std.HashSet pos := gm.mz.insert gm.S
  let mut obsts : Std.HashSet pos := {}
  let path : Std.HashSet (pos × pos) := (moveUntilWithDirs gm).1
  IO.println "First path:"
  draw <| drawSparse (project path) dat.size dat.size
  while gm.d != (0, 0) do
    gm := move gm
  --for (currP, odir) in path do
    let (currP, odir) := (gm.S, gm.d)
    let newObst := currP + odir
    --if newObst == gm.S then continue
    if tried.contains newObst then continue
    --con := con + 1
  --while ! obsts.isEmpty do
    let gmo := {gm with mz := gm.mz.insert newObst}
    let (fin, loop?) := moveUntil gmo
    if loop? then
      obsts := obsts.insert (newObst)
    tried := tried.insert newObst
    --if 1000 ≤ con then IO.println s!"Found {obsts.size} obstacles. I am done!"; return
  IO.println <| s!"{obsts.size} positions form a loop."
  draw <| drawSparse obsts dat.size dat.size

#exit
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
--/-
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
set_option trace.profiler true in
solve 2 1770

end Day06
