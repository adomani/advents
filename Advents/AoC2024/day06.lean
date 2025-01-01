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

def findNext (gr mz : Std.HashSet pos) (p d : pos) : pos × Bool := Id.run do
  let mut curr := p
  while gr.contains curr && !mz.contains curr do
    curr := curr + d
  return (curr - d, mz.contains curr)

def findNextAll (gr mz : Std.HashSet pos) (p d : pos) : Std.HashSet pos := Id.run do
  let mut curr := p
  let mut sides : Std.HashSet pos := ∅
  while gr.contains curr && !mz.contains curr do
    curr := curr + d
    if mz.contains (curr + rot d) then
      --dbg_trace "found {curr}, going from {p} in the direction {d}"
      sides := sides.insert curr
  return sides

structure Edges where
  /-- The returned direction is `(0, 0)`, when the position is on the edge of the grid, pointing
  outwards. -/
  e : Std.HashMap (pos × pos) (pos × pos)


def addEdge (grid mz : Std.HashSet pos) (edgs : Edges) (p d : pos) : Edges :=
  let (finPos, inGrid) := findNext grid mz p d
  {edgs with e := edgs.e.insert (p, d) (finPos, if inGrid then rot d else (0, 0))}

def addNewWall (grid mz : Std.HashSet pos) (edgs : Edges) (wall : pos) : Edges := Id.run do
  let mut e := edgs.e
  for del in [(0, 1), (0, -1), (1, 0), (-1, 0)] do
    let toBreak := findNextAll grid mz wall del
    for p in toBreak do
      match edgs.e[(p, rot (rot del))]? with
        | none => continue
        | some tgt =>
          let newTgt := (findNext grid mz tgt.1 tgt.2)
          let negDel := rot (rot del)
          --dbg_trace "from the edge {(p, negDel)}--{tgt} to {(p, negDel)} and {(wall + del, rot negDel)}"
          e := e.insert (p, negDel) (wall + del, rot negDel)

          let secTgt := findNext grid mz (wall + del) (rot negDel)
          --dbg_trace "also insert {(wall + del, rot negDel)}--{(secTgt.1, if secTgt.2 then del else (0, 0))}"
          e := e.insert (wall + del, rot negDel) ((secTgt.1, if secTgt.2 then del else (0, 0)))
    --let pths := edgs.e.filter fun (p, d) next => p.1 == wall.1
    --edgs.e[(p, rot (rot del))]?
  return {edgs with e := e}
  --let (finPos, inGrid) := findNext grid mz p d
  --let newEdges := edgs.e.insert (p, d) (finPos, if inGrid then rot d else (0, 0))
--
  --let newEdges := if inGrid then
  --  dbg_trace "also inserting {(finPos, rot (rot d))} to {(p+ d, rot (rot (rot d)))}"
  --  newEdges.insert (finPos, rot (rot d)) (p, rot (rot (rot d))) else newEdges
  --{edgs with e := newEdges}

def addEdgeAllDirs (grid mz : Std.HashSet pos) (edgs : Edges) (p : pos) : Edges :=
  [(0, 1), (0, -1), (1, 0), (-1, 0)].foldl (init := edgs) fun h' d =>
    let e' := addEdge grid (mz.insert p) h' (p - d) (rot d)
    e'

def addEdges (grid mz new : Std.HashSet pos) (edgs : Edges) : Edges :=
  new.fold (init := edgs) fun h p =>
    [(0, 1), (0, -1), (1, 0), (-1, 0)].foldl (init := h) fun h' d => addEdge grid mz h' (p - d) (rot d)

/-
partial
def length (e : Edges) (p d : pos) (tot : Nat) : Nat :=
  if d == (0, 0) then tot else
  match e.e[(p, d)]? with
      | none => tot
      | some (p', d', t) => length e p' d' (tot + t)
-/

partial
def next (e : Edges) (p d : pos) : pos × pos :=
  e.e.getD (p, d) default

def nextCache (memo : Std.HashSet (pos × pos)) (e : Edges) (p d : pos) :
    pos × pos × Std.HashSet (pos × pos) :=
  match e.e[(p, d)]? with
      | none => (default, default, memo)
      | some (p', d') => (p', d', memo.insert (p, d))

def loops? (memo : Std.HashSet (pos × pos)) (grid hashes : Std.HashSet pos)
    (e : Edges) (p d : pos) : Bool := Id.run do
  let (firstWall, _) := findNext grid hashes p d
  let mut memo := memo
  let mut (p1, d1) := (firstWall, rot d)
  let mut con := 0
  while d1 != (0, 0) && !memo.contains (p1, d1) do
    --dbg_trace (p1, d1)
    (p1, d1, memo) := nextCache memo e p1 d1
    con := con + 1
  --dbg_trace "went around {con} times\nd1 != (0, 0): {d1 != (0, 0)}\n!memo.contains {(p1, d1)}: {!memo.contains (p1, d1)}"
  return d1 != (0, 0)

def addAndLoops? (memo : Std.HashSet (pos × pos)) (grid hashes : Std.HashSet pos)
    (e : Edges) (wall p d : pos) : Bool :=
  let hashes := hashes.insert wall
  --dbg_trace "edges before: {e.e.size}"
  let e := addEdgeAllDirs grid hashes e wall
    --[(0, 1), (0, -1), (1, 0), (-1, 0)].foldl (init := e) fun h' d' => addEdge grid hashes h' (wall - d') (rot d')
  dbg_trace "edges after: {e.e.size}, {e.e.contains ((6, 4), (-1, 0))}"
  loops? memo grid hashes e p d

#eval do
  let dat := atest
  let szx := atest.size
  let szy := atest[0]!.length
  let grid := sparseGrid dat (fun _ => true)
  let Spos := sparseGrid dat (· == '^') |>.toArray[0]!
  let S : pos × pos := (Spos, (-1, 0))
  let hashes := sparseGrid dat (· == '#') --|>.insert (S.1 + (0, -1))
  let edgs := addEdges grid hashes hashes ⟨∅⟩
  let del := (-1, 0)
  let wl := (6, 3)
  --let s := findNextAll grid hashes wl del
  --dbg_trace "adding a total of {s.size} broken paths"
  --dbg_trace "breaking {s.fold (init := #[]) fun (h : Array _) p => h.push (edgs.e[(p, rot (rot del))]?)} broken paths"
  draw <| drawSparse hashes szx szy
  let newEdges := addNewWall grid hashes edgs wl
  IO.println <| loops? ∅ grid (hashes.insert wl) newEdges S.1 S.2
  IO.println <| loops? ∅ grid hashes edgs S.1 S.2
  --dbg_trace "putting a wall at {S.1 + (0, -1)}"
  --IO.println <| addAndLoops? ∅ grid (hashes.insert (S.1 + (0, -1))) edgs (S.1 + (0, -1)) S.1 S.2
  --let mut (p1, d1) := ((1, 4), (0, 1))
--#exit
  let (firstWall, _) := findNext grid hashes S.1 S.2
  let mut curr := (firstWall, rot S.2)
  let mut (p1, d1) := curr
  while d1 != (0, 0) do
    IO.println <| curr
    (p1, d1) := next edgs p1 d1
    curr := (p1, d1)
  IO.println <| curr
  --IO.println <| edgs.e.toArray
--#exit
#eval do
  let dat := atest
  let szx := atest.size
  let szy := atest[0]!.length
  let grid := sparseGrid dat (fun _ => true)
  let hashes := sparseGrid dat (· == '#')
  let edgs := (addEdges grid hashes hashes ⟨∅⟩).e
  let removeDir := edgs.fold (init := ∅)
    fun (h : Std.HashMap pos (pos × pos)) p ((d, e, _) : pos × pos × Nat) => h.insert (Prod.fst p) (d, e)
  let _ : ToString (pos × pos) :=
    ⟨fun s : pos × pos => match s.2 with
      | ( 1,  0) => "↓"
      | (-1,  0) => "↑"
      | ( 0,  1) => "→"
      | ( 0, -1) => "←"
      | _ => "·"
       ⟩
  draw <| drawSparse hashes szx szy
  draw <| drawHash removeDir szx szy
  IO.println <| edgs.toArray
  let S : pos × pos := ((1, 1), (0, 1))
  IO.println <| findNext grid hashes (1, 1) (0, 1)
  IO.println <| findNext grid hashes (9, 1) (-1, 0)
  IO.println <| hashes.contains (6, 1)
  let next := edgs.filter

def mkEdges (mz : Std.HashSet pos) (szx szy : Int) : Std.HashMap (pos × pos) (pos × pos) := Id.run do
  let mut fin := ∅
  for p@(mx, my) in mz do
    let (mxsmalls, mxbigs) := mz.partition (·.1 < mx)
    let (mysmalls, mybigs) := mz.partition (·.2 < my)

    let mxsmall   : pos := mxsmalls.toArray.qsort (· > ·) |>.getD 0 (0, my)
    let dirxsmall : pos := ( 1,  0)
    let mxbig     : pos := mxbigs.toArray.qsort (· < ·)   |>.getD 1 (szx, my)
    let dirxbig   : pos := (-1,  0)
    let mysmall   : pos := mysmalls.toArray.qsort (· > ·) |>.getD 0 (mx, 0)
    let dirysmall : pos := ( 0,  1)
    let mybig     : pos := mybigs.toArray.qsort (· < ·)   |>.getD 1 (mx, szy)
    let dirybig   : pos := ( 0, -1)

    fin := fin  |>.insert (p - dirxsmall, dirxsmall) (mxsmall + dirxsmall, rot dirxsmall)
                |>.insert (p - dirysmall, dirysmall) (mysmall + dirysmall, rot dirysmall)
                |>.insert (p - dirxbig,   dirxbig)   (mxbig   + dirxbig,   rot dirxbig)
                |>.insert (p - dirybig,   dirybig)   (mybig   + dirybig,   rot dirybig)

  return fin

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
  let grid := sparseGrid dat (fun _ => true)
  let Spos := sparseGrid dat (· == '^') |>.toArray[0]!
  let S : pos × pos := (Spos, (-1, 0))
  let hashes := sparseGrid dat (· == '#')
  let edgs := addEdges grid hashes hashes ⟨∅⟩

  let obsts : Std.HashSet pos := path.fold (init := ∅) fun h obst =>
    let newEdges := addNewWall grid hashes edgs obst
    let loop? := loops? ∅ grid (hashes.insert obst) newEdges S.1 S.2
    if loop? then
      h.insert obst
    else h
  obsts.size

#assert part2 atest == 6

set_option trace.profiler true in solve 2 1770  -- takes approx 21s

end Day06
