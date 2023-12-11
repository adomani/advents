import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day10.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test1` is the test string for the problem. -/
def test1 := "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"

/-- A `pos`ition is a pair of integers. -/
abbrev pos := Int × Int

/-- `nbs` is the list of neighbours of `(0, 0)`, horizontally, vertically and diagonally. -/
def nbs := Id.run do
  let mut t := #[]
  for i in [-1, 0, 1] do
    for j in [-1, 0, 1] do
      t := t.push (i,j)
  return t.erase (0,0)

#assert nbs == #[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

variable (dat : Array (Array Char)) in
/-- returns the positions to which you can go by following the pipes
from the given position. -/
def possibleMoves (x : pos) : Array pos :=
  match dat[x.1.toNat]![x.2.toNat]! with
    | '|' => #[x + (1,   0), x + (- 1,   0)]
    | '-' => #[x + (0,   1), x + (  0, - 1)]
    | 'F' => #[x + (1,   0), x + (  0,   1)]
    | 'J' => #[x + (0, - 1), x + (- 1,   0)]
    | '7' => #[x + (0, - 1), x + (  1,   0)]
    | 'L' => #[x + (0,   1), x + (- 1,   0)]
    | 'S' => #[x]
    | _ => #[x]

variable (dat : Array (Array Char)) in
/-- one elementary move along the pipes. -/
def mv (x : pos) (vis : Array pos) : pos :=
  let new := possibleMoves dat x
  match new.filter (! · ∈ vis) with
    | #[xx] => xx
    | ohno! => dbg_trace s!"\noh no!\nmv\npos: {x}\nvis: {vis}\nohno: {ohno!}"; x

/-- given the data of the problem, returns the position of
the starting position `S`. -/
def findS (dat : Array String) : pos :=
  let lin := (dat.findIdx? (String.contains · 'S')).getD 0
  let loc := (dat[lin]!.find (· == 'S')).byteIdx
  (lin, loc)

/-- given a region of characters and a position `X`,
it returns all the neighbours of `X` from which the pipes
point to `X`. -/
def findToX (dat : Array (Array Char)) (X : pos) : Array pos :=
  let xx := nbs.map fun nb =>
    (possibleMoves dat (X + nb)).push (X + nb)
  let cands := ((xx.filter (X ∈ ·)).map <| fun x => x.filter (· != X))
  cands.map (Array.back ·)

/-
#eval do
  let maze := ← IO.FS.lines input
  let maze := (test1.splitOn "\n").toArray
  let dat := maze.map (List.toArray ∘ String.toList)
  let S := findS maze
  IO.println <| S
  IO.println <| findToX dat S
-/

/-- finds the location of `S` on grid and follows the pipes
around from there.
It returns the array of location that it visited. -/
def getPath (maze : Array String) : Array pos :=
  let dat := maze.map (fun x => x.toList.toArray)
  let S := findS maze
  let fin := (findToX dat S)[1]! --(S + (1, 0))
  Id.run do
  let mut curr := (findToX dat S)[0]!
  let mut prev := S
  let mut vis := #[S].push curr
  let mut con := 2
  while curr != fin do
    con := con + 1
    let currn:= mv dat curr #[prev]
    prev := curr
    curr := currn
    vis := vis.push curr
  vis

/-- `part1 maze` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (maze : Array String) : Nat :=
  (getPath maze).size / 2

#assert part1 (test1.splitOn "\n").toArray == 8

solve 1 7066

/-- a utility function to display arrays of strings.
It assumes that the strings all have the same length,
it also surrounds the data with dashes/vertical bars.
-/
def draw (ar : Array String) : IO Unit := do
  let sep := String.mk <| List.replicate (ar[0]!.length + 2) '-'
  IO.println <| sep
  for i in ar do
    IO.println s!"|{i}|"
  IO.println <| sep

/-- the four directions `L`eft, `R`ight, `U`p, `D`own,
and... `S`tay. -/
inductive out | L | R | U | D | S
  deriving BEq, DecidableEq, Inhabited, Repr

/-- represent each direction by the corresponding arrow. -/
instance : ToString out where
  toString | .L => "←" | .R => "→" | .U => "↑" | .D => "↓" | .S => "·"

/-- converts a unit vector into the direction that is
obtained by a counter-clockwise rotation.
It is useful for defining orientations. -/
def toLeft : pos → out
  | (  1,   0) => .R
  | (- 1,   0) => .L
  | (  0, - 1) => .D
  | (  0,   1) => .U
  | _ => .S

/-- orient a path, assuming that it is a cycle. -/
def orientPath (path : Array pos) : Array (pos × out) :=
  let ps := path.size
  Id.run do
    let lst := path.back
    let fst := path[0]!
    let mut prev := (lst, toLeft (fst - lst))
    let mut oriented := #[]
    for p in [:ps] do
      let curr := path[p]!
      let dif := path[(p + 1) % ps]! - curr
      let dir := toLeft dif
      if dir != prev.2 then
        oriented := (oriented.push (curr, prev.2)).push (curr, dir)
      else
        oriented := oriented.push (curr, dir)
      prev := (curr, dir)
    return oriented

variable (orp : Array (pos × out)) in
/-- check whether a position is inside an oriented path. -/
def inside? (p : pos) : Bool :=
  let pth := orp.map Prod.fst
  let bd := 1 + (pth.map Prod.snd).foldl (max · ·) 0
  if p ∈ pth then false
  else
    Id.run do
      let mut prev := p
      let mut curr := p + (0,1)
      while ((!curr ∈ pth) ∧ curr.2 ≤ bd) do
        prev := curr
        curr := curr + (0,1)
      if bd ≤ curr.2 then false else
      return ! (curr, out.L) ∈ orp

/-- `test2` is the second test string for the problem. -/
def test2 := "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."


/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let path := getPath dat
  let orp := orientPath path
  Id.run do
  let mut con := 0
  for i in [:dat.size] do
    let mut outside := #[]
    let cou := orp.filter fun x : pos × out => x.1.1 == i
    let cop := cou.map Prod.fst
    for j in [:dat[0]!.length] do
      let pp : pos := (i, j)
      if ! pp ∈ cop then
      if pp - ((0, 1) : pos) ∈ outside then
        outside := outside.push pp
      else if inside? cou pp then
        con := con + 1
      else
        outside := outside.push pp
  return con

#assert part2 (test1.splitOn "\n").toArray == 1
#assert part2 (test2.splitOn "\n").toArray == 4

solve 2 401
