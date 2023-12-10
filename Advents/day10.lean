import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day10.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"
--variable {α} [Repr α] in
--#synth Repr (α × α)
abbrev pos := Int × Int

--instance : Repr pos where
--  reprPrec x n := reprPrec (x : Int × Int) n

--instance : ToString pos where
--  toString x := ToString.toString (x : Int × Int)

instance : Add pos where
  add x y := (x.1 + y.1, x.2 + y.2)

/-- `nbs` is the list of neighbours of `(0, 0)`, horizontally, vertically and diagonally. -/
def nbs := Id.run do
  let mut t := #[]
  for i in [-1, 0, 1] do
    for j in [-1, 0, 1] do
      t := t.push (i,j)
  return t.erase (0,0)

#assert nbs == #[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

variable (dat : Array (Array Char)) in
def possibleMoves (x : pos) (dbg? : Bool := false) : Array pos :=
  match dat[x.1.toNat]![x.2.toNat]! with
    | c@'|' => /-if dbg? then dbg_trace s!"{c}";-/ #[x + (1,   0), x + (- 1,   0)]
    | c@'-' => /-if dbg? then dbg_trace s!"{c}";-/ #[x + (0,   1), x + (  0, - 1)]
    | c@'F' => /-if dbg? then dbg_trace s!"{c}";-/ #[x + (1,   0), x + (  0,   1)]
    | c@'J' => /-if dbg? then dbg_trace s!"{c}";-/ #[x + (0, - 1), x + (- 1,   0)]
    | c@'7' => /-if dbg? then dbg_trace s!"{c}";-/ #[x + (0, - 1), x + (  1,   0)]
    | c@'L' => /-if dbg? then dbg_trace s!"{c}";-/ #[x + (0,   1), x + (- 1,   0)]
    | 'S' => dbg_trace s!"possibleMoves: beginning! {x}"; #[x]
    | not => dbg_trace s!"possibleMoves: beginning! {not}"; #[x]

#eval
  let dat := (test.splitOn "\n").toArray.map (fun x => x.toList.toArray)
  let x : pos := (1, 1)
  dbg_trace possibleMoves dat x
  0

#eval #[1, 2, 3].filter (· != 2)

variable (dat : Array (Array Char)) in
def mv (x : pos) (vis : Array pos) : pos × Array pos :=
  let new := possibleMoves dat x
  match new.filter (! · ∈ vis) with
    | #[xx] => (xx, vis.push xx)
    | ohno! => dbg_trace s!"\noh no!\nmv\npos: {x}\nvis: {vis}\nohno: {ohno!}"; (x, vis)

def findS (dat : Array String) : pos :=
  let lin := (dat.findIdx? (String.contains · 'S')).getD 0
  let loc := (dat[lin]!.find (· == 'S')).byteIdx
  (lin, loc)

#eval show MetaM _ from do
  let dat := (← IO.FS.lines input)
  let dat := (test.splitOn "\n").toArray
  return findS dat

#eval IO.println test

#eval show MetaM _ from do
  let maze := (test.splitOn "\n").toArray
  let maze := (← IO.FS.lines input)
  let dat := (maze).map (fun x => x.toList.toArray)
  let S := findS maze
  let fin := S + (1, 0)
--  Id.run do
  let mut curr := S + (0, - 1)
  let mut prev := S
  let mut vis := #[S].push curr
  let mut con := 2
  while curr != fin do
    con := con + 1
    let (currn, visn):= mv dat curr vis
    prev := curr
    curr := currn
    vis := vis.push currn
    vis := #[prev]
--  IO.println test
--  IO.println vis
  IO.println con
  IO.println <| con / 2
  IO.println vis.size
  IO.println <| vis.size / 2

-- too low: 7065



variable (dat : Array (Array Char)) in
def pipeToDirs (pp : Array pos × pos) : Array pos × pos :=
  let (vis, x) := pp
  let goingTo : Array pos := match dat[x.1.toNat]![x.2.toNat]! with
    | '|' => #[x + (0, 1), x + (0, - 1)]
    | '-' => #[x + (1, 0), x + (- 1, 0)]
    | 'F' => #[x + (0, 1), x + (1, 0)]
    | 'J' => #[x + (- 1, 0), x + (0, - 1)]
    | '7' => #[x + (- 1, 0), x + (0,   1)]
    | 'S' => dbg_trace s!"pipeToDirs: beginning! {x}"; #[x]
    | not => dbg_trace s!"pipeToDirs: beginning! {not}"; #[x]
  default

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 (test.splitOn "\n").toArray == ???

--solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
