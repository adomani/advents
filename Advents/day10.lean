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

def findToX (dat : Array (Array Char)) (X : pos) : Array pos :=
  let xx := nbs.map fun nb => (possibleMoves dat (X + nb)).push (X + nb)
  let cands := ((xx.filter (X ∈ ·)).map <| fun x => x.filter (· != X))
  cands.map (Array.back ·)

#eval do
  let maze := ← IO.FS.lines input
  let dat := maze.map (List.toArray ∘ String.toList)
  let S := findS maze
  IO.println <| S
  IO.println <| findToX dat S


def getPath (maze : Array String) : Array pos :=
  let dat := (maze).map (fun x => x.toList.toArray)
  let S := findS maze
  let fin := (findToX dat S)[1]! --(S + (1, 0))
  Id.run do
  let mut curr := (findToX dat S)[0]!
  let mut prev := S
  let mut vis := #[S].push curr
  let mut con := 2
  while curr != fin do
    con := con + 1
    let (currn, visn):= mv dat curr #[prev]
    prev := curr
    curr := currn
    vis := vis.push curr
  vis

/-- `part1 maze` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (maze : Array String) : Nat :=
  (getPath maze).size / 2

#assert part1 (test.splitOn "\n").toArray == 8

solve 1 7066

#eval show MetaM _ from do
  let dat := (← IO.FS.lines input)
  let dat := (test.splitOn "\n").toArray
  return findS dat

#eval IO.println test

#check Array.eraseIdx

#eval
  let p : pos := (1, 1)
  let dat := #["0123".toList.toArray, "4567".toList.toArray, "8901".toList.toArray]
  let nl := (dat[p.1.toNat]!.eraseIdx (p.2.toNat + 1)).insertAt! p.2.toNat '='
  (dat.eraseIdx (p.1.toNat)).insertAt! p.1.toNat nl



#eval show MetaM _ from do
  let maze := (test.splitOn "\n").toArray
  let maze := (← IO.FS.lines input)
  let path := getPath maze
  let dat := maze.map (List.toArray ∘ String.toList)
  let new :=
    Id.run do
    let mut np := dat
    for p in path do
      let ln := maze.findIdx?
      let nl := (dat[p.1.toNat]!.eraseIdx (p.2.toNat + 1)).insertAt! p.2.toNat '='
      np := (np.eraseIdx (p.1.toNat)).insertAt! p.1.toNat nl
    return np
  let tot := new.map (String.mk ∘ Array.toList)
  for t in tot do
    IO.println t

  IO.println <| s!"path length: {path.size}"

  IO.println <| path.size / 2



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


/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
