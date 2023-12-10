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
def mv (x : pos) (vis : Array pos) : pos :=
  let new := possibleMoves dat x
  match new.filter (! · ∈ vis) with
    | #[xx] => xx
    | ohno! => dbg_trace s!"\noh no!\nmv\npos: {x}\nvis: {vis}\nohno: {ohno!}"; x

def findS (dat : Array String) : pos :=
  let lin := (dat.findIdx? (String.contains · 'S')).getD 0
  let loc := (dat[lin]!.find (· == 'S')).byteIdx
  (lin, loc)

def findToX (dat : Array (Array Char)) (X : pos) : Array pos :=
  let xx := nbs.map fun nb => --if (0 ≤ nb.1 ∧ 0 ≤ nb.2 ) then
    (possibleMoves dat (X + nb)).push (X + nb)
--  else #[]
  let cands := ((xx.filter (X ∈ ·)).map <| fun x => x.filter (· != X))
  cands.map (Array.back ·)



#eval do
  let maze := ← IO.FS.lines input
  let maze := (test.splitOn "\n").toArray
  let dat := maze.map (List.toArray ∘ String.toList)
  let S := findS maze
  IO.println <| S
  IO.println <| findToX dat S


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

#assert part1 (test.splitOn "\n").toArray == 8

solve 1 7066

#eval show MetaM _ from do
  let dat := (← IO.FS.lines input)
  let dat := (test.splitOn "\n").toArray
  return findS dat

#eval IO.println test

#check Array.eraseIdx

def draw (ar : Array String) : IO Unit := do
  let sep := String.mk <| List.replicate (ar[0]!.length + 2) '-'
  IO.println <| sep
  for i in ar do
    IO.println s!"|{i}|"
  IO.println <| sep

/-- the four directions `L`eft, `R`ight, `U`p, `D`own,
and... `S`tay. -/
inductive out | L | R | U | D | X
  deriving BEq, DecidableEq, Inhabited, Repr

instance : ToString out where
  toString | .L => "←" | .R => "→" | .U => "↑" | .D => "↓" | .X => "·"

open out in
def orient : Char → Array out × Array out
  | '|' => (#[L], #[R])
  | '-' => (#[U], #[D])
  | 'F' => (#[U, L], #[D, R])
  | 'J' => (#[U, L], #[D, R])
  | '7' => (#[U, R], #[D, L])
  | 'L' => (#[U, L], #[D, R])
  | 'S' => dbg_trace s!"orient: S"; default
  | not => dbg_trace s!"orient: {not}"; default

#check String.get

/-- rotate counter-clockwise. -/
def rot : out → out
  | .L => .D
  | .R => .U
  | .U => .L
  | .D => .R
  | .X => .X

/-- rotate clockwise. -/
def crot : out → out
  | .L => .U
  | .R => .R
  | .U => .R
  | .D => .L
  | .X => .X

instance : Sub pos where
 sub x y := (x.1 - y.1, x.2 - y.2)

def toLeft : pos → out
  | (  1,   0) => .R
  | (- 1,   0) => .L
  | (  0, - 1) => .D
  | (  0,   1) => .U
  | _ => .X

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

#eval
  let path : Array pos := #[(0, 0), (0, 1), (0, 2), (1, 2), (1, 1), (1, 0)]
  orientPath path

#eval do
  let dat ← IO.FS.lines input
  let dat := (test.splitOn "\n").toArray
  let path := getPath dat
  draw dat
  IO.println <| orientPath path
  --return (dat.size, dat[0]!.length, dat.size * dat[0]!.length)

variable (orp : Array (pos × out)) in
def inside? (p : pos) : Bool :=
  let pth := orp.map Prod.fst
  let bd := 1 + (pth.map Prod.snd).foldl (max · ·) 0
  if p ∈ pth then false
  else
    Id.run do
      let mut prev := p
      let mut curr := p + (0,1)
--      let mut con := 0
      while ((!curr ∈ pth) ∧ curr.2 ≤ bd) do
--        dbg_trace curr
--        con := con + 1
        prev := curr
        curr := curr + (0,1)
--      dbg_trace con
      if bd ≤ curr.2 then false else
      return ! (curr, out.L) ∈ orp
      --true

def test2 := "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."

#eval do
  let dat := (test.splitOn "\n").toArray
  let dat := (test2.splitOn "\n").toArray
  let dat ← IO.FS.lines input
  let path := getPath dat
--  draw dat
  let orp := orientPath path
  let mut con := 0
  let mut outside := #[]
  let mut inside := #[]
--  IO.println <| orp
  for i in [:dat.size] do
    let cou := orp.filter fun x : pos × out => x.1.1 == i
    let cop := cou.map Prod.fst
    for j in [:dat[0]!.length] do
      let pp : pos := (i, j)
      if ! pp ∈ cop then
      --if (! (i, j-1) ∈ outside) ∨ inside? orp (i, j) then
      if pp - ((0, 1) : pos) ∈ outside then
        outside := outside.push pp
      else if inside? cou pp then
        inside := inside.push pp
      else
        outside := outside.push pp
      --else
      --  outside := outside.push (i, j)
--        IO.println <| s!"inside? {(i, j)}: {inside? orp (i, j)}"
--  IO.println <| inside? orp (0,0)
--  IO.println <| inside? orp (6,5)
  IO.println inside.size

--  too high: 19600
#exit



def orientPath (path : Array pos) : Array (pos × out) :=
  let ps := path.size
  let
  Id.run do
    let mut new := #[]
    for p in [:ps] do
      let dif := path[(p + 1) % ps]! - path[(p - 1) % ps]!
      if dif.1 != 0 && dif.2 != 0 then
        new := (new.push path[p]!).push path[p]!
      else
        new := new.push path[p]!
    return new

def extendPath (path : Array pos) : Array pos :=
  let ps := path.size
  Id.run do
    let mut new := #[]
    for p in [:ps] do
      let dif := path[(p + 1) % ps]! - path[(p - 1) % ps]!
      if dif.1 != 0 && dif.2 != 0 then
        new := (new.push path[p]!).push path[p]!
      else
        new := new.push path[p]!
    return new



#eval
  let path : Array pos := #[(0, 0), (0, 1), (0, 2), (1, 2), (1, 1), (1, 0)]
  extendPath path


def frame (path : Array pos) (fr : pos × out) : pos × out :=
  let si := path.findIdx? (· == fr.1)
  let (l, r) := fr.1
  let i := si.get!
  let (nr, nc) := path[(i + 1) % path.size]!
  let orients := orient char
  let ors := orients.1 ++ orients.2
  if ! d ∈ ors then X else
    default



open out in
def orientPath (dat : Array String) (path : Array pos)
    (d : out) (i : Nat) : out :=
  let (r, c) := path[i % path.size]!
  let row := dat[r.toNat]!
  let char := row.get ⟨c.toNat⟩
  let (nr, nc) := path[(i + 1) % path.size]!
  let nrow := dat[nr.toNat]!
  let nchar := nrow.get ⟨nc.toNat⟩
  let orients := orient char
  let ors := orients.1 ++ orients.2
  if ! d ∈ ors then X else
  match nchar with
    | '|' => d
    | '-' => d
    | 'F' => rot d
    | 'J' => rot d
    | '7' => crot d
    | 'L' => rot d
    | 'S' => dbg_trace s!"orient: S"; X
    | not => dbg_trace s!"orient: {not}"; X

#eval
  let dat := #["-J"]
  let path : Array pos := #[(0, 0), (0, 1)]
  let d : out := .U
  orientPath dat path d 0

def orient (p : pos) : Array out :=
  match

def driftLeft (init : pos) (path : Array pos) (inside? : Bool := false) : pos × Bool :=
  let L : pos := (0, -1)
  let U : pos := (-1, 0)
  if ! init + L ∈ path then (init + L, inside?)
  else if ! init + U ∈ path then (init + U, inside?)
  else
  default


#eval
  let p : pos := (1, 3)
  let dat := #["0123".toList.toArray, "4567".toList.toArray, "8901".toList.toArray]
  let nl := (dat[p.1.toNat]!.eraseIdx (p.2.toNat + 1)).insertAt! p.2.toNat '='
  (dat.eraseIdx (p.1.toNat)).insertAt! p.1.toNat nl

#check Array.find?
def makeLine (i lth : Nat) (path : Array pos) : String :=
  let row := path.filter (fun x : Int × Int => Prod.fst x == i)
  let chars := (List.range lth).map fun x : Nat =>
    if (row.find? (Prod.snd · == x)).isSome then '*' else ' '
  String.mk chars




#exit

#eval show MetaM _ from do
  let maze := (test.splitOn "\n").toArray
  let maze := (← IO.FS.lines input)
  draw maze
  IO.println ""
  let path := getPath maze
  let dat := maze.map (List.toArray ∘ String.toList)
  IO.println path
#exit
  let new :=
    Id.run do
    let mut np := dat
--    let mut nl := dat[0]!
    for p in path do
      --let ln := maze.findIdx?
      let nl := (np[p.1.toNat]!.eraseIdx (p.2.toNat + 1)).insertAt! p.2.toNat '*'
      np := (np.eraseIdx (p.1.toNat)).insertAt! p.1.toNat nl
    return np
--  let draw := Id.run do
--    let mut row := #[]
--    for i in [:maze.size] do
--      let cond := path.filter (fun x : Int × Int => Prod.fst x == i)
--      row := row.push cond
--      return row
  let byRows := (Array.range maze.size).map fun i : Nat => path.filter
    (fun x : Int × Int => Prod.fst x == i)

  IO.println byRows
  let tot := new.map (String.mk ∘ Array.toList)
--  for t in tot do
--    IO.println t

  IO.println <| s!"path length: {path.size}"

  IO.println <| path.size / 2
  let empt := ' '
  let full := '*'
--  let line := #[' ']
--  let line := line.foldl (Array.push) #[]
  let line := (List.replicate maze[0]!.length empt).toArray
--  let line := (List.range maze[0]!.length).map fun _ => line.push ' '
  --let next := maze.map String.push (fun )
  let allSt := (List.replicate maze.size line).toArray
  let (new, ps) :=
    Id.run do
    let mut ps := 0
    let mut np := allSt
--    let mut nl := dat[0]!
    for p in path do
      ps := ps + 1
      --let ln := maze.findIdx?
      let nl := (np[p.1.toNat]!.eraseIdx (p.2.toNat + 1)).insertAt! p.2.toNat full
      np := (np.eraseIdx (p.1.toNat)).insertAt! p.1.toNat nl
    return (np, ps)
  IO.println ps
  let tot := new.map (String.mk ∘ Array.toList)

  for t in tot do
    IO.println <| t



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
