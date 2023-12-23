import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day23.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- the four directions `L`eft, `R`ight, `U`p, `D`own,
and... `S`tay. -/
inductive dir | L | R | U | D | S
  deriving BEq, DecidableEq, Inhabited, Repr

/-- represent each direction by the corresponding arrow. -/
instance : ToString dir where
  toString | .L => "←" | .R => "→" | .U => "↑" | .D => "↓" | .S => "·"

/-- converts a unit vector into the direction that is
obtained by a counter-clockwise rotation.
It is useful for defining orientations. -/
def pos.toLeft : pos → dir
  | (  1,   0) => .D
  | (- 1,   0) => .U
  | (  0, - 1) => .L
  | (  0,   1) => .R
  | _ => .S

def dir.toPos : dir →  pos
  | .D => (  1,   0)
  | .U => (- 1,   0)
  | .L => (  0, - 1)
  | .R => (  0,   1)
  | .S => (  0,   0)

def Char.toDir : Char → dir
  | '<' => .L
  | '>' => .R
  | '^' => .U
  | 'v' => .D
  | _ => .S

def uts : Array pos := #[(  1,   0), (- 1,   0), (  0, - 1), (  0,   1)]

def getPos (dat : Array String) : HashMap pos dir :=
  Id.run do
  let mut new : HashMap pos dir := .empty
  for i in [:dat.size] do
    let ri := dat[i]!.toList
    for j in [:ri.length] do
      if ri[j]! ≠ '#' then new := new.insert (i, j) ri[j]!.toDir
  return new

#eval do
  let dat := atest
  let mz := getPos dat
  for d in mz do IO.println d
  draw dat

def pos.nbs' (mz : HashMap pos dir) (p : pos) : Array pos :=
  uts.filter fun u => (mz.find? (p + u)).isSome

def pos.nbs'' (mz : HashMap pos dir) (p : pos) : Array pos :=
  uts.filter fun u =>
    match (mz.find? (p + u)) with
      | some d => if d.toPos + u = (0, 0) then false else true
      | none => false

def pos.nbs (mz : HashMap pos dir) (p : pos) : Array pos :=
  (p.nbs' mz).map (p + ·)

def forced (mz : HashMap pos dir) (p : pos) : Bool :=
  (p.nbs mz).size ≤ 2

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mz := getPos dat
  for d in mz do
    if ! forced mz d.1 then IO.println s!"{d}, {(d.1.nbs mz).size}"
  draw dat

abbrev ray := pos × dir

/-
def distRays (mz : HashMap pos dir) (p q : ray) : Option Nat × Array ray :=
  Id.run do
  let mut pth := #[p]
  let mut curr := (p.1 + p.2.toPos, p.2)
  let mut prev := p.1
  let mut con := 0
  while curr.1 ≠ q.1 ∧ con ≤ 200 do
    con := con + 1
    let nb := curr.1.nbs mz
    pth := pth.push curr
    match nb with
      | #[_l, _r] =>
        let c1 := (nb.erase curr.1)[0]!
        curr := (c1, (c1 - prev).toLeft)
        prev := curr.1
      | x => if q.1 ∈ x then curr := q else return (none, pth)
  dbg_trace con
  return (if curr.1 = q.1 then (con, pth) else (none, pth))

def dist (mz : HashMap pos dir) (p q next : pos) : Array pos :=
  Id.run do
  let mut pth := #[p, next]
  let mut curr := next
  let mut prev := p
  let mut con := 0
  while curr ≠ q ∧ con ≤ 200 do
    con := con + 1
    let nb := curr.nbs mz
    match nb with
      | #[_l, _r] =>
        let ncurr := ((nb.reverse).erase prev)[0]!
        prev := curr
        curr := ncurr
      | x => if q ∈ x then curr := q else return pth
    pth := pth.push curr
  dbg_trace con
  return pth --(if curr = q then pth else pth)
-/

def go (mz : HashMap pos dir) (p next : pos) : Array pos :=
  Id.run do
  let mut curr := next
  let mut pth := #[].push p
  let mut prev := p
  let mut con := 0
  while con ≤ 2000 do
    con := con + 1
    pth := pth.push curr
    match curr.nbs mz with
      | nb@#[_, _] =>
        let ncurr := ((nb.reverse).erase prev)[0]!
        prev := curr
        curr := ncurr
      | _ => return pth
  dbg_trace con
  return pth --(if curr = q then pth else pth)

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mz := getPos dat
--  let mut free := #[]
--  for d in mz do
--    if ! forced mz d.1 then free := free.push d; --IO.println s!"{d}, {(d.1.nbs mz).size}"
--  let init : ray := ((5, 3), .R)
--  let pth := dist mz ((5, 3)) ((3, 11)) (5, 4)
  let pth := go mz ((0, 1)) (1, 1)
--  IO.println (pos.nbs mz (5, 7))
  let wInfo := (pth.map fun p => (p, if (mz.find? p).getD default ≠ .S then s!"XXX {(mz.find? p).getD default}" else default))
  for w in wInfo do IO.println <| w
  draw dat




/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2
