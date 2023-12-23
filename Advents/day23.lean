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

def addInfo (mz : HashMap pos dir) (p : pos) :=
  (p, if (mz.find? p).getD default ≠ .S then
        s!"XXX {(mz.find? p).getD default}"
      else default)

def tailInfo (mz : HashMap pos dir) (pth : Array pos) :=
  #[pth.pop.pop.back, pth.pop.back, pth.back].map <| addInfo mz

def headInfo (mz : HashMap pos dir) (pth : Array pos) :=
  #[pth[0]!, pth[1]!, pth[2]!].map <| addInfo mz

def htInfo (mz : HashMap pos dir) (pth : Array pos) :=
  (headInfo mz pth, tailInfo mz pth)

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
        let ncurr := (nb.erase prev)[0]!
        prev := curr
        match mz.find? ncurr with
          | none => dbg_trace "oh no!"; return default
          | some d =>
            if d = .S ∨ d.toPos = (ncurr - prev) then
              curr := ncurr
            else return pth
      | _ => return pth
  dbg_trace con
  return pth --(if curr = q then pth else pth)

def chooseBack (mz : HashMap pos dir) (sz : Nat) : Array pos × Nat :=
  Id.run do
  let mut tot := 0
  let mut init : pos := (0, 1)
  let mut bifr := #[init]
  let mut newD : pos := (1, 1)
  let mut pth := go mz init newD
  tot := tot + pth.size - 1
  bifr := bifr.push pth.back
  let mut con := 0
  while con ≤ 1000 ∧ pth.pop.back ≠ (sz, sz) do
    con := con + 1
    let newSteps := pth.back.nbs'' mz
    pth := go mz pth.back (pth.back + newSteps.back)
    tot := tot + pth.size - 1
    init := pth.back
    bifr := bifr.push init
    newD := newSteps.back
  return (bifr, tot)

def go1 (mz : HashMap pos dir) (p next : pos) : Nat × Array (pos × pos) :=
  let pth := go mz p next
  let fin := pth.back
  let newSteps := (fin.nbs'' mz).map (· + fin)
--  dbg_trace pth
  (pth.size - 1, newSteps.map (Prod.mk fin))

partial
def ArrayLexCompare {α} [Inhabited α] [Ord α] (a b : Array α) : Ordering :=
  match a.size, b.size with
    | 0, 0 => .eq
    | 0, _ => .lt
    | _, 0 => .gt
    | _, _ => match compare a.back b.back with
      | .eq => ArrayLexCompare a.pop b.pop
      | c => c

instance {α} [Inhabited α] [Ord α] : Ord (Array α) where
  compare x y := ArrayLexCompare x y

instance {α β} [Ord α] [Ord β] : Ord (α × β) where
  compare x y := match compare x.1 y.1 with
    | .eq => compare x.2 y.2
    | g => g

def findPaths (dat : Array String) : Array Nat :=
  let mz := getPos dat
  let sz := dat.size - 2
  Id.run do
  let mut x : RBTree (Nat × Array (pos × pos)) (fun x y => compare x y) := RBTree.empty
  x := x.insert (0, #[((0, 1), (1, 1))])
  let mut con := 0
  let mut tots := #[]
  while ! x.isEmpty do
    con := con + 1
    for old@(lth, steps) in x do
      let (lthNew, newPairs) := go1 mz steps.back.1 steps.back.2
      for fins in newPairs do
        if fins.2 = ((sz : Int), (sz : Int)) then
          tots := tots.push (lth + lthNew)
        else x := x.insert (lth + lthNew, steps.push fins)
      x := x.erase old
  tots.qsort (· > ·)

#assert findPaths atest == #[94, 90, 86, 82, 82, 74]

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  (findPaths dat)[0]!

#assert part1 atest == 94

solve 1 2366

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2


#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mz := getPos dat
  let sz := dat.size - 2
  IO.println s!"{sz}"
  let mut x : RBTree (Nat × Array (pos × pos)) (fun x y => compare x y) := RBTree.empty
  x := x.insert (0, #[((0, 1), (1, 1))])
  let mut con := 0
  let mut tots := #[]
  while ! x.isEmpty do
    con := con + 1
    for old@(lth, steps) in x do
      let (lthNew, newPairs) := go1 mz steps.back.1 steps.back.2
      for fins in newPairs do
        if fins.2 = ((sz : Int), (sz : Int)) then
          tots := tots.push (lth + lthNew); IO.println s!"found {tots.back} after {con} iterations"
        else x := x.insert (lth + lthNew, steps.push fins)
      x := x.erase old
  IO.print x.toList
  IO.print <| tots.qsort (· > ·)
  --let mut curr : Array (pos × pos) := #[((0, 1), (1, 1))]

  --let extend := go1 mz (0, 1) (1, 1)
  --IO.println s!"extend: {extend}"

#exit
  let mut

  x := RBTree.empty.insert extend.2

    for old@(part, steps) in x do
      if steps.back.2 = ((sz : Int), (sz : Int)) then tots := tots.push (part + extend.1)
      else x := x.insert (go1 mz steps.back.1 steps.back.2)
--      let newSteps := steps.back.1.nbs'' mz
--      for ls in newSteps do
--        let extend := go1 mz steps.back.1 ls
--        else x := x.insert (part + extend.1, steps.push extend.2.back)
      x := x.erase old
  IO.print tots
#exit
--  for l in x do IO.println l
#exit
  let mut part := 0
  let news := #[((0, 1), (1, 1))]
  let (part, news) := go1 mz (0, 1) (1, 1)
  IO.println <| go1 mz (0, 1) (1, 1)
  IO.println <| chooseBack mz sz
  let mut free := #[]
  for d in mz do
    if ! forced mz d.1 then free := free.push d; --IO.println s!"{d}, {(d.1.nbs mz).size}"
  IO.println s!"free: {free.size}\n{free}"
--  let init : ray := ((5, 3), .R)
--  let pth := dist mz ((5, 3)) ((3, 11)) (5, 4)
  let mut init : pos := (0, 1)
  let mut newD : pos := (1, 1)
  IO.println s!"From {init}, going {newD}"
--  IO.println s!"Start from {init}, going {newD}"
  let mut pth := go mz init newD
  let mut con := 0
  while con ≤ 100 ∧ pth.pop.back ≠ (sz, sz) do
    con := con + 1
    let (h, t) := htInfo mz pth
    IO.println <| s!"{h}\n  ...\n{t}"
    IO.println ""
    IO.println s!"From {init}, going {newD}"
    let newSteps := pth.back.nbs'' mz
    pth := go mz pth.back (pth.back + newSteps.back)
    init := pth.back
    newD := newSteps.back
  IO.println ""
--  let newSteps := pth.back.nbs'' mz
--  IO.println s!"directions? {newSteps}"
--  let pth := go mz pth.back (pth.back + newSteps.back)

--  IO.println (pos.nbs mz (5, 7))
--  let wInfo := (pth.map fun p => (p, if (mz.find? p).getD default ≠ .S then s!"XXX {(mz.find? p).getD default}" else default))
--  for w in wInfo do IO.println <| w
  draw dat
