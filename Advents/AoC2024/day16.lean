import Advents.Utils
open Lean

namespace Day16

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day16.input"

/-!
#  Question 1
-/

/-- `test1` is the test string for the problem. -/
def test1 := "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"

/-- `atest1` is the test string for the problem, split into rows. -/
def atest1 := (test1.splitOn "\n").toArray

/-- `test2` is the test string for the problem. -/
def test2 := "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"

/-- `atest2` is the test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

structure RM where
  gr : Std.HashSet pos
  S : pos × pos
  vs : Std.HashSet (pos × pos)
  sz : Nat

structure RMp where
  gr : Std.HashMap pos (Array pos)
  S : pos × pos
  growing : Std.HashMap (pos × pos) Nat
  vs : Std.HashMap (pos × pos) Nat
  sz : Nat

def drawRM0 (rm : RM) : IO Unit := do
  let vis : Std.HashSet pos := rm.vs.fold (·.insert <| Prod.fst ·) {}
  draw <| drawSparseWith (rm.gr.union vis) rm.sz rm.sz (yes := fun p =>
         if vis.contains p then "*"
    else if rm.gr.contains p then "#" else "·")

def inputToRM (s : Array String) : RM :=
  let init := sparseGrid s (· == 'S')
  { gr := sparseGrid s (· == '#')
    S  := (init.toArray[0]!, (0, 1))
    vs := init.fold (·.insert (·, (0, 1))) {}
    sz := s.size }

abbrev nbs : Array pos := #[(0, 1), (0, - 1), (1, 0), (- 1, 0)]

def dirsAt1 (gr : Std.HashSet pos) (p : pos) : Array pos :=
  nbs.filter fun d => gr.contains (p + d)

def dirsAt0 (gr : Std.HashSet pos) (p : pos) : Array pos :=
  nbs.filter fun d => ! (gr.contains (p + d))

def dirsAt (rm : RM) (p : pos) : Array pos :=
  dirsAt0 rm.gr p

def dirsAtp (rm : Std.HashSet pos) (p : pos) : Array pos := nbs.filter fun d => (rm.contains (p + d))

def inputToRMp (s : Array String) : RMp :=
  let init := sparseGrid s (· == 'S')
  let empties := (sparseGrid s (".SE".contains ·)).insert init.toArray[0]!
  { gr := empties.fold (fun h p => h.insert p (dirsAtp empties p)) {}
    S  := (init.toArray[0]!, (0, 1))
    growing := init.fold (·.insert (·, (0, 1)) 0) {}
    vs := init.fold (·.insert (·, (0, 1)) 0) {}
    sz := s.size }

instance : Neg pos where neg p := (- p.1, - p.2)

def update (v : Std.HashMap (pos × pos) Nat) (p d : pos) (val : Nat) :
    Option (Std.HashMap (pos × pos) Nat) :=
  match v.get? (p, d) with
    | none => some (v.insert (p, d) val)
    | some oldVal => if oldVal ≤ val then none else some (v.insert (p, d) val)

def rot (p : pos) : pos := (p.2, - p.1)

def increase (rm : RMp) : RMp := Id.run do
  let mut (grow, vis) := (rm.growing, rm.vs)
  for ((p, d), val) in rm.growing do
    --if p == final then dbg_trace "found {(p, d)}, {val}"
    match update vis p (rot d) (val + 1000) with
      | none => grow := grow --.erase (p, rot d)
      | some v =>
        vis := v
        grow := grow.insert (p, rot d) (val + 1000)
    match update vis p (- rot d) (val + 1000) with
      | none => grow := grow --.erase (p, - rot d)
      | some v =>
        vis := v
        grow := grow.insert (p, - rot d) (val + 1000)
    let newP := p + d
    if rm.gr.contains newP then
      match update vis newP d (val + 1) with
        | none => grow := grow --.erase (newP, d)
        | some v =>
          vis := v
          grow := grow.insert (newP, d) (val + 1)
  return { rm with
    growing := grow.filter fun p _v => (! rm.growing.contains p)
    vs := vis
    }

#eval do
  let dat := atest2 -- 11048
  let dat := atest1 -- 7036
  let dat ← IO.FS.lines input
  let E := sparseGrid dat (· == 'E') |>.toArray[0]!
  dbg_trace E
  let mut rm := inputToRMp dat
  --draw <| drawSparseWith (rm.gr.fold (fun h (p) _ => h.insert p) {}) rm.sz rm.sz (yes := fun p =>
  --      s!"{(rm.gr.getD p #[]).size}")
  let mut con := 0
  let mut oldGrow : Std.HashMap _ _ := {}
  while (oldGrow.toArray != rm.growing.toArray) do
    oldGrow := rm.growing
    con := con + 1
    rm := increase rm
    --IO.println s!"{con}, rm.vs.size: {rm.vs.size}, rm.growing.size: {rm.growing.size}"
  let vals := rm.vs.filter fun ((p, _) : pos × pos) _ => p == E
  IO.println <| vals.fold (fun m _ v => min m v) vals.toArray[0]!.2
  IO.println s!"steps: {con}"
--#exit
  con := 0
  while con ≤ 10 do
    oldGrow := rm.growing
    con := con + 1
    rm := increase rm
    --IO.println s!"{con}, rm.vs.size: {rm.vs.size}, rm.growing.size: {rm.growing.size}"
  let vals := rm.vs.filter fun ((p, _) : pos × pos) _ => p == E
  IO.println <| vals.fold (fun m _ v => min m v) vals.toArray[0]!.2
  --let rm := increase rm
  --IO.println rm.vs.toArray
  --draw <| drawSparseWith (rm.vs.fold (fun h (p) _ => h.insert p) {}) rm.sz rm.sz (yes := fun p =>
  --      s!"{(rm.gr.getD p #[]).size}")
  --IO.println <| travel rm.gr trivs S
  --IO.println <| travel rm.gr trivs ((7, 3), (0, 1))

/-!
-/

-- 109432 too high
-- 99460
def tallyRot (d e : pos) : Nat :=
  if d == e then 0
  else if d == - e then 2000
  else 1000

def travel (gr trivs : Std.HashMap pos (Array pos)) (p : pos × pos) : (Nat × pos × pos) := Id.run do
  let mut (currPos, currDir) := p
  let mut tot := 0
  -- we scan until we find a new vertex of valence at least 3, but we allow "rotating in place"
  while (! trivs.contains currPos) || currPos == p.1 do
    let newPos := currPos + currDir
    if gr.contains newPos then
      currPos := newPos
      tot := tot + 1
    else
      dbg_trace "turning at {currPos}"
      let newDir := (gr[currPos]!.erase (- currDir))[0]!
      tot := tot + tallyRot currDir newDir
      currDir := newDir
  return (tot, (currPos, currDir))

#eval do
  let dat := atest1
  let dat ← IO.FS.lines input
  let dat := atest2
  let rm := inputToRMp dat
  draw <| drawSparseWith (rm.gr.fold (fun h (p) _ => h.insert p) {}) rm.sz rm.sz (yes := fun p =>
        s!"{(rm.gr.getD p #[]).size}")
  let S := rm.S
  let trivs := rm.gr.filter (fun _ arr => 3 ≤ Array.size arr)
  IO.println <| travel rm.gr trivs S
  IO.println <| travel rm.gr trivs ((7, 3), (0, 1))
  --let mut gr := sparseGrid dat (".SE".toList.contains)
  --let mut consume := consumeDE gr S
  --while consume.size < gr.size do
  --  --draw <| drawSparseWith gr dat.size dat.size (yes := fun p => s!"{(dirsAt1 gr p).size}")
  ----draw <| drawSparseWith gr dat.size dat.size --(yes := fun p => "*") --s!"{(rm.gr.getD p #[]).size}")
  --  gr := consume
  --  consume := consumeDE consume S
  --  IO.println (consume.size, gr.size)
  --draw <| drawSparseWith consume dat.size dat.size (yes := fun p => s!"{(dirsAt1 consume p).size}")
  --draw <| drawSparseWith consume dat.size dat.size --(yes := fun p => "*") --s!"{(rm.gr.getD p #[]).size}")


def consumeDE (gr : Std.HashSet pos) (keep : pos := (- 1, - 1)) : Std.HashSet pos :=
  gr.fold (fun h p => if (dirsAt1 gr p).size ≤ 1 then h.erase p else h) gr |>.insert keep

#eval do
  let dat := atest1
  let dat ← IO.FS.lines input
  let dat := atest2
  --let rm := inputToRMp dat
  --draw <| drawSparseWith (rm.gr.fold (fun h (p) _ => h.insert p) {}) rm.sz rm.sz (yes := fun p =>
  --      s!"{(rm.gr.getD p #[]).size}")
  let S := sparseGrid dat ("S".toList.contains) |>.toArray[0]!
  let mut gr := sparseGrid dat (".SE".toList.contains)
  let mut consume := consumeDE gr S
  while consume.size < gr.size do
    --draw <| drawSparseWith gr dat.size dat.size (yes := fun p => s!"{(dirsAt1 gr p).size}")
  --draw <| drawSparseWith gr dat.size dat.size --(yes := fun p => "*") --s!"{(rm.gr.getD p #[]).size}")
    gr := consume
    consume := consumeDE consume S
    IO.println (consume.size, gr.size)
  draw <| drawSparseWith consume dat.size dat.size (yes := fun p => s!"{(dirsAt1 consume p).size}")
  --draw <| drawSparseWith consume dat.size dat.size --(yes := fun p => "*") --s!"{(rm.gr.getD p #[]).size}")

--def shrink (rm : RMp) :

#eval do
  let dat := atest2
  let dat := atest1
  let dat ← IO.FS.lines input
  let rm := inputToRM dat
  drawRM0 rm
  IO.println rm.gr.size
  --draw <| drawSparseWith (rm.gr.fold (·.insert <| Prod.fst ·) {}) rm.sz rm.sz (yes := fun p =>
  --      s!"{(rm.gr.filter (Prod.fst · == p)).size}")

/--
info:
Directions at (13, 1): #[(0, 1), (-1, 0)]
Directions at (13, 2): #[(0, 1), (0, -1)]
Directions at (13, 3): #[(0, -1)]
Directions at (12, 1): #[(1, 0), (-1, 0)]
-/
#guard_msgs in
#eval do
  let dat := atest1
  let rm := inputToRM dat
  --drawRM0 rm
  let ds := #[rm.S.1, rm.S.1 + (0, 1), rm.S.1 + (0, 2), rm.S.1 + (- 1, 0)]
  for d in ds do
    IO.println s!"Directions at {d}: {dirsAt rm d}"

def dirs (rm : RM) : Array pos := dirsAt rm rm.S.1

def drawRM (rm : RM) : IO Unit := do
  drawRM0 rm
  IO.println s!"Available dirs at {rm.S}: {dirs rm}"

#eval do
  let dat := atest1
  let rm := inputToRM dat
  drawRM rm

def moveCost (rm : RM) (d : pos) : Option (Nat × pos) := Id.run do
  let mut (p, d) := (rm.S.1, d)
  let mut tot := 1
  let ds := dirs rm
  let mut con := 0
  while ds.size ≤ 2 && con ≤ 5 do
    con := con
    let ds := dirsAt rm p
    if ds.size == 1 then return none
    let dnew := (ds.filter fun new => new != (0, 0) - d)[0]!
    dbg_trace "dnew: {dnew}"
    tot := tot + if dnew == d then 1 else 1000
    d := dnew
    p := p + d
    dbg_trace "{(p, d)}"
  return some (tot, p)

#eval do
  let dat := atest1
  let rm := inputToRM dat
  drawRM rm
  IO.println <| moveCost rm (0, 1)
  IO.println ""
  IO.println <| moveCost rm (- 1, 0)


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

end Day16
