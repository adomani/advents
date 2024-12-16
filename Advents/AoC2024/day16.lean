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
  gr : Std.HashSet pos --(Array pos)
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
  { gr := (sparseGrid s (".SE".contains ·)).insert init.toArray[0]!
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

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let E := sparseGrid dat (· == 'E') |>.toArray[0]!
  let mut rm := inputToRMp dat
  let mut con := 0
  let mut oldGrow : Std.HashMap _ _ := {}
  while (oldGrow.toArray != rm.growing.toArray) do
    oldGrow := rm.growing
    con := con + 1
    rm := increase rm
  let vals := rm.vs.filter fun ((p, _) : pos × pos) _ => p == E
  return vals.fold (fun m _ v => min m v) vals.toArray[0]!.2

#assert part1 atest1 == 7036
#assert part1 atest2 == 11048

--set_option trace.profiler true in solve 1 99460 -- takes approximately 50s

/-!
#  Question 2
-/

/--
`getMinDists rm tgt` does most of the computations.
The input is a grid and a "target" position (`E` in the case of the puzzle).
It returns
* the `HashMap` assigning to each pair `p = (position, direction)` the minimum score of a path
  from the starting position `S` to `p`;
* the `HashMap` assigning value `0` to each pair `p = (position, direction)` where `position`
  is the position of `E` -- this is used to repeat the operation for the reverse path;
* the actual minimum score of a path from `S` to any location with underlying position `E`
  (i.e. allowing any direction at `E`, unlike what happens at `S`).
-/
def getMinDists (rm : RMp) (tgt : pos) :
    Std.HashMap (pos × pos) Nat × Std.HashMap (pos × pos) Nat × Nat := Id.run do
  let mut rm := rm
  let mut oldGrow : Std.HashMap _ _ := {}
  while (oldGrow.toArray != rm.growing.toArray) do
    oldGrow := rm.growing
    rm := increase rm
  let vals := rm.vs.filter fun (p, _) _ => p == tgt
  let minValue := vals.fold (fun m _ v => min m v) vals.toArray[0]!.2
  let reverseStart := vals.fold (init := ∅)
    fun h p v => if v == minValue then h.insert (p.1, -p.2) 0 else h
  return (rm.vs, reverseStart, minValue)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let E := sparseGrid dat (· == 'E') |>.toArray[0]!
  let rm' := inputToRMp dat
  let (rmToE, es, oldMin) := getMinDists rm' E
  let (rmToS, _, newMin) := getMinDists {rm' with growing := es, vs := es} rm'.S.1
  let target := (newMin + oldMin) / 2 + 500
  let mids : Std.HashSet pos := rmToS.fold (fun h p v =>
    let sec := rmToE.getD (p.1, - p.2) 0
    if v + sec ≤ target then h.insert p.1 else h) ∅
  return mids.size

#assert part2 atest1 == 45
#assert part2 atest2 == 64

--solve 2 500 -- takes approximately 1 minute

end Day16

--open Day16 in
set_option trace.profiler true in
#eval do
  let dat := atest2 -- 11048
  let dat := atest1 -- 7036
  let dat ← IO.FS.lines input
  let E := sparseGrid dat (· == 'E') |>.toArray[0]!
  let rm' := inputToRMp dat
  let rmToE := getMinDists rm'
  let initS := rm'.S
  dbg_trace "source: {rm'.S}, target: {E}"
  draw <| drawSparseWith (rm'.gr) rm'.sz rm'.sz --(yes := fun p => s!"{(rm.gr.getD p #[]).size}")
  let vals := rmToE.filter fun ((p, _) : pos × pos) _ => p == E --rm.S.1
  let oldMin := vals.fold (fun m _ v => min m v) vals.toArray[0]!.2
  for v in vals do dbg_trace v
  let es : Std.HashMap (pos × pos) Nat :=
    vals.fold (fun h (p, d) v => if v == oldMin then h.insert (p, - d) 0 else h) ∅
  for e in es do IO.println e
  let rmToS := {inputToRMp dat with S := es.toArray[0]!.1, growing := es, vs := es}
  let rmToS := getMinDists rmToS
  let vals := rmToS.filter fun ((p, _) : pos × pos) _ => p == initS.1 --rm.S.1
  let newMin := vals.fold (fun m _ v => min m v) vals.toArray[0]!.2
  IO.println s!"Shortest path: {vals.fold (fun m _ v => min m v) vals.toArray[0]!.2}"

  draw <| drawHash (rmToS.fold (fun h p n => h.insert (Prod.fst p) s!"{n % 10}") ({} : Std.HashMap pos String)) dat.size dat.size
  let target := (newMin + oldMin) / 2 --+ 1004
  let mids : Std.HashSet pos := rmToS.fold (fun h (p : pos × pos) v =>
    let sec := rmToE.getD (p.1, - p.2) 0
    if v + sec ≤ target + 500 then --target then
      --dbg_trace "{(v, sec)} = {v + sec} vs {target}  -- pos {p}"
      h.insert p.1
    else h) ∅
  IO.println s!"{mids.size}"
  draw <| drawSparse mids dat.size dat.size
