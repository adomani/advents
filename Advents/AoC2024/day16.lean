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

def dirsAt (rm : RM) (p : pos) : Array pos := nbs.filter fun d => ! (rm.gr.contains (p + d))

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

def moveCost (rm : RM) : Option (Nat × pos) := Id.run do
  let mut (p, d) := rm.S
  let mut tot := 0
  let ds := dirs rm
  let mut con := 0
  while ds.size ≤ 2 && con ≤ 5 do
    con := con + 1
    if ds.size == 1 then return none
    let dnew := (ds.filter fun new => new != (0, 0) - d)[0]!
    tot := tot + if dnew == d then 1 else 1000
    d := dnew
    p := p + d
    dbg_trace "{(p, d)}"
  return some (tot, p)

#eval do
  let dat := atest1
  let rm := inputToRM dat
  drawRM rm
  IO.println <| moveCost rm


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
