import Advents.Utils
open Std

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

/--
`ReindeerMap` is the main state for the puzzle.
* `tiles` is the `HashSet` of tiles that the Reindeer can take -- the entries labeled with
  `.`, `S` or `E`.
* `S` is the pair consisting of the starting position and direction of the Reindeer.
* `growing` is `HashMap` recording which tiles have already been visited and what is their
  relative distance along the chosen path.
  This information is used to update `visited`, which tracks the minima of these values.
* `visited` is the current minimum score to go from `S` to the given tile:
  this value is updated dynamically, as `growing` grows.
-/
structure ReindeerMap where
  /-- `tiles` is the `HashSet` of tiles that the Reindeer can take -- the entries labeled with
  `.`, `S` or `E`. -/
  tiles : HashSet pos
  /-- `S` is the pair consisting of the starting position and direction of the Reindeer. -/
  S : pos × pos
  /-- `growing` is `HashMap` recording which tiles have already been visited and what is their
  relative distance along the chosen path.
  This information is used to update `visited`, which tracks the minima of these values. -/
  growing : HashMap (pos × pos) Nat := {(S, 0)}
  /-- `visited` is the current minimum score to go from `S` to the given tile:
  this value is updated dynamically, as `growing` grows. -/
  visited : HashMap (pos × pos) Nat := {(S, 0)}

/-- Converts the input data into the corresponding `ReindeerMap`. -/
def inputToRMp (s : Array String) : ReindeerMap :=
  let init := sparseGrid s (· == 'S')
  let S := (init.toArray[0]!, (0, 1))
  { tiles := (sparseGrid s (".SE".contains ·)).insert init.toArray[0]!
    S := S }

/-- We use this instance to reverse directions. -/
instance : Neg pos where neg p := (- p.1, - p.2)

/--
The main function to update `ReindeerMap.visiting`: checks whether the new value is strictly
smaller than what is already stored and, if so, returns `some updatedHashMap`.
Otherwise, it returns `none`.
-/
def update (v : HashMap (pos × pos) Nat) (p d : pos) (val : Nat) :
    Option (HashMap (pos × pos) Nat) :=
  match v.get? (p, d) with
    | none => some (v.insert (p, d) val)
    | some oldVal => if oldVal ≤ val then none else some (v.insert (p, d) val)

/-- A 90⁰ rotation: useful for moving around the maze. -/
def rot (p : pos) : pos := (p.2, - p.1)

/--
Produces the updated `ReindeerMap`, by moving each entry in `growing` either one step in the
direction in which it is currently facing, or by rotating it 90⁰ clockwise or counterclockwise.
Each movement is then used to update `ReindeerMap.visited` and `ReindeerMap.growing`,
as appropriate.
-/
def increase (rm : ReindeerMap) : ReindeerMap := Id.run do
  let mut (grow, vis) := (rm.growing, rm.visited)
  for ((p, d), val) in rm.growing do
    if rm.tiles.contains (p + rot d) then
      match update vis p (rot d) (val + 1000) with
        | none => grow := grow
        | some v =>
          vis := v
          grow := grow.insert (p, rot d) (val + 1000)
    if rm.tiles.contains (p - rot d) then
      match update vis p (- rot d) (val + 1000) with
        | none => grow := grow
        | some v =>
          vis := v
          grow := grow.insert (p, - rot d) (val + 1000)
    let newP := p + d
    if rm.tiles.contains newP then
      match update vis newP d (val + 1) with
        | none => grow := grow
        | some v =>
          vis := v
          grow := grow.insert (newP, d) (val + 1)
  return { rm with
    growing := grow.filter fun p _v => (! rm.growing.contains p)
    visited := vis
    }

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
def getMinDists (rm : ReindeerMap) (tgt : pos) :
    HashMap (pos × pos) Nat × HashMap (pos × pos) Nat × Nat := Id.run do
  let mut rm := rm
  let mut oldGrow : HashMap (pos × pos) Nat := ∅
  while (oldGrow.toArray != rm.growing.toArray) do
    oldGrow := rm.growing
    rm := increase rm
  let vals := rm.visited.filter fun (p, _) _ => p == tgt
  let minValue := vals.fold (fun m _ v => min m v) vals.toArray[0]!.2
  let reverseStart := vals.fold (init := ∅)
    fun h p v => if v == minValue then h.insert (p.1, -p.2) 0 else h
  return (rm.visited, reverseStart, minValue)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let E := sparseGrid dat (· == 'E') |>.toArray[0]!
  let (_, _, val) := getMinDists (inputToRMp dat) E
  val

#assert part1 atest1 == 7036
#assert part1 atest2 == 11048

--set_option trace.profiler true in solve 1 99460 -- takes approximately 20s

/-!
#  Question 2
-/

/-- Returns all the tiles through which there is a paths of minimum score. -/
def getMinPaths (rm : ReindeerMap) (tgt : pos) : HashSet pos :=
  let (rmToE, es, oldMin) := getMinDists rm tgt
  let (rmToS, _, newMin) := getMinDists {rm with growing := es, visited := es} rm.S.1
  let target := (newMin + oldMin) / 2 + 500 -- add 500 to average the extra rotation, I think!
  rmToS.fold (fun h p v =>
    let sec := rmToE.getD (p.1, - p.2) oldMin
    if v + sec ≤ target then h.insert p.1 else h) ∅

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let E := sparseGrid dat (· == 'E') |>.toArray[0]!
  (getMinPaths (inputToRMp dat) E).size

#assert part2 atest1 == 45
#assert part2 atest2 == 64

--set_option trace.profiler true in solve 2 500 -- takes approximately 25s

/-- Produces a picture of the tiles on some path of minimum score. -/
def drawMinPaths (dat : Array String) : IO Unit := do
  let E := sparseGrid dat (· == 'E') |>.toArray[0]!
  let mids := getMinPaths (inputToRMp dat) E
  draw <| drawSparse mids dat.size dat.size

/-
#eval do
  let dat ← IO.FS.lines input
  let dat := atest2 -- 11048
  let dat := atest1 -- 7036
  drawMinPaths dat
-/

end Day16
