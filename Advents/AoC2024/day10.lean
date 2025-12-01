import Advents.Utils
open Std

namespace Day10

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2024"/"day10" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "0123
1234
8765
9876"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `test2` is the test string for the problem. -/
def test2 := "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"

/-- `atest2` is the test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/--
The exploration state of the tails.
* `grid` is the `HashMap` deduced from the input, with a height for each map location.
* `paths` is the current `HashSet` of paths.

A path is an array of positions:
* for part 1, it is simply the start and end;
* for part 2, it is the full array of visited locations.
-/
structure TrailState where
  /-- `grid` is the `HashMap` deduced from the input, with a height for each map location. -/
  grid : HashMap pos Nat
  /-- `paths` is the current `HashSet` of paths. -/
  paths : HashSet (Array pos)

/--
Reads the input `i` and creates the initial `TrailState`, loading the map and placing
a path of length 1 at each `0` location.
-/
def mkTrailState (i : Array String) : TrailState where
  grid := loadGridNats i
  paths := (sparseGrid i (· == '0')).fold (·.insert #[·]) {}

/--
Updates a `TrailState`, by expanding each path with one extra step aiming for height `i + 1`.
If the `all?` flag is `true`, then we record the full path;
if the `all?` flag is `false`, then we record just the beginning and the lastest location.
-/
def move (all? : Bool) (t : TrailState) (i : Nat) : TrailState := Id.run do
  let mut mvs := {}
  for pth in t.paths do
    for d in #[(1, 0), (- 1, 0), (0, 1), (0, - 1)] do
      let new := pth.back! + d
      match t.grid.get? new with
        | none => continue
        | some n =>
          if n != i + 1 then continue
          mvs := mvs.insert <| (if all? then pth else (pth.take 1)).push new
  return {t with paths := mvs}

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  (List.foldl (move false) (mkTrailState dat) (.range 9)).paths.size

#assert part1 atest2 == 36

solve 1 510

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  (List.foldl (move true) (mkTrailState dat) (.range 9)).paths.size

#assert part2 atest2 == 81

solve 2 1058

end Day10
