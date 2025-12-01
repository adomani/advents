import Advents.Utils
open Std

namespace Day12

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2021"/"day12" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "start-A
start-b
A-c
A-b
b-d
A-end
b-end"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `test2` is the test string for the problem. -/
def test2 := "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"

/-- `atest2` is the test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/-- `test3` is the test string for the problem. -/
def test3 := "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"

/-- `atest3` is the test string for the problem, split into rows. -/
def atest3 := (test3.splitOn "\n").toArray

/-- The state of the maze, as we are exploring it.
* `maze` assigns to each cave the adjacent caves.
* `completed` is a count of how many paths we already found from `start` to `end`.
* `growing` is the collection of partial paths beginning from `start`,
  together with a boolean that is `true` if and only if we already repeated a small cave
  (this is only relevant for part 2).
-/
structure MazeState where
  /-- `maze` assigns to each cave the adjacent caves. -/
  maze      : HashMap String (Array String)
  /-- `completed` is a count of how many paths we already found from `start` to `end`. -/
  completed : Nat
  /-- `growing` is the collection of partial paths beginning from `start`,
  together with a boolean that is `true` if and only if we already repeated a small cave
  (this is only relevant for part 2). -/
  growing   : HashSet (Array String × Bool) := {(#["start"], false)}

/-- Checks if a string consists entirely of lower-case characters. -/
def isLower (s : String) : Bool :=
  s.toList.map (·.isLower) |>.all id

/--
`addStep` replaces the `MazeState` with the one that is obtained from the input
by extending each partial path in `growing` by one step in all possible
directions and updating everything else accordingly.
-/
def addStep (m : MazeState) : MazeState := Id.run do
  let mut completed := m.completed
  let mut growing : HashSet (Array String × Bool) := {}
  for (g, dup) in m.growing do
    let tail := g.back!
    for n in m.maze.get! tail do
      if isLower n && g.contains n
      then
        continue
      if n == "end"
      then
        completed := completed + 1
      else
        growing := growing.insert (g.push n, dup)
  return {m with completed := completed, growing := growing}

/-- Produces the "adjacency `HashMap`" for the cave from the puzzle input. -/
def mkMaze (dat : Array String) : HashMap String (Array String) :=
  dat.foldl (init := {}) fun mz d =>
    if let [s, t] := d.splitOn "-" then
      let s1 := mz.getD s #[]
      let t1 := mz.getD t #[]
      mz.insert s (s1.push t) |>.insert t (t1.push s)
    else mz

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let mut mz : MazeState := {maze := mkMaze dat, completed := 0}
  while !mz.growing.isEmpty do
    mz := addStep mz
  mz.completed

#assert part1 atest  == 10
#assert part1 atest2 == 19
#assert part1 atest3 == 226

solve 1 5457

/-!
#  Question 2
-/

/-- Similar to `addStep`: explore the input `MazeState` by extending the current paths one
further step, this time allowing a small cave to be visited at most once. -/
def addStepOneRep (m : MazeState) : MazeState := Id.run do
  let mut completed := m.completed
  let mut growing : HashSet (Array String × Bool) := {}
  for (g, dup) in m.growing do
    let tail := g.back!
    for n in m.maze.get! tail do
      if n == "end"
      then
        completed := completed + 1
      else if 2 ≤ g.size && n == "start"
      then
        continue
      else
        let preCond := isLower n && g.contains n
        if preCond && dup
        then
          continue
        else
          growing := growing.insert (g.push n, preCond || dup)
  return {m with completed := completed, growing := growing}

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let mut mz : MazeState := {maze := mkMaze dat, completed := 0}
  while !mz.growing.isEmpty do
    mz := addStepOneRep mz
  mz.completed

#assert part2 atest  == 36
#assert part2 atest2 == 103
#assert part2 atest3 == 3509

solve 2 128506

end Day12
