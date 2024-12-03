import Advents.Utils
open Lean

namespace Day12

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day12.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

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

structure MazeState where
  maze      : Std.HashMap String (Array String)
  completed : Std.HashSet (Array String)
  growing   : Std.HashSet (Array String) := {#["start"]}

def isLower (s : String) : Bool :=
  s.toList.map (·.isLower) |>.all id

def addStep (m : MazeState) : MazeState := Id.run do
  let mut completed := m.completed
  let mut growing : Std.HashSet (Array String) := {}
  for g in m.growing do
    let tail := g.back!
    for n in m.maze.get! tail do
      if isLower n && g.contains n
      then
        continue
      if n == "end"
      then
        completed := completed.insert (g.push n)
      else
        growing := growing.insert (g.push n)
  return {m with completed := completed, growing := growing}

def mkMaze (dat : Array String) : Std.HashMap String (Array String) := Id.run do
  let mut mz := {}
  for d in dat do
    if let [s, t] := d.splitOn "-" then
      let s1 := mz.getD s #[]
      mz := mz.insert s (s1.push t)
      let t1 := mz.getD t #[]
      mz := mz.insert t (t1.push s)
  return mz

#eval do
  let dat := atest2
  let dat := atest
  let dat := atest3
  let dat ← IO.FS.lines input
  let mut mz : MazeState := {maze := mkMaze dat, completed := {}}
  let mut con := 0
  while !mz.growing.isEmpty do
    mz := addStep mz
    con := con + 1
  --IO.println s!"{mz.maze.toList}"
  --IO.println s!"{mz.growing.toList}"
  IO.println s!"\nIn {con} steps, I found {mz.completed.size} paths\n\n{mz.completed.toList}"

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

end Day12
