import Advents.Utils
open Lean

namespace Day13

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day13.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray


def getGrid (inp : Array String) : Std.HashSet pos := Id.run do
  let mut h := {}
  for l in inp do
    if let [x, y] := l.getInts then
      h := h.insert (x, y)
  return h

def foldX (n : Int) (g : Std.HashSet pos) : Std.HashSet pos := Id.run do
  let mut h := {}
  for p in g do
    h := h.insert (if p.1 < n then p else (2 * n - p.1, p.2))
  return h

def foldY (n : Int) (g : Std.HashSet pos) : Std.HashSet pos := Id.run do
  let mut h := {}
  for p in g do
    h := h.insert (if p.2 < n then p else (p.1, 2 * n - p.2))
  return h

/-- A function to draw the `state` of the `OctoState` -- just needed for pretty pictures, no actual content. -/
def drawHash (h : Std.HashSet pos) (Nx Ny : Nat) : Array String := Id.run do
  let mut fin := #[]
  for i in [0:Nx] do
    let mut str := ""
    for j in [0:Ny] do
      match h.get? (i, j) with
        | some d => str := str ++ "#"
        | none => str := str.push '·'
    fin := fin.push str
  return fin


#eval
  let dat := atest
  let grid := getGrid dat
  let fx := foldY 7 grid
  let fx := foldX 5 fx
  dbg_trace fx.size
  draws <| drawHash fx 10 20
  --dat

#eval do
  let mut val := (655, 447)
  IO.println val
  for _ in (List.range 6) do
    val := (val.1 / 2, val.2 / 2)
    IO.println val



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

end Day13
