import Advents.Utils
open Lean

namespace Day15

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day15.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `test2` is the test string for the problem. -/
def test2 := "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"

/-- `atest2` is the test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

structure Boxes where
  w : Std.HashSet pos
  b : Std.HashSet pos
  S : pos
  m : String
  old : String
  deriving Inhabited

def rev (b : Boxes) : Std.HashMap pos Char :=
  let t : Std.HashMap pos Char := (b.w.fold (fun h p => h.insert p '#') ∅)
  let t := (b.b.fold (fun h p => h.insert p 'O') t)
  t.insert b.S '@'

instance : ToString Boxes where
  toString b :=
    "\n".intercalate <| (drawHash (rev b) 20 20).toList ++ ["", b.m, b.old, ""]

def mkBoxes (s : String) : Boxes :=
  if let [p, ms] := s.splitOn "\n\n"
  then
    let ap := (p.splitOn "\n").toArray
    { w := sparseGrid ap (· == '#')
      b := sparseGrid ap (· == 'O')
      S := sparseGrid ap (· == '@') |>.toArray[0]!
      m := ms.replace "\n" ""
      old := "" }
  else
    panic "There should be two parts to the input!"

#eval do
  let dat := test2; let sz := 8
  let B := mkBoxes dat
  draw <| drawHash (rev B) sz sz

def addOne : String → pos → pos
  | "^", p => p + (- 1,   0)
  | ">", p => p + (  0,   1)
  | "<", p => p + (  0, - 1)
  | "v", p => p + (  1,   0)
  | _, p => p

partial
def add (B : Boxes) (s : String) (init : pos) : pos :=
  let c := addOne s B.S
  if B.w.contains c then init
  else
  if B.b.contains c then
    add {B with S := c} s init
  else
    c

#eval do
  let dat := test2; let sz := 8
  let B := mkBoxes dat
  draw <| drawHash (rev B) sz sz
  IO.println B.m
  IO.println <| add B (B.m.take 1) B.S

def move (B : Boxes) : Boxes :=
  let mv := B.m.take 1
  let newPos := add B mv B.S
  let one := addOne mv B.S
  let box? := B.b.contains one
  if newPos == B.S then {B with m := B.m.drop 1, old := B.old ++ mv} else
  {B with
    b := if box? then
        (B.b.erase one).insert newPos
      else B.b
    S := one
    m := B.m.drop 1
    old := B.old ++ mv
    }

def GPS (B : Boxes) : Int :=
  B.b.fold (fun tot (x, y) => tot + 100 * x + y) 0

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let mut B := mkBoxes dat
  while !B.m.isEmpty do
      B := move B
  return (GPS B).natAbs

#assert part1 test == 10092

solve 1 1426855 file

/-!
#  Question 2
-/

#eval do
  let dat := test2
  let dat := test
  let dat ← IO.FS.readFile input
  let sz := ((dat.splitOn "\n\n")[0]!.splitOn "\n").length
  let mut B := mkBoxes dat
  draw <| drawHash (rev B) sz sz
  IO.println B.m
  while !B.m.isEmpty do
      --IO.println s!"\nMove: {B.m.take 1}, Current: {B.S}, found pos: {add B (B.m.take 1) B.S}"
      B := move B
  draw <| drawHash (rev B) sz sz
  IO.println s!"'{B.m}' '{B.old}'"
  IO.println <| GPS B

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day15
