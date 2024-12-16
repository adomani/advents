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

/-- `test3` is the test string for the problem. -/
def test3 := "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"

/-- `atest3` is the test string for the problem, split into rows. -/
def atest3 := (test3.splitOn "\n").toArray

/--
`Boxes` encodes the state of the boxes while the robot moves around.
* `w` is the location of the walls `#`.
* `b` is the location of the boxes `O`.
* `S` is the current position `@`.
* `m` is what is left of the string of instructions.
* `old` is what has already been used of the string of instructions.
-/
structure Boxes where
  /-- `w` is the location of the walls `#`. -/
  w : Std.HashSet pos
  /-- `b` is the location of the boxes `O`. -/
  b : Std.HashSet pos
  /-- `S` is the current position `@`. -/
  S : pos
  /-- `m` is what is left of the string of instructions. -/
  m : String
  /-- `old` is what has already been used of the string of instructions. -/
  old : String
  deriving Inhabited

/-- Converts a state of `Boxes` into a `HashMap` that is easier to print. -/
def rev (b : Boxes) : Std.HashMap pos Char :=
  let t : Std.HashMap pos Char := (b.w.fold (fun h p => h.insert p '#') ∅)
  let t := (b.b.fold (fun h p => h.insert p 'O') t)
  t.insert b.S '@'

/-- Converts the input to a state of `Boxes`. -/
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

/-- Convert the symbol of a movement into the corresponding direction. -/
def toDir : String → pos
  | "^" => (- 1,   0)
  | ">" => (  0,   1)
  | "<" => (  0, - 1)
  | "v" => (  1,   0)
  | _ => (0, 0)

partial
def add (B : Boxes) (s : String) (init : pos) : pos :=
  let c := B.S + toDir s
  if B.w.contains c then init
  else
  if B.b.contains c then
    add {B with S := c} s init
  else
    c

def move (B : Boxes) : Boxes :=
  let mv := B.m.take 1
  let newPos := add B mv B.S
  let one := B.S + toDir mv
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

/-- The tally for the first part of the problem. -/
def GPS (B : Boxes) : Int :=
  B.b.fold (fun tot (x, y) => tot + 100 * x + y) 0

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let mut B := mkBoxes dat
  while !B.m.isEmpty do
      B := move B
  return (GPS B).natAbs

#assert part1 test == 10092
#assert part1 test2 == 2028

solve 1 1426855 file

/-!
#  Question 2
-/

/-- A `box` is just the position `l` of its left-most block. -/
structure box where
  l : pos
  deriving BEq, Hashable

/--
A utility to print `box`es: technically it is not needed, but it has been very useful for debugging.
-/
instance : ToString box where toString := fun {l := p} => s!"({p.1}, {p.2}-{p.2 + 1})"

/--
The positions contained in a `box`: the actual position mentioned in `box` and the square
to its right.
-/
abbrev nbs (b : box) : Array pos := #[b.l, b.l + (0, 1)]

/-- Doubles the width coordinate. -/
def doubleWidth (p : pos) : pos := (p.1, 2 * p.2)

/--
`Boxes2` is very similar to `Boxes`, except that it encodes a `HashSet` of `box`es, rather
than a `HashSet` of `pos`itions.
* `w` is the location of the walls `#`.
* `b` is the location of the boxes `O`.
* `S` is the current position `@`.
* `m` is what is left of the string of instructions.
* `old` is what has already been used of the string of instructions.
-/
structure Boxes2 where
  /-- `w` is the location of the walls `#`. -/
  w : Std.HashSet pos
  /-- `b` is the location of the boxes `O`. -/
  b : Std.HashSet box
  /-- `S` is the current position `@`. -/
  S : pos
  /-- `m` is what is left of the string of instructions. -/
  m : String
  /-- `old` is what has already been used of the string of instructions. -/
  old : String
  deriving Inhabited

def toBox (p : pos) : box := ⟨p⟩

def resize (b : Boxes) : Boxes2 where
  w := b.w.fold (fun h p => (h.insert (doubleWidth p)).insert (doubleWidth p + (0, 1))) {}
  b := b.b.fold (fun h p => h.insert ⟨doubleWidth p⟩) {}
  S := doubleWidth b.S
  m := b.m
  old := b.old

/--
`ContBoxes` is the structure to find `cont`iguous configurations of boxes.
* `walls` is the set of walls `#`.
* `boxes` is the set of boxes `O`.
* `front` is the set of fronts of the expansion: the positions that were just added.
* `growing` is the set of boxes accumulated so far.
-/
structure ContBoxes where
  /-- `walls` is the set of walls `#`. -/
  walls : Std.HashSet pos
  /-- `boxes` is the set of boxes `O`. -/
  boxes : Std.HashSet box
  /-- `front` is the set of fronts of the expansion: the positions that were just added. -/
  front : Std.HashSet pos
  /-- `growing` is the set of boxes accumulated so far. -/
  growing : Std.HashSet box
  deriving Inhabited

/--
Increases the contiguous boxes to the input `b` by one layer in the direction specified by
the string input `s`.
-/
def growBoxes (b : ContBoxes) (s : String) : ContBoxes × Bool :=
  let shift : Std.HashSet pos := b.front.fold (fun h p => h.insert (p + toDir s)) b.front
  --dbg_trace "first shift: {shift.toArray}"
  let shift := shift.filter (fun p => ! shift.contains (p + toDir s))
  --dbg_trace "shift: {shift.toArray}"
  if ! (shift.filter b.walls.contains).isEmpty
  then
    --dbg_trace "found a wall {(shift.filter b.walls.contains).toArray}"
    (default, false)
  else
  let metBoxes := b.boxes.filter (fun b => ((nbs b).map shift.contains).any id)
  let shift := metBoxes.fold (fun h b => h.insertMany (nbs b)) ({} : Std.HashSet pos)
  --dbg_trace "intermediate shift: {shift.toArray}"
  let shift := shift.filter (fun p => ! shift.contains (p + toDir s))
  --dbg_trace "no wall -- metBoxes: {metBoxes.toArray}\nagain shift: {shift.toArray}\n"
  ({b with
      growing := b.growing.union metBoxes
      front := if metBoxes.isEmpty then {} else shift
   }, true)

/--
Iteratively applies `growBoxes` until either no more boxes can be added or a wall is reached.
The `Bool`ean output tracks whether the move should be made or not.
-/
def adjacentBoxes (b : Boxes2) (s : String) : Std.HashSet box × Bool := Id.run do
  let mut (temp, continue?) : ContBoxes × Bool :=
    ({walls := b.w, boxes := b.b, front := {b.S}, growing := {}}, true)
  while ! temp.front.isEmpty do
    (temp, continue?) := growBoxes temp s
  return (temp.growing, continue?)

/-- Puts together `adjacentBoxes` and a string direction and updates the box configuration. -/
def moveBoxes (b : Boxes2) (s : String) : Boxes2 :=
  let (adj, move?) := adjacentBoxes b s
  --dbg_trace "adjacent: {adj.toArray}"
  if ! move? then
    --dbg_trace "do not move"
    b
  else
    --dbg_trace "move {adj.size} boxes"
    let erasedBoxes := adj.fold (·.erase ·) b.b
    let insertBoxes := adj.fold (fun h q => h.insert (toBox (q.l + toDir s))) erasedBoxes
    {b with b := insertBoxes, S := b.S + toDir s }

/-- Automates the string input to `moveBoxes`, by reading it off `b.m`. -/
def autoMove (b : Boxes2) : Boxes2 :=
  {moveBoxes b (b.m.take 1) with m := b.m.drop 1, old := b.old ++ b.m.take 1}

/-- The tally for the second part of the problem. -/
def GPS2 (B : Boxes2) : Int :=
  B.b.fold (fun tot {l := (x, y)} => tot + 100 * x + y) 0

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := Id.run do
  let mut B2 := resize <| mkBoxes dat
  while B2.m.length != 0 do
    B2 := autoMove B2
  GPS2 B2 |>.natAbs

#assert part2 test == 9021

--solve 2 1404917 file  -- takes approximately 3 minutes

end Day15
