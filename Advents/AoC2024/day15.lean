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

structure box where
  l : pos
  deriving BEq, Hashable

instance : ToString box where toString := fun {l := p} => s!"({p.1}, {p.2}-{p.2 + 1})"

def nbs (b : box) : Array pos := #[b.l, b.l + (0, 1)]

instance : HMul Nat pos pos where hMul a p := (p.1, a * p.2)

structure Boxes2 where
  w : Std.HashSet pos
  b : Std.HashSet box
  S : pos
  m : String
  old : String
  deriving Inhabited

def resize (b : Boxes) : Boxes2 where
  w := b.w.fold (fun h p => (h.insert (2 * p)).insert (2 * p + (0, 1))) {}
  b := b.b.fold (fun h p => h.insert ⟨2 * p⟩) {}
  S := 2 * b.S
  m := b.m
  old := b.old

def toBoxes (b : Boxes2) : Boxes where
  w := b.w
  b := b.b.fold (fun h ⟨p⟩ => (h.insert p).insert (p + (0, 1))) {}
  S := b.S
  m := b.m
  old := b.old

def touchingBoxes (b : Boxes2) (s : String) (bs : Std.HashSet box) : Std.HashSet box :=
  let f : Std.HashSet pos :=
    bs.fold (·.insertMany <| Std.HashSet.ofArray <| nbs ·) {}

  let shift : Std.HashSet pos := f.fold (fun h p => h.insert (addOne s p)) {}
  if ! (shift.filter b.w.contains).isEmpty then bs --shift
  else
  let metBoxes := b.b.filter (fun b => ((nbs b).map shift.contains).any id)
  bs.union metBoxes
  --shift.union (metBoxes.fold (·.insertMany <| Std.HashSet.ofArray <| nbs ·) {})

/-- info: #[(3, 4-5), (4, 5-6), (3, 6-7)] -/
#guard_msgs in
#eval do
  let dat := test
  let B2 := resize <| mkBoxes dat
  let B2 := {B2 with S := B2.S + (1, -2), b := (B2.b.erase ⟨(4, 6)⟩).insert ⟨(4, 5)⟩}
  let tb := touchingBoxes B2 "^" {({l := (4, 5)} : box)}
  let tb := touchingBoxes B2 "^" tb
  IO.println (touchingBoxes B2 "^" tb).toArray

#eval do
  let dat ← IO.FS.readFile input
  let dat := test2
  let dat := test
  let sz := ((dat.splitOn "\n\n")[0]!.splitOn "\n").length

  let B2 := resize <| mkBoxes dat
  let B2 := {B2 with S := B2.S + (1, -2), b := (B2.b.erase ⟨(4, 6)⟩).insert ⟨(4, 5)⟩}
  let mut B := toBoxes <| B2
  --B := {B with b := B.b.erase (4, 6) }
  draw <| drawHash (rev B) sz (2 * sz)
  let tb := touchingBoxes B2 "^" {({l := (4, 5)} : box)}
  IO.println (touchingBoxes B2 "^" tb).toArray

def shiftFront (b : Boxes2) (s : String) (f : Std.HashSet pos) : Std.HashSet pos :=
  let shift := f.fold (fun h p => h.insert (addOne s p)) {}
  if (shift.filter b.w.contains).isEmpty then shift
  else
  let metBoxes := b.b.filter (fun b => ((nbs b).map shift.contains).any id)
  shift.union (metBoxes.fold (·.insertMany <| Std.HashSet.ofArray <| nbs ·) {})

def checkWall (b : Boxes2) (s : String) (f : Std.HashSet pos) : Bool :=
  f.fold (fun h p => _) false

def nbs (b : Boxes2) (s : String) : Std.HashSet box := Id.run do
  let mut h := b.b
  let mut front : Std.HashSet pos := {b.S}


  return h

#eval do
  let dat ← IO.FS.readFile input
  let dat := test2
  let dat := test
  let sz := ((dat.splitOn "\n\n")[0]!.splitOn "\n").length

  let mut B := toBoxes <| resize <| mkBoxes dat
  draw <| drawHash (rev B) sz (2 * sz)
  --IO.println B.m
  while !B.m.isEmpty do
      --IO.println s!"\nMove: {B.m.take 1}, Current: {B.S}, found pos: {add B (B.m.take 1) B.S}"
      B := move B
  draw <| drawHash (rev B) sz sz
  --IO.println s!"'{B.m}' '{B.old}'"
  IO.println <| GPS B

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day15
