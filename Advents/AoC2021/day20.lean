import Advents.Utils
open Lean

namespace Day20

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day20.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"

structure Image where
  iea : String
  light : Std.HashSet pos
  deriving Inhabited

def inputToImage (dat : String) : Image :=
  match dat.splitOn "\n\n" with
    | [iea, im] =>
      { iea := iea.replace "\n" ""
        light := sparseGrid (im.splitOn "\n").toArray (· == '#') }
    | _ => panic "Malformed input!"

def showImage (i : Image) : IO Unit := do
  let ((Mx, My), (mx, my)) := i.light.fold (init := ((0, 0), (1000, 1000)))
    fun ((Mx, My), (mx, my)) (x, y) => ((max Mx x, max My y), (min mx x, min my y))
  draw <| drawSparse (i.light.fold (init := ∅) (fun h p => h.insert (p - (mx, my)))) (Mx + 1 - mx).natAbs (My + 1 - my).natAbs
  IO.println i.iea

#eval do
  let dat ← IO.FS.readFile input
  let dat := test
  let i := inputToImage dat
  showImage i

def nbs : Array pos := (Array.range 9).foldl (fun h i => h.push (i.cast / 3 - 1, i.cast % 3 - 1)) ∅

#eval nbs

def checkNbs (i : Image) (p : pos) : Array Bool := nbs.foldl (·.push <| i.light.contains <| p + ·) ∅

#assert checkNbs (inputToImage test) (2, 2) ==
        #[false, false, false, true, false, false, false, true, false]

def binToNat (as : Array Bool) : Nat :=
  Array.sum <| as.reverse.zipWith (Array.range as.size) fun a n => 2 ^ n * if a then 1 else 0

#assert binToNat #[false, false, false, true, false, false, false, true, false] == 34

def newChar (i : Image) (p : pos) : Char :=
  let newCount : Nat := binToNat <| checkNbs i p
  i.iea.get ⟨newCount⟩

def enhance (i : Image) (sz shift : Nat) : Image :=
  {i with
    light := Id.run do
      let mut newImage : Std.HashSet pos := ∅
      for x in [0:sz + 2 * shift + 1] do
        for y in [0:sz + 2 * shift + 1] do
          let p : pos := (x-shift, y-shift)
          --p in i.light do
          if newChar i p == '#' then
            newImage := newImage.insert p
      return newImage}

def parts (dat : String) (reps : Nat) : Nat := Id.run do
  let mut i := inputToImage dat
  for _ in [0:reps / 2] do
    let (szx, szy) : pos := i.light.fold (init := (0, 0)) fun (mx, my) (x, y) => (max mx x, max my y)
    let sz := (max szx szy).natAbs + 1
    i := enhance (enhance i sz 3) sz 3
    let mins := i.light.fold (init := (0, 0)) fun (mx, my) (x, y) => (min mx x, min my y)
    i :=  {i with light := i.light.erase mins}
    let mins := i.light.fold (init := (0, 0)) fun (mx, my) (x, y) => (min mx x, min my y)
    i :=  {i with light := i.light.fold (init := ∅) (·.insert <| · - mins)}
  i.light.size


/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
--def part1 (dat : Array String) : Nat := sorry
def part1 (dat : String) : Nat := parts dat 2

#assert part1 test == 35

set_option trace.profiler true in solve 1 5486 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := parts dat 50

#assert part2 test == 3351

set_option trace.profiler true in solve 2 20210 file
#exit
#eval do
  let dat := test
  let dat ← IO.FS.readFile input
  let mut i := inputToImage dat
  let mut mins : pos := (0, 0)
  --showImage (enhance i 100 2)
  for j in [0:25] do
    --let j' := j / 2 + 3
    --i := (enhance (enhance i (100 + 2 * j') (2 * j' + 3)) (100 + 2 * j') (2 * j' + 3))

    let (szx, szy) : pos := i.light.fold (init := (0, 0)) fun (mx, my) (x, y) => (max mx x, max my y)
    let sz := (max szx szy).natAbs + 1
    i := enhance (enhance i sz 3) sz 3
    mins := i.light.fold (init := (0, 0)) fun (mx, my) (x, y) => (min mx x, min my y)
    i :=  {i with light := i.light.erase mins}
    mins := i.light.fold (init := (0, 0)) fun (mx, my) (x, y) => (min mx x, min my y)
    i :=  {i with light := i.light.fold (init := ∅) (·.insert <| · - mins)}
  IO.println i.light.size
  IO.println mins
  showImage i --.light.erase (mx, my) |>.size

    --i := {i with light := i.light.erase (-3, -3)}
  --let mut newImage : Std.HashSet pos := ∅
  --for p in i.light do
  --  if newChar i p == '#' then newImage := newImage.insert p
  --IO.println (mx, my)
  --IO.println i2.light.toArray

  --showImage i

/-!
-/
-- 5487 -- too high

-- 5713

end Day20
