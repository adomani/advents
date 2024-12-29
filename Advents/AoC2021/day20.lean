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

/--
An `Image` is a structure containing
* `iea`, the `i`mage `e`nhancing `a`lgorithm -- a string of length 512;
* `light` a `HashSet` of positions that are lit (i.e., labeled as `#`).
-/
structure Image where
  /-- `iea` is the `i`mage `e`nhancing `a`lgorithm -- a string of length 512. -/
  iea : String
  /-- `light` is a `HashSet` of positions that are lit (i.e., labeled as `#`). -/
  light : Std.HashSet pos
  deriving Inhabited

/-- Converts the string input to an `Image`. -/
def inputToImage (dat : String) : Image :=
  match dat.splitOn "\n\n" with
    | [iea, im] =>
      { iea := iea.replace "\n" ""
        light := sparseGrid (im.splitOn "\n").toArray (· == '#') }
    | _ => panic "Malformed input!"

/--
A utility function to draw an `Image`. Useful for developing the solution, not so much after that.
-/
def showImage (i : Image) : IO Unit := do
  let ((Mx, My), (mx, my)) := i.light.fold (init := ((0, 0), (1000, 1000)))
    fun ((Mx, My), (mx, my)) (x, y) => ((max Mx x, max My y), (min mx x, min my y))
  draw <| drawSparse (i.light.fold (init := ∅) (fun h p => h.insert (p - (mx, my)))) (Mx + 1 - mx).natAbs (My + 1 - my).natAbs
  IO.println i.iea

/--
info: --01234-
0|#··#·|
1|#····|
2|##··#|
3|··#··|
4|··###|
--01234-

..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
---
warning: unused variable `dat`
note: this linter can be disabled with `set_option linter.unusedVariables false`
-/
#guard_msgs in
#eval do
  let dat ← IO.FS.readFile input
  let dat := test
  let i := inputToImage dat
  showImage i

/-- The neighbours of `(0, 0)`: the square `[-1, 0, 1] × [-1, 0, 1]`. -/
def nbs : Array pos := (Array.range 9).foldl (fun h i => h.push (i.cast / 3 - 1, i.cast % 3 - 1)) ∅

/-- Performs the checks of which neighbours of `p` are lit (i.e., are equal to `#`) in `i`. -/
def checkNbs (i : Image) (p : pos) : Array Bool := nbs.foldl (·.push <| i.light.contains <| p + ·) ∅

#assert checkNbs (inputToImage test) (2, 2) ==
        #[false, false, false, true, false, false, false, true, false]

/-- Converts an array of `Bool`eans to a natural number. -/
def binToNat (as : Array Bool) : Nat :=
  Array.sum <| as.reverse.zipWith (Array.range as.size) fun a n => 2 ^ n * if a then 1 else 0

#assert binToNat #[false, false, false, true, false, false, false, true, false] == 34

/-- Reads the new character that the enhancement of the `Image` `i` places at `p`. -/
def newChar (i : Image) (p : pos) : Char :=
  i.iea.get ⟨binToNat <| checkNbs i p⟩

/-- `enhance i sz shift` takes as input an `Image` `i`, the size `sz` of the grid and a
natural number `shift`.

It computes the enhancement of `i` in the positions in the range
`[- shift, sz + shift] × [- shift, sz + shift]`.
-/
def enhance (i : Image) (sz shift : Nat) : Image :=
  {i with
    light := Id.run do
      let mut newImage : Std.HashSet pos := ∅
      for x in [0:sz + 2 * shift + 1] do
        for y in [0:sz + 2 * shift + 1] do
          let p : pos := (x-shift, y-shift)
          if newChar i p == '#' then
            newImage := newImage.insert p
      return newImage}

/--
The common loop for the two parts.
On odd iterations, all but finitely many lights are on, so we compute the effect of enhancing twice
and iterate that.

This has the effect of creating an impurity at `(- 3, - 3)`, that we erase.

Moreover, the whole picture drifts by `(- 2, - 2)`, so we also translate back by that amount.
-/
def parts (dat : String) (reps : Nat) : Nat := Id.run do
  let mut i := inputToImage dat
  let (szx, szy) : pos := i.light.fold (init := (0, 0)) fun (mx, my) (x, y) => (max mx x, max my y)
  let mut sz := (max szx szy).natAbs + 1
  for _ in [0:reps / 2] do
    i := enhance (enhance i sz 3) sz 3
    sz := sz + 4
    i :=  {i with light := i.light.erase (- 3, - 3)}
    i :=  {i with light := i.light.fold (init := ∅) (·.insert <| · + (2, 2))}
  i.light.size

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := parts dat 2

#assert part1 test == 35

set_option trace.profiler true in solve 1 5486 file  --  takes approximately 1.5s

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := parts dat 50

#assert part2 test == 3351  --  takes approximately 11s

--set_option trace.profiler true in solve 2 20210 file  --  takes approximately 65s

end Day20
