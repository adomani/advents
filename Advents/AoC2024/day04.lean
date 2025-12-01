import Advents.Utils
open Std

namespace Day04

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2024"/"day04" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- The scalar multiplication between an integer and pair of integers. -/
instance : HMul Int pos pos where
  hMul a p := (a * p.1, a * p.2)

/--
Search the word `wd`, starting from the position `p` and continuing in all possible directions.
-/
def findWord (h : HashMap pos Char) (p : pos) (wd : String := "XMAS") : Nat := Id.run do
  let mut ct := 0
  let mut cond := true
  let mut k := 0
  if some (String.Pos.Raw.get wd 0) != h.get? p then return 0
  for i in [0, 1, -1] do
    for j in [0, 1, -1] do
      if (i, j) == (0, 0) then continue
      cond := true
      k := 0
      while cond && k < wd.length do
        cond := some (String.Pos.Raw.get wd ⟨k⟩) == h.get? (p + (k : Int) * ((i, j) : pos))
        k := k + 1
      if cond then ct := ct + 1
  return ct

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let grid := loadGrid dat id
  grid.fold (init := 0) fun s p _ => s + findWord grid p

#assert part1 atest == 18

solve 1 2397

/-!
#  Question 2
-/

/-- Rotate a position by 90⁰. -/
def rot (p : pos) : pos := (p.2, - p.1)

/--
Search the `X` of `MAS`, starting with an `A` in position `p` with each of the possible
4 orientations.
-/
def findX (h : HashMap pos Char) (p : pos) : Nat :=
  if some 'A' != h.get? p then 0 else
  [(1, 0), (-1, 0), (0, 1), (0, -1)].foldl (init := 0) fun ct i =>
    let j := (- 1) * i
    if some 'M' == h.get? (p + (i +         rot i)) &&
       some 'M' == h.get? (p + (i + (- 1) * rot i)) &&
       some 'S' == h.get? (p + (j +         rot i)) &&
       some 'S' == h.get? (p + (j + (- 1) * rot i))
    then
      ct + 1
    else
      ct

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let grid := loadGrid dat id
  grid.fold (init := 0) fun s p _ => s + findX grid p

#assert part2 atest == 9

solve 2 1824

end Day04
