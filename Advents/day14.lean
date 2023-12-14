import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day14.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- Transpose an array of strings. -/
def Array.transpose (s : Array String) : Array String :=
  let rows := s.map (List.toArray ∘ String.toList)
  let cols := rows[0]!.size
  Id.run do
    let mut ans := #[]
    for c in [:cols] do
      let mut row := ""
      for r in [:rows.size] do
        row := row.push (rows[r]!.getD c default)
      ans := ans.push row
    return ans

-- A `pos`ition is a pair of integers. -/
abbrev pos := Int × Int

def parseColAux : Nat → List Char → Array (Bool × Nat)
  | n, 'O'::rs => (parseColAux (n + 1) rs).push (false, n + 1)
  | n, '#'::rs => (parseColAux (n + 1) rs).push (true, n + 1)
  | n, _::rs => (parseColAux (n + 1) rs)
  | _, [] => default

#assert parseColAux 0 "..#O.O.#.O".toList ==
  #[(false, 10), (true, 8), (false, 6), (false, 4), (true, 3)]

def parseCol (s : String) : Array (Bool × Nat) :=
  parseColAux 0 s.toList

def getRanges (dat : Array (Bool × Nat)) : Array (Nat × Nat) :=
  Id.run do
    let mut ranges := #[]
    let mut fix := 0
    let mut current := 0
    for xi in [:dat.size] do
      let x := dat[xi]!
      if x.1 then
        ranges := ranges.push (current, fix)
        fix := xi
        current := 0
      else
        current := current + 1
    return ranges.push (current, fix)

def getInfo (s : String) : List (Nat × Nat) :=
  let pc := s.toList
  let lth := pc.length
  let breaks := lth :: (pc.findIdxs (· == '#')).map (lth - 1 - ·)
  let moving := ((pc.filter (! · == '.')).splitOn '#').map List.length
  moving.zip breaks

def getLoad (dat : List (Nat × Nat)) : Nat :=
  Id.run do
  let mut tot := 0
  for (wt, pos) in dat do
    tot := tot + ((List.range wt).map (pos - ·)).sum
  return tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let infos := dat.transpose.map getInfo
  (infos.map getLoad).sum

#assert part1 atest == 136

solve 1 110821

/-!
#  Question 2
-/

#exit

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
