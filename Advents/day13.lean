import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day13.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"

def nums (s : Array String) : IO Unit := do
  for i in [:s.size] do
    IO.println s!"{i}|{s[i]!}|"
  IO.println ""


#eval do
  IO.println ((← IO.FS.lines input).size, (← IO.FS.lines input)[0]!.length)

def getPats (s : String) : Array String :=
  (s.splitOn "\n\n").toArray

def transpose (s : Array String) : Array String :=
  let rows := s.map (List.toArray ∘ String.toList)
  let cols := rows[0]!.size
  Id.run do
    let mut ans := #[]
    for c in [:cols] do
      let mut row := ""
      for r in [:rows.size] do
        row := row.push rows[r]![c]!
      ans := ans.push row
    return ans

#eval do
  let ts := getPats test
  let ca := ts[1]!
  let car := (ca.splitOn "\n").toArray
  nums <| car
  nums <| transpose car

/-- Decide whether a horizontal position is a line of symmetry for the array. -/
def isNsymm (s : Array String) (n : Nat) : Bool :=
  let rloc := min n (s.size - n)
--  dbg_trace s!"line between = {(n-1,n)}, range = {rloc}"
  if rloc = 0 then false else
  Id.run do
  let mut cond := true
  for j in [:rloc] do
    if s[n + j]! ≠ s[(n - 1) - j]! then
      cond := false
      break
  cond

/-- Find the array of positions for horizontal symmetries. -/
def rsymm (s : Array String) : Array Nat :=
  Id.run do
    let mut rows := #[]
    for n in [:s.size] do
      if isNsymm s n then rows := rows.push (n)
    return rows

/-- Find the array of positions for vertical symmetries. -/
def csymm (s : Array String) : Array Nat :=
  rsymm (transpose s)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let ts := getPats dat
  Id.run do
  let mut tot := 0
  for ca in ts do
--    let ca := ts[cai]!
    let t1 := (ca.splitOn "\n").toArray
--    nums t1
    let rs := rsymm t1
    let cs := csymm t1
    if rs != #[] && cs != #[] then dbg_trace ca
    tot := tot + 100 * rs.sum + cs.sum
  return tot

#assert part1 test == 405

solve 1 33735 file


#eval do -- 33735
  let ts := getPats test
  let ts := getPats <| ← IO.FS.readFile input
  let mut tot := 0
  for ca in ts do
--  let ca := ts[1]!
    let t1 := (ca.splitOn "\n").toArray
--    nums t1
    let rs := rsymm t1
    let cs := csymm t1
    tot := tot + 100 * rs.sum + cs.sum
    IO.print "Rows    "
    IO.println <| rsymm t1
    IO.print "Columns "
    IO.println <| csymm t1
    IO.println ""
  IO.println tot

#eval do
  let ts := getPats test
  let ca := ts[1]!
  let t1 := (ca.splitOn "\n").toArray
--  let n := 5
  nums t1
--  IO.println <| s!"{ca}\n"
  for n in [:t1.size] do
    if isNsymm (transpose ca) n then IO.println <| (n, isNsymm (transpose ca) n)
--    IO.println ""
--    IO.println t1


#exit
def hsymm (s : Array String) : Array Nat :=
  Id.run do
    let mut tot := #[]
    for i in [:s.size] do
      let mut con := 0
      for j in

#eval do
  IO.println <| getPats test

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
