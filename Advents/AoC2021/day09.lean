import Advents.Utils
open Lean

namespace Day09

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day09.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "2199943210
3987894921
9856789892
8767896789
9899965678"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

#check Char.isDigit
#eval
  let c := '9'
  --dbg_trace ('4' < '3' : Bool)
  let s := ""
  let d := s.get ⟨000⟩
  (c, d, (c < d : Bool))

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  --let mut lows := 0
  let mut heights := 0
  for d in [0:dat.size] do
    let prev := dat[d-1]!
    let next := dat[d+1]?.getD dat[d]!
    let row := dat[d]!
    for c in [0:row.length] do
      let curr := row.get ⟨c⟩
      let rownext := if c == row.length - 1 then curr else row.get ⟨c+1⟩
      let rowprev := if c == 0              then curr else row.get ⟨c-1⟩
      if (curr ≤ rownext && curr ≤ rowprev && curr ≤ prev.get ⟨c⟩ && curr ≤ next.get ⟨c⟩) &&
        ! (curr == rownext && curr == rowprev && curr == prev.get ⟨c⟩ && curr == next.get ⟨c⟩)
      then
        --lows := lows + 1
        heights := heights + 1 + ("".push curr).toNat!
  IO.println heights

--  594 -- too low,  all `<`
--  633 -- incorrect
--  683 -- too high, all `≤`, but not all `==`
-- 1773 -- too high, all `≤`

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

end Day09
