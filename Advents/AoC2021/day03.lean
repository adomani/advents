import Advents.Utils
open Lean

namespace Day03

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day03.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--solve 1

/-!
#  Question 2
-/

def inputToDat (as : Array String) : Array (List Nat) :=
  as.map fun a => a.toList.map (if · == '0' then 0 else 1)

partial
def keepMost (as : Array (List Nat)) (pos : Nat) (many? : Bool) : List Nat :=
  --dbg_trace "Round {pos}: {as}"
  if h : as.size = 1 then as.get ⟨0, lt_of_lt_of_eq Nat.zero_lt_one h.symm⟩ else
  let (zeros, ones) := as.partition (·.getD pos 0 == 0)
  if many? then
    match (zeros.size ≤ ones.size : Bool) with
      | true => keepMost ones (pos + 1) many?
      | false => keepMost zeros (pos + 1) many?
  else
    match (zeros.size ≤ ones.size : Bool) with
      | true => keepMost zeros (pos + 1) many?
      | false => keepMost ones (pos + 1) many?

def toDecimal : List Nat → Nat
  | [] => 0
  | a::as => a * 2 ^ as.length + toDecimal as

#eval
  toDecimal [1, 0, 1, 1, 1]


#eval do
  let dat := inputToDat (← IO.FS.lines input)
  let dat := inputToDat atest
  --dbg_trace dat
  --dat
  let (oxygen, CO2) := (keepMost dat 0 true, keepMost dat 0 false)
  IO.println <| toDecimal oxygen * toDecimal CO2
  --IO.println (oxygen, CO2)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let dat := inputToDat dat
  let (oxygen, CO2) := (keepMost dat 0 true, keepMost dat 0 false)
  toDecimal oxygen * toDecimal CO2


#assert part2 atest == 230

solve 2 2990784

end Day03
