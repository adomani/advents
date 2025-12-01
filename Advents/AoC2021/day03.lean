import Advents.Utils

namespace Day03

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2021"/"day03" : FilePath).withExtension "input"

/-!
#  Question 1
-/

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

/--
Convert an array of strings assumed to be arrays of `0`s and `1`
into an array of lists of `0`s and `0`.
-/
def inputToDat (as : Array String) : Array (List Nat) :=
  as.map fun a => a.toList.map (if · == '0' then 0 else 1)

/--
Given an array of lists of natural numbers, extract the list of natural numbers each of
whose entries is the most common or the least common entry in its position.

* `pos` is expected to be `0` at the start and is the position of the digit under consideration.
* `curr` is the currently accumulared value for the answer.
* `many?` decides whether we select the most common or least common digit.
-/
partial
def constructMost (as : Array (List Nat)) (pos : Nat) (many? : Bool) (curr : Array Nat) : Array Nat :=
  if pos == (as.getD 0 default).length then curr else
  let (zeros, ones) := as.partition (·.getD pos 0 == 0)
  if many? then
    match (zeros.size ≤ ones.size : Bool) with
      | true => constructMost as (pos + 1) many? (curr.push 1)
      | false => constructMost as (pos + 1) many? (curr.push 0)
  else
    match (zeros.size ≤ ones.size : Bool) with
      | true => constructMost as (pos + 1) many? (curr.push 0)
      | false => constructMost as (pos + 1) many? (curr.push 1)

/-- Convert a list of `0`s and `1`s to the corresponding decimal number. -/
def toDecimal : List Nat → Nat
  | [] => 0
  | a::as => a * 2 ^ as.length + toDecimal as

#guard toDecimal [1, 0, 1, 1, 1] == 23

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let dat := inputToDat dat
  let (γ, ε) := (constructMost dat 0 true #[], constructMost dat 0 false #[])
  toDecimal γ.toList * toDecimal ε.toList

#assert part1 atest == 198

solve 1 3813416

/-!
#  Question 2
-/

/--
Given an array of lists of natural numbers, extract the list of natural numbers obtained by
successively filtering all lists whose `pos`th position is the most frequent or most rare digit.

* `pos` is expected to be `0` at the start and is the position of the digit under consideration.
* `many?` decides whether we select the most common or least common digit.
-/
partial
def keepMost (as : Array (List Nat)) (pos : Nat) (many? : Bool) : List Nat :=
  if h : as.size = 1 then as[0] else
  let (zeros, ones) := as.partition (·.getD pos 0 == 0)
  if many? then
    match (zeros.size ≤ ones.size : Bool) with
      | true => keepMost ones (pos + 1) many?
      | false => keepMost zeros (pos + 1) many?
  else
    match (zeros.size ≤ ones.size : Bool) with
      | true => keepMost zeros (pos + 1) many?
      | false => keepMost ones (pos + 1) many?

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let dat := inputToDat dat
  let (oxygen, CO2) := (keepMost dat 0 true, keepMost dat 0 false)
  toDecimal oxygen * toDecimal CO2

#assert part2 atest == 230

solve 2 2990784

end Day03
