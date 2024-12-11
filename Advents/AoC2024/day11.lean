import Advents.Utils
open Lean

namespace Day11

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day11.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "0 1 10 99 999"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `test2` is the test string for the problem. -/
def test2 := "125 17"

/-- `atest2` is the test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/--
The effect of blinking ones on the stones, represented as a `HashMap` where
* the keys are the numbers on the stones and
* the values are the multiplicities.
-/
def blink (h : Std.HashMap Nat Nat) : Std.HashMap Nat Nat :=
  h.fold (init := {}) fun h val mult =>
    if val == 0
    then
      h.alter 1 (some <| ·.getD 0 + mult)
    else
    let digs := Nat.toDigits 10 val
    let hlf := digs.length
    if hlf % 2 == 0 then
      let left := String.toNat! ⟨digs.take (hlf / 2)⟩
      let right := String.toNat! ⟨digs.drop (hlf / 2)⟩
      h |>.alter left  (some <| ·.getD 0 + mult)
        |>.alter right (some <| ·.getD 0 + mult)
    else
      h |>.alter (2024 * val) (some <| ·.getD 0 + mult)

/-- The iteration of `blink`. -/
def blinks (h : Std.HashMap Nat Nat) : Nat → Std.HashMap Nat Nat
  | 0 => h
  | n + 1 => blinks (blink h) n

/-- A function to test the outcome of `blink`ing. -/
def mkTest (dat : String) : Array (Nat × Nat) :=
  let vals := dat.getNats
  let h := Std.HashMap.ofList <| dat.getNats.zip <| List.replicate vals.length 1
  (blink h).toArray.qsort (·.1 < ·.1)

#guard mkTest      "0" == #[(1, 1)]
#guard mkTest      "0" == #[(1, 1)]
#guard mkTest      "1" == #[(2024, 1)]
#guard mkTest     "10" == #[(0, 1), (1, 1)]
#guard mkTest     "99" == #[(9, 2)]
#guard mkTest    "999" == #[(2021976, 1)]
#guard mkTest "0 1 10" == #[(0, 1), (1, 2), (2024, 1)]

/-- Converts the input string `dat` and the number of `blink`s `n` to the final configuration. -/
def parts (dat : String) (n : Nat) : Nat :=
  let vals := dat.getNats
  let h := Std.HashMap.ofList <| dat.getNats.zip <| List.replicate vals.length 1
  let fin := blinks h n
  fin.fold (fun tot _ mult => tot + mult) 0

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := parts dat 25

#assert part1 test2 == 55312

solve 1 186996 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := parts dat 75

#assert part2 test == 149161030616311 -- not given in the problem

solve 2 221683913164898 file

end Day11
