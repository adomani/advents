import Advents.Utils
open Lean

namespace Day24

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day24.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure ALU where
  var : List (String × Int)
  ops : Array (String → String → Int)

def lineToFun (d : String) (var : List (String × Int)) : Int :=
  match d.splitOn with
    | [op, v1, v2] =>
      let w1 := match var.lookup v1 with | some d => d | none => v1.toInt!
      let w2 := match var.lookup v2 with | some d => d | none => v2.toInt!
      match op with
        | "add" => w1 + w2
        | "mul" => w1 * w2
        | "div" => w1 / w2
        | "mod" => w1 % w2
        | "eql" => if w1 == w2 then 1 else 0
        | _ => panic s!"The value of '{op}' is unexpected: \
                      it should be 'add', 'mul', 'div', 'mod', or 'eql'"
    | _ => panic "Expected exactly 3 words from each line of the input"

def inputToFun (dat : Array String) (var : List (String × Int)) (n : Int) : Int :=
  dat.foldl (init := n) fun tot s => match s.splitOn with
    | [op, v1, v2] =>
      let w1 := match var.lookup v1 with | some d => d | none => v1.toInt!
      let w2 := match var.lookup v2 with | some d => d | none => v2.toInt!

      default
    | _ => panic "Expected exactly 3 words from each line of the input"

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--set_option trace.profiler true in solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day24
