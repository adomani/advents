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
  input : List Nat
  var : List (String × Int)
  ops : Array String
  current : Nat
  deriving Inhabited

instance : ToString ALU where
  toString a :=
    let pairs := "\n".intercalate <| a.var.foldl (init := ["Pairings:"]) fun h (var, val) => h ++ [s!"{var}: {val}"]
    s!"(w, x, y, z) = {a.var.map Prod.snd}\nCurrent: {a.current}, prevMove: {a.ops[a.current-1]!}, nextMove: {a.ops.getD a.current "Done"}"

def inputToALU (dat : Array String) : ALU where
  input := []
  var := [("w", 0), ("x", 0), ("y", 0), ("z", 0)]
  ops := dat
  current := 0

def modifyEntry (a : ALU) (w : String) (result : Int) : ALU :=
  {a with var := a.var.modify (fun (x, _) => (x, result)) (a.var.findIdx? (·.1 == w)).get!}

def ALU.enter (a : ALU) (l : List Nat) : ALU :=
  {a with input := l}

def oneOp (a : ALU) : ALU :=
  let d := a.ops[a.current]!
  let a := {a with current := a.current + 1}
  match d.splitOn with
    | [op, v1, v2] =>
      let w1 := (a.var.lookup v1).getD v1.toInt!
      let w2 := (a.var.lookup v2).getD v2.toInt!
      let result := match op with
        | "add" => w1 + w2
        | "mul" => w1 * w2
        | "div" => w1 / w2
        | "mod" => w1 % w2
        | "eql" => if w1 == w2 then 1 else 0
        | _ => panic
          s!"The value of '{op}' is unexpected: it should be 'add', 'mul', 'div', 'mod', or 'eql'"
      modifyEntry a v1 result
    | ["inp", w] => {modifyEntry a w a.input[0]! with input := a.input.drop 1}
    | _ => panic "Expected exactly 3 words from each line of the input"



#eval do
  let dat := atest
  --let vars : List (String × Int) := [("w", 4), ("z", 2), ("y", 1), ("x", 8)]
  --let a : ALU := {var := vars, ops := #[]}
  let mut a := inputToALU dat
  a := a.enter [4, 0, 0, 0]
  IO.println <| a
  while a.current < a.ops.size do
    a := oneOp a
    IO.println s!"{a}\n"



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
