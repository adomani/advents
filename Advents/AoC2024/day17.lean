import Advents.Utils
open Lean

namespace Day17

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day17.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure State where
  A : Nat
  B : Nat
  C : Nat
  pos : Nat := 0
  program : Array Nat
  out : Array Nat := ∅
  deriving Inhabited

instance : ToString State where
  toString s := s!"A: {s.A}\nB: {s.B}\nC: {s.C}\npos: {s.pos}\nprogram: {s.program}\nout: {s.out}\n"

def inputToState (a : Array String) : State :=
  let (abc, program) := a.map (·.getNats) |>.partition (·.length == 1)
  match abc.foldl (· ++ ·) [] with
    | [a, b, c] => {A := a, B := b, C := c, program := (program.foldl (· ++ ·) ∅).toArray}
    | _ => panic "Unparsed input!"

#eval do
  let dat := atest
  let s := inputToState dat
  IO.println s

def combo (s : State) : Nat → Nat
  | 4 => s.A
  | 5 => s.B
  | 6 => s.C
  | n + 7 => panic s!"instructions are never 7, while this one was {n + 7}!" + 7
  | n => n

/-- info: (A, B, C): (10, (20, 30)): (0 → 0) (1 → 1) (2 → 2) (3 → 3) (4 → 10) (5 → 20) (6 → 30) -/
#guard_msgs in
#eval do
  let s := {(default : State) with A := 10, B := 20, C := 30}
  IO.print s!"(A, B, C): {(s.A, s.B, s.C)}:"
  for i in [0:7] do
    IO.print <| s!" ({i} → {combo s i})"


def oneOp (s : State) : State :=
  let pos := s.pos
  match s.program[pos]? with
    | none => panic s!"Reading position {pos} is out of bounds: {s.program.size}!"
    | some opcode =>
      let lit := s.program[pos + 1]!
      let s := {s with pos := pos + 2}
      dbg_trace "opcode {opcode}, literal {lit}"
      match opcode with
        | 0 => -- adv
          {s with A := s.A / (2 ^ (combo s lit))}
        | 1 => -- bxl
          {s with B := s.B.xor lit}
        | 2 => -- bst
          {s with B := (combo s lit) % 8}
        | 3 => -- jnz
          if s.A == 0
          then
            s
          else
            --dbg_trace "jumping from {pos} to {lit}"
            {s with pos := lit}
        | 4 => -- bxc
          {s with B := s.B.xor s.C}
        | 5 => -- out
          {s with out := s.out.push ((combo s lit) % 8)}
        | 6 => -- bdv
          {s with B := s.A / (2 ^ (combo s lit))}
        | 7 => -- cdv
          {s with C := s.A / (2 ^ (combo s lit))}
        | o =>
          panic s!"opcode was {o}, but should be at most 7"

#eval show Elab.Term.TermElabM _ from do
  let inps : Array (String × (State → Bool)) := #[
    ("0\n0\n9\n2,6", (·.B == 1)),
    ("10\n7880\n9233\n5,0,5,1,5,4", (·.out == #[0,1,2])),
    ("2024\n7880\n9233\n0,1,5,4,3,0", fun s => s.out == #[4,2,5,6,7,7,7,7,3,1,0] && s.A == 0),
    ("2024\n29\n9233\n1,7", (·.B == 26)),
    ("2024\n2024\n43690\n4,0", (·.B == 44354)),
    (test, (·.out == #[4,6,3,5,6,3,5,2,1,0]))
  ]
  for (inp, check) in inps do
    let dat := (inp.splitOn "\n").toArray
    let mut s := inputToState dat
    let mut con := 0
    --IO.println <| s!"Steps: {con}\n\n{s}"
    while (s.program[s.pos]?).isSome do
      con := con + 1
      s := oneOp s
    guard (check s)
    --if check s then
    --  IO.println s!"Test passed"
    --else IO.println s!"Fail! {inp}"
    --IO.println <| s!"Steps: {con}\n\n{s}"

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let mut s := inputToState dat
  while (s.program[s.pos]?).isSome do
    s := oneOp s
  return (s.out.foldl (· ++ s!"{·}") "").toNat!

#assert part1 atest == 4635635210

set_option trace.profiler true in solve 1 350151510


#eval show Elab.Term.TermElabM _ from do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut s := inputToState dat
  let mut con := 0
  IO.println <| s!"Steps: {con}\n\n{s}"
  while (s.program[s.pos]?).isSome do
    con := con + 1
    s := oneOp s
    IO.println <| s!"Steps: {con}\n\n{s}"
  guard <| s.out == #[3, 5, 0, 1, 5, 1, 5, 1, 0]

-- 350151510

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day17
