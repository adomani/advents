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

#eval do
  for i in [0:7] do
    IO.println <| combo ⟨10, 20, 30, 0, #[], #[]⟩ i


def oneOp (s : State) : State :=
  let pos := s.pos
  match s.program[pos]?, s.program[pos + 1]? with
    | none, _ => s
    | _, none => panic "I really do not want to be here"
    | some opcode, some lit =>
      let combo := combo s lit
      let s := {s with pos := pos + 2}
      match opcode with
        | 0 => let adv := s.A / 2 ^ combo; {s with A := adv}
        | 1 => let bxl := s.B.xor lit; {s with B := bxl}
        | 2 => let bst := combo % 8; {s with B := bst}
        | 3 => if s.A == 0 then s else {s with pos := lit}
        | 4 => let bxc := s.B.xor s.C; {s with B := bxc}
        | 5 => let out := combo % 8; {s with out := s.out.push out}
        | 6 => let bdv := s.A / 2 ^ combo; {s with B := bdv}
        | 7 => let cdv := s.A / 2 ^ combo; {s with C := cdv}
        | o => panic s!"opcode was {o}, but should be at most 7"

#eval do
  let inps : Array (String × (State → Array Nat)) := #[
    ("0\n0\n9\n2,6", (#[·.B])),
    ("10\n7880\n9233\n5,0,5,1,5,4", (·.out)),
    ("2024\n7880\n9233\n0,1,5,4,3,0", fun s => s.out.push s.A),
    ("2024\n29\n9233\n1,7", (#[·.B])),
    (test, (·.out))
  ]
  for (inp, _) in inps do
    let dat := (inp.splitOn "\n").toArray
    let mut s := inputToState dat
    let mut con := 0
    --IO.println <| s!"Steps: {con}\n\n{s}"
    while (s.program[s.pos]?).isSome do
      con := con + 1
      s := oneOp s
    IO.println <| s!"Steps: {con}\n\n{s}"


#eval do
  let dat ← IO.FS.lines input
  let inp := "0\n0\n9\n2,6"
  let inp := "10\n7880\n9233\n5,0,5,1,5,4"
  let inp := "2024\n7880\n9233\n0,1,5,4,3,0"
  let dat := atest
  let inp := "2024\n29\n9233\n1,7"
  let dat := (inp.splitOn "\n").toArray
  let mut s := inputToState dat
  let mut con := 0
  IO.println <| s!"Steps: {con}\n\n{s}"
  while (s.program[s.pos]?).isSome do
    con := con + 1
    s := oneOp s
  IO.println <| s!"Steps: {con}\n\n{s}"

-- 350151510

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

end Day17
