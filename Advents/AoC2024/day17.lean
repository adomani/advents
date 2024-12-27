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

/-- `test2` is the test string for the problem. -/
def test2 := "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"

/-- `atest2` is the test2 string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/--
The state of the program.
* The `A`-register, containing a natural number.
* The `B`-register, containing a natural number.
* The `C`-register, containing a natural number.
* `pos` is the pointer to the position in the program where we are currently, starting from `0`.
* `program` list of instructions encoding the program: these alternate between being
  `opcode`s (each assigned to an operation) and
  `literal`s (numbers used as inputs to the operations).
* `out` is the current output of the program.

The `program` does not change during execution, while everything else does.
-/
structure State where
  /-- The `A`-register, containing a natural number. -/
  A : Nat
  /-- The `B`-register, containing a natural number. -/
  B : Nat
  /-- The `C`-register, containing a natural number. -/
  C : Nat
  /-- `pos` is the pointer to the position in the program where we are currently,
  starting from `0`. -/
  pos : Nat := 0
  /-- `program` list of instructions encoding the program: these alternate between being
  `opcode`s (each assigned to an operation) and
  `literal`s (numbers used as inputs to the operations). -/
  program : Array Nat
  /-- `out` is the current output of the program. -/
  out : Array Nat := ∅
  deriving Inhabited

/--
A utility instance to print the state, recording as much as the information as possible.
This is not needed for the final solution, but was useful in working out the problem.
-/
instance : ToString State where
  toString s := s!"\
    (A, B, C): ({s.A}, {s.B}, {s.C}) % 8 = ({s.A % 8}, {s.B % 8}, {s.C % 8})\n\
    program: {s.program.toList.take s.pos} \
             {[s.pos, s.pos+1].map (s.program[·]?) |>.reduceOption} \
             {s.program.toList.drop (s.pos + 2)}\n\
    out: {s.out}, pos: {s.pos}\n"

/-- Converts the input to the program state. -/
def inputToState (a : Array String) : State :=
  let (abc, program) := a.map (·.getNats) |>.partition (·.length == 1)
  match abc.foldl (· ++ ·) [] with
    | [a, b, c] => {A := a, B := b, C := c, program := (program.foldl (· ++ ·) ∅).toArray}
    | _ => panic "Unparsed input!"

/-- The `combo` operand as a function of the state and the `literal` operand. -/
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

/-- Perform one operation of the program, updating all the state as necessary. -/
def oneOp (s : State) : State :=
  let pos := s.pos
  match s.program[pos]? with
    | none => panic s!"Reading position {pos} is out of bounds: {s.program.size}!"
    | some opcode =>
      let lit := s.program[pos + 1]!
      let s := {s with pos := pos + 2}
      match opcode with
        | 0 => -- adv
          {s with A := s.A / (2 ^ (combo s lit))}
        | 1 => -- bxl
          {s with B := s.B.xor lit}
        | 2 => -- bst
          {s with B := (combo s lit) % 8}
        | 3 => -- jnz
          if s.A == 0 then s else {s with pos := lit}
        | 4 => -- bxc
          {s with B := s.B.xor s.C}
        | 5 => -- out
          {s with out := s.out.push ((combo s lit) % 8)}
        | 6 => -- bdv
          {s with B := s.A / (2 ^ (combo s lit))}
        | 7 => -- cdv
          {s with C := s.A / (2 ^ (combo s lit)) % 8}
        | o =>
          panic s!"opcode was {o}, but should be at most 7"

-- Tests to make sure that the program works as intended.
#eval show Elab.Term.TermElabM _ from do
  let inps : Array (String × (State → Bool)) := #[
    ("0\n0\n9\n2,6", (·.B == 1)),
    ("10\n7880\n9233\n5,0,5,1,5,4", (·.out == #[0,1,2])),
    ("2024\n7880\n9233\n0,1,5,4,3,0", fun s => s.out == #[4,2,5,6,7,7,7,7,3,1,0] && s.A == 0),
    ("2024\n29\n9233\n1,7", (·.B == 26)),
    ("2024\n2024\n43690\n4,0", (·.B == 44354)),
    (test, (·.out == #[4,6,3,5,6,3,5,2,1,0]))]
  for (inp, check) in inps do
    let dat := (inp.splitOn "\n").toArray
    let mut s := inputToState dat
    let mut con := 0
    while (s.program[s.pos]?).isSome do
      con := con + 1
      s := oneOp s
    guard (check s)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : String := Id.run do
  let mut s := inputToState dat
  while (s.program[s.pos]?).isSome do
    s := oneOp s
  return ",".intercalate <| s.out.map (s!"{·}") |>.toList

#assert part1 atest == "4,6,3,5,6,3,5,2,1,0"

solve 1 "3,5,0,1,5,1,5,1,0"

/-!
#  Question 2
-/

/--
Convert an array of binary digits into the corresponding natural number.
The most significant digits appear *first*.
-/
partial
def binToNat (a : Array Nat) : Nat :=
  if a.isEmpty then 0 else
    2 * binToNat a.pop + a.back!

/--
An auxilliary function: it returns the result of splitting the input into an array of size
`n - 3` and one of size `3`.
-/
def get3 (a : Array Nat) : Array Nat × Array Nat :=
  let a3 := (a.reverse.take 3).reverse
  let aleft := a.pop.pop.pop
  (aleft, a3)

/--
The "period" of the program.
Assumes that the program consists of `2 * lth + 2` entries, where the last `2` entries make the
program loop, as long as `A` is non-zero.

It turns out that the given program goes through a series of `7` steps before looping back at the
beginning and the number of entries of the program is `16`.
Similarly, the example for the puzzle goes through `2` steps before looping back and the number
of entries of the test program is `6`.

Only the value of `A` is significant in each loop and determines `B`, `C` and each digit of the
output.
When the program loops back, it drops 3 binary digits from `A` and starts over.

The slight complication is that more than just the last `3` binary digits of `A` matter: as much as
`10` binary digits of `A` are necessary to know what the output is.
-/
def reprogramOne (s : State) (a : Array Nat) : Array Nat × Nat :=
  let seed := binToNat a
  -- we make the guess that the program goes through `lth`-many steps, before looping back
  let lth := (s.program.size - 2) / 2
  let new := (Array.range lth).foldl (init := {s with A := seed}) fun s _ => oneOp s
  (((Nat.toDigits 2 new.A).map (fun d => String.toNat! (⟨[d]⟩))).toArray, new.out.back!)

/--
Returns all the 10-digit binary numbers that yield first digit `i` when used in the program.
The output is stored in *reverse* order, so that the most significant digits are last.
-/
def inits (s : State) (i : Nat) : Std.HashSet (Array Nat) :=
  --  we use 10, since these should be the relevant digits for guaranteeing the first digit
  (Array.range (2 ^ 10)).foldl (fun h A =>
    let a : Array Nat := (Nat.toDigits 2 (2 ^ 11 + A)).map ("".push · |>.toNat!) |>.toArray
    if (reprogramOne s a).2 == i then
      -- we only store `9` digits, since empirically this is enough and speeds up the computation
      h.insert (a.reverse.take 9) else h) ∅

/--
The inputs are arrays of natural numbers, each one containing arrays of a fixed size
(though the common size for `sts1` could be different from the size for `sts2`).

The output is the result of merging the arrays of `sts1` with the arrays of `sts2`, as follows:
* if `#[a, b, c] ++ common` is in `sts1` and
* `common ++ rest` is in `sts2`

then we store `#[a, b, c] ++ common ++ rest`.
-/
def merge2 (sts1 sts2 : Std.HashSet (Array Nat)) : Std.HashSet (Array Nat) :=
  sts2.fold (init := ∅) fun next a4 =>
    let a4d := a4.pop.pop.pop
    let cands := sts1.filter fun a2 : Array _ =>
                    (a2.toList.drop (a2.size - a4d.size)).toArray == a4d
    next.union <| cands.fold (·.insert <| · ++ (a4.toList.drop (a4.size - 3)).toArray) ∅

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let s := inputToState dat
  let pr := s.program
  let sts := pr.foldl (·.push <| inits s ·) #[]
  let merges := (Array.range (sts.size - 1)).foldl (init := #[]) fun merges pi =>
    let p1 := sts[pi]!
    let p2 := sts[pi + 1]!
    let a24 := merge2 p1 p2
    merges.push a24
  let fins := ((merges.erase merges[0]!).foldl merge2 merges[0]!).toArray
  let ns := fins.map (binToNat ·.reverse)
  (ns.qsort (· < ·))[0]!

#assert part2 atest2 == 117440

solve 2 107413700225434 -- takes about 3s

end Day17
