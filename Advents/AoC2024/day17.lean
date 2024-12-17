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

structure State where
  A : Nat
  B : Nat
  C : Nat
  pos : Nat := 0
  program : Array Nat
  out : Array Nat := ∅
  deriving Inhabited

--    pos: {(s.pos, [s.program[s.pos]?, s.program[s.pos+1]?].reduceOption)}\n\
--    (A, B, C): ({String.toNat! (⟨Nat.toDigits 8 s.A⟩)}, {String.toNat! (⟨Nat.toDigits 8 s.B⟩)}, {String.toNat! (⟨Nat.toDigits 8 s.C⟩)}) % 8 = ({s.A % 8}, {s.B % 8}, {s.C % 8})\n\

instance : ToString State where
  toString s := s!"\
    (A, B, C): ({s.A}, {s.B}, {s.C}) % 8 = ({s.A % 8}, {s.B % 8}, {s.C % 8})\n\
    program: {s.program.toList.take s.pos} \
             {[s.pos, s.pos+1].map (s.program[·]?) |>.reduceOption} \
             {s.program.toList.drop (s.pos + 2)}\n\
    out: {s.out}, pos: {s.pos}\n"

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
      --dbg_trace "opcode {opcode}, literal {lit}"
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
            --dbg_trace "no jump"
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
          {s with C := s.A / (2 ^ (combo s lit)) % 8}
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

solve 1 350151510


#eval show Elab.Term.TermElabM _ from do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut s := inputToState dat
  let mut con := 0
  IO.println <| s!"{s}" --s!"Steps: {con}\n\n{s}"
  while (s.program[s.pos]?).isSome do
    con := con + 1
    s := oneOp s
    IO.println <| s!"{s}" --s!"Steps: {con}\n\n{s}"
  guard <| s.out == #[3, 5, 0, 1, 5, 1, 5, 1, 0]

-- 350151510
#eval (60589763 - 2 ^ 6 * (60589763 / (2 ^ 6)))
#eval ((60589763 / (2 ^ 3)))
/-!
#  Question 2
-/

partial
def binToNat (a : Array Nat) : Nat :=
  if a.isEmpty then 0 else
    2 * binToNat a.pop + a.back!

def get3 (a : Array Nat) : Array Nat × Array Nat :=
  let a3 := (a.reverse.take 3).reverse
  let aleft := a.pop.pop.pop
  (aleft, a3)

#eval binToNat #[0, 1, 1, 1, 0, 1]

def reprogramOne (a : Array Nat) : Array Nat × Nat :=

  let (aleft, a3) := get3 a
  match a3 with
    | #[] => default
    | s =>
      let seed1 := binToNat s
      let offset := seed1.xor 5
      let c := (Array.range offset).foldl (fun _ => ·.pop) a
      let (_, seed2) := get3 c
      (aleft, (seed1.xor 3).xor (binToNat seed2))

def inits (i : Nat) : Std.HashSet (Array Nat) :=
  (Array.range (2 ^ 12)).foldl (fun h A =>
    let a : Array Nat := (Nat.toDigits 2 (2 ^ 17 + A)).map ("".push · |>.toNat!) |>.toArray
    if (reprogramOne a).2 == i then
      --let a := match a.take 3 with
      --  | #[] => #[0, 0, 0]
      --  | #[x] => #[0, 0, x]
      --  | #[x, y] => #[0, x, y]
      --  | t => t
      h.insert (a.reverse.take 12) else h) ∅


/--
info: There are 320 12-digit sequences outputting #[0,...]
There are 704 12-digit sequences outputting #[1,...]
There are 320 12-digit sequences outputting #[2,...]
There are 960 12-digit sequences outputting #[3,...]
There are 320 12-digit sequences outputting #[4,...]
There are 704 12-digit sequences outputting #[5,...]
There are 320 12-digit sequences outputting #[6,...]
There are 448 12-digit sequences outputting #[7,...]
There are 0 12-digit sequences outputting #[8,...]
-/
#guard_msgs in
#eval do
  for d in [0:9] do
    let sts2 := inits d
    for a in sts2 do
      let dig := (reprogramOne a.reverse).2
      if dig != d then
        IO.println dig
    IO.println s!"There are {sts2.size} 12-digit sequences outputting #[{d},...]"

def merge2 (sts1 sts2 : Std.HashSet (Array Nat)) : Std.HashSet (Array Nat) := Id.run do
  let mut next : Std.HashSet _ := ∅
  for a4 in sts2 do
    let a4d := a4.pop.pop.pop
    let cands := sts1.filter fun a2 : Array _ =>
                     --((a2.take 9).size != a4d.size)
                    (a2.toList.drop (a2.size - a4d.size)).toArray == a4d
    --dbg_trace cands.size
    next := next.union <| cands.fold (·.insert <| · ++ (a4.toList.drop (a4.size - 3)).toArray) ∅
  return next

#eval do
  let pr := #[2, 4, 1, 5, 7, 5, 1, 6, 4, 1, 5, 5, 0, 3, 3, 0]
  let pr' := pr.sortDedup
  let sts := pr.foldl (fun (h : Array (Std.HashSet _)) (n : Nat) => h.push (inits n)) ∅
  let mut merges := #[]
  for pi in [0:sts.size - 1] do
    let p1 := sts[pi]!
    let p2 := sts[pi + 1]!
    let a24 := merge2 p1 p2
    merges := merges.push a24
    let next := a24
  --let sts1 := inits 2
  --let sts2 := inits 4
  --let mut next : Std.HashSet _ := ∅
  --for a4 in sts2 do
  --  let a4d := a4.pop.pop.pop
  --  let cands := sts1.filter fun a2 : Array _ =>
  --                   --((a2.take 9).size != a4d.size)
  --                  (a2.toList.drop 3).toArray == a4d
  --  --dbg_trace cands.size
  --  next := next.union <| cands.fold (·.insert <| · ++ (a4.toList.drop 9).toArray) ∅
    for a in next do
      let dig := (reprogramOne a.reverse)
      if dig.2 != pr[pi]! then
        IO.println dig
      let dig := (reprogramOne dig.1)
      if dig.2 != pr[pi + 1]! then
        IO.println dig

    IO.println s!"There are {next.size} 12-digit sequences outputting #[{pr[pi]!}, {pr[pi + 1]!},...]"
  let mergeTot1 := merge2 merges[0]! merges[1]!
  let mergeTot2 := merge2 merges[1]! merges[2]!
  IO.println <| (mergeTot1.size, mergeTot2.size)
  let fins := ((merges.erase merges[0]!).foldl merge2 merges[0]!).toArray
  let ns := fins.map fun s : Array _ => binToNat s.reverse
  IO.println <| (ns.qsort (· < ·))[0]!
/-!
-/

#eval do
  let pr := #[2, 4, 1, 5, 7, 5, 1, 6, 4, 1, 5, 5, 0, 3, 3, 0]
  let sts := #[#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
]
  for st in sts do
  --let st := #[0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0]
    let st := st.reverse
    let n := binToNat st
    let mut outs := #[]
    let mut a := st
    for i in [0:16] do
      let (a', out) := reprogramOne a
      a := a'
      outs := outs.push out
    if pr == outs then
      IO.println (n, outs)
    else IO.println "error!"
  let ns := sts.map fun s : Array _ => binToNat s.reverse
  IO.println <| ns.qsort (· < ·)


#exit
  let mut firstDigs : Std.HashSet (Array Nat) := ∅
  let pr := #[2, 4, 1, 5, 7, 5, 1, 6, 4, 1, 5, 5, 0, 3, 3, 0]
  let bd := 3
  --let mut a : Array Nat := #[]
  --let A := 60589763
  for A in [0:2 ^ 1] do
    --let a := Nat.toDigits 2 A
      let mut out := #[]
    --IO.println a
    --let fd : Array Nat := Id.run do
      let mut a : Array Nat := (Nat.toDigits 2 A).map ("".push · |>.toNat!) |>.toArray
      let a0 := a
      --let mut out := #[]
      --let mut r := #[]
      for i in [0:bd] do
        let (a', o') := reprogramOne a
        (a, out) := (a', out.push o')
        --if bd ≤ out.size then dbg_trace "{Nat.toDigits 2 A}, {out}"
        --if pr.take out.size != out then return #[]
      --return out
      if  (!out.isEmpty) &&
      out == pr.take out.size then
        firstDigs := firstDigs.insert (a.reverse.take 3)
        --IO.println s!"{a.reverse} {fd}"
  IO.println <| firstDigs.toArray
  --let (a', o') := reprogramOne a
  --(a, out) := (a', out.push o')
  --IO.println <| (a, out)
  --let (a', o') := reprogramOne a
  --(a, out) := (a', out.push o')
  --IO.println <| (a, out)

/-
def oneLoop (s : State) : State :=
  (List.range s.program.size).foldl (fun _ => oneOp ·) s

def secondLoop (s : State) : State :=
  (List.range s.program.size).foldl (fun _ => oneLoop ·) s
-/

#eval show Elab.Term.TermElabM _ from do
  let dat := atest2
  let dat ← IO.FS.lines input
  let mut s := inputToState dat
  --s := {s with A := 117440}
  let mut con := 0
  --for a in [2^11:2^12] do
  --  let sa := secondLoop {s with A := a}
--
  --  if sa.out.take 5 == s.program.take 5 then
  --    IO.println <| s!"{Nat.toDigits 8 a}: {sa}" --s!"Steps: {con}\n\n{s}"
--#exit
  IO.println <| s!"{s}" --s!"Steps: {con}\n\n{s}"
  while (s.program[s.pos]?).isSome do
    con := con + 1
    s := oneOp s
    if s.pos == 6 then IO.println <| s!"{s.C % 8}" --s!"Steps: {con}\n\n{s}"
    if s.pos == 0 then IO.println <| s!"{s}" --s!"Steps: {con}\n\n{s}"
  IO.println <| s!"{s}" --s!"Steps: {con}\n\n{s}"

#eval Nat.xor 2 5
/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day17
