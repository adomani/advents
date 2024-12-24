import Advents.Utils
open Lean

namespace Day24

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day24.input"

/-!
#  Question 1
-/

/-- `test1` is the test string for the problem. -/
def test1 := "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02"

/-- `atest1` is the test string for the problem, split into rows. -/
def atest1 := (test1.splitOn "\n").toArray

/-- `test2` is the test string for the problem. -/
def test2 := "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"

/-- `atest2` is the test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

structure state where
  /-- `1 ↦ some true`, `0 ↦ some false`, `. ↦ none`. -/
  values : Std.HashMap String (Option Bool)
  /-- 4 entries representing first value, operation, second value, where to store the result. -/
  gates : Std.HashSet (String × String × String × String)

def inputToState (dat : Array String) : state :=
  dat.foldl (init := {values := {}, gates := {}}) fun s i =>
    match i.splitOn with
    | [v, tf] =>
      {s with values := s.values.insert (v.dropRight 1) (if tf == "1" then true else false)}
    | [v1, op, v2, _, tgt] => {s with
        values := [v1, v2, tgt].foldl (init := s.values) fun vs n => (vs.alter n (some <| ·.getD none))
        gates := s.gates.insert (v1, op, v2, tgt)

        }
    | [""] => s
    | _ => dbg_trace "oh no!"; s

def showState (s : state) : IO Unit := do
  let sortedValues := s.values.toArray.qsort (·.1 < ·.1)
  IO.println <|
    s!"values:\n{"\n".intercalate <| sortedValues.map (s!"{·}") |>.toList}\n\n\
      gates:\n{"\n".intercalate <| s.gates.toList.map (s!"{·}")}"

#eval do
  let dat := atest1
  let s := inputToState dat
  showState s

def oper : String → Bool → Bool → Bool
  | "AND" => and
  | "OR"  => or
  | "XOR" => xor
  | op? => dbg_trace "oper error on {op?}!"; fun _ _ => false

def runOnce (s : state) : state :=
  let newValues := s.gates.fold (init := s.values) fun vs (s1, op, s2, tgt) =>
    match vs[s1]?, vs[s2]?, op with
      | none, _, _ | _, none, _ => dbg_trace "runOnce error!"; vs
      | some none, _, _ | _, some none, _ => vs
      | some (some s1), some (some s2), op => vs.insert tgt <| some ((oper op) s1 s2)
      --| _, _, _ => dbg_trace "runOnce error!"; vs
  {s with values := newValues}

def runAll (s : state) : state := Id.run do
  let mut s := s
  while s.values.valuesArray.contains none do
    s := runOnce s
  s

partial
def toNum (s : Array Bool) : Nat :=
  if s.isEmpty then 0 else
  (if s.back! then 2 ^ (s.size - 1) else 0) + toNum s.pop

#assert toNum #[] == 0
#assert toNum #[false, false, true] == 4

def out (s : state) : Nat :=
  let zs := s.values.filterMap fun str bl => if str.startsWith "z" then bl else none
  let sortedZs := zs.toArray.qsort (·.1 < ·.1) |>.map (·.2)
  toNum sortedZs

#eval do
  let dat := atest1
  let s := inputToState dat
  showState s
  IO.println "\nrunOnce\n"
  showState <| runOnce s
  IO.println <| s!"\nanswer: {out <| runOnce s}"

#eval do
  let dat := atest2
  let s := inputToState dat
  showState s
  --let s := runOnce s
  --let s := runOnce s
  IO.println "\nrunAll\n"
  showState <| runAll s
  IO.println <| s!"\nanswer: {out <| runAll s}"

#eval do
  let dat ← IO.FS.lines input
  let s := inputToState dat
  IO.println <| s!"\nanswer: {out <| runAll s}"

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
