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

def showGates (s : state) : IO Unit := do
  let sortedGates := s.gates.toArray.qsort (·.1 < ·.1)
  IO.println <| s!"gates:\n{"\n".intercalate <| sortedGates.toList.map (s!"{·}")}"

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
  let dat := atest2
  let s := inputToState dat
  showState s
  IO.println "\nrunAll\n"
  showState <| runAll s
  IO.println <| s!"\nanswer: {out <| runAll s}"

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let s := inputToState dat
  out <| runAll s

#assert part1 atest2 == 2024

solve 1 58639252480880

/-!
#  Question 2
-/

def showOut (s : state) : IO Unit := do
  let mut tots := #[]
  for xyz in #["x", "y", "z"] do
    let xs := s.values.filterMap fun str bl => if str.startsWith xyz then bl else none
    tots := tots.push <| toNum <| xs.toArray.qsort (·.1 < ·.1) |>.map (·.2)
  let sum := tots[0]! + tots[1]!
  IO.println s!"{if sum == tots[2]! then checkEmoji else crossEmoji} \
              (x, y, z) = ({tots[0]!}, {tots[1]!}, {tots[2]!}), \
              sum: {sum}"

/-- info:
❌️ (x, y, z) = (7, 2, 4), sum: 9
❌️ (x, y, z) = (13, 31, 2024), sum: 44
-/
#guard_msgs in
#eval do
  for dat in [atest1, atest2] do
    showOut <| runAll (inputToState dat)

#eval do
  for i in [13, 31, 2024] do
    IO.println <| List.reverse <| Nat.toDigits 2 i

/-- `visitedOnce` is just like this, but uses the state `VisState` -/
def visitedOnce' (h : Std.HashSet String) (s : state) : Std.HashSet String :=
  s.gates.fold (init := h) fun vs (_s1, _op, _s2, tgt) =>
    if vs.contains tgt then
      dbg_trace "loop detected!"
      vs.insert tgt
    else
      vs.insert tgt

def mkVisited (s : state) : Std.HashSet String :=
  s.values.fold (init := ∅) fun h inp tf => if tf.isSome then h.insert inp else h

structure VisState where
  s : state
  visited : Std.HashSet String := mkVisited s

def inputToVisState (dat : Array String) : VisState where
  s := inputToState dat

def visitedOnce (h : VisState) : Option VisState :=
  if h.visited.contains "loop" then none else
  let (unusedGates, visited) :=
    h.s.gates.fold (init := (h.s.gates, h.visited)) fun (gs, vs) g@(_s1, _op, _s2, tgt) =>
      if vs.contains tgt then
        dbg_trace "loop detected!"
        ({("loop", "op", "loop", "tgt")}, {"loop"})
      else
        (gs.erase g, vs.insert tgt)
  some {h with
    s := {h.s with gates := unusedGates}
    visited := visited}

-- contains also all possible values, but maybe we never reach them
--def mkVisited (s : state) : Std.HashSet String × Std.HashSet String :=
--  (s.values.fold (init := ∅) fun h inp tf => if tf.isSome then h.insert inp else h,
--   s.values.fold (init := ∅) fun h inp _ => h.insert inp)

#eval do
  let dat := atest2
  let dat := atest1
  let dat ← IO.FS.lines input
  let mut vs := inputToVisState dat
  let mut oldVs := ∅
  let mut con := 0
  while oldVs != vs.visited && con ≤ 10 do
    con := con + 1
    IO.println s!"Step {con} starting with {vs.s.gates.size} gates and {vs.visited.size} visited"
    oldVs := vs.visited
    match visitedOnce vs with
      | none => IO.println "Looped";
      | some newVs => vs := newVs
  IO.println s!"gates:\n{vs.s.gates.toArray}\n\nvisited\n{vs.visited.toArray}\n"

def state.swap (s : state) (a b : String) : state :=
  {s with
    gates := s.gates.fold (init := s.gates) fun h vs@(s1, op, s2, tgt) =>
      if tgt == a then
        --dbg_trace "swapping '{a}'"
        (h.erase vs).insert (s1, op, s2, b)
      else
      if tgt == b then
        --dbg_trace "swapping '{b}'"
        (h.erase vs).insert (s1, op, s2, a)
      else h}

#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let dat := atest1
  let s := inputToState dat
  showGates <| s
  showGates <| s.swap "z00" "z01"
  --showState <| swap (runOnce s) "" ""

def swappable (s : state) (a b : String) : Bool := Id.run do
  let s := s.swap a b
  let mut visited : Std.HashSet String := s.values.fold (init := ∅) fun h inp tf =>
    if tf.isSome then h.insert inp else h
  dbg_trace "visited:{"\n".intercalate <| ""::(visited.toArray.qsort (· < ·)).toList.map (s!"{·}")}"
  return default

#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let dat := atest1
  let s := inputToState dat
  IO.println <| swappable (runOnce s) "" ""

#eval (List.range 4 |>.map (312 - 2 * · |>.binom 2)).prod

#eval do
  let dat := atest2
  let dat := atest1
  let dat ← IO.FS.lines input
  let mut vs := inputToVisState dat
  dbg_trace "pool of {vs.s.values.size} values"
  --let pairs := #[("tvp", "z17"), ("vpd", "kbk"), ("gps", "gvw"), ("gvj", "qdf"), ("kmd", "mmc"), ("x00", "vng")]
  let pairs := #[("vng", "vng")]
  let mut swaps := vs.s
  for (l, r) in pairs do
    swaps := swaps.swap l r
  vs := {vs with s := swaps}
  let mut oldVs := ∅
  let mut con := 0
  while oldVs != vs.visited && con ≤ 10 do
    con := con + 1
    --IO.println s!"Step {con} starting with {vs.s.gates.size} gates and {vs.visited.size} visited"
    oldVs := vs.visited
    match visitedOnce vs with
      | none => IO.println "Looped";
      | some newVs => vs := newVs
  IO.println s!"\ngates:\n{vs.s.gates.toArray}\n\nvisited:\n{vs.visited.toArray}"




/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day24
