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

/-- `test3` is the test string for the problem. -/
def test3 := "x00: 0
x01: 1
x02: 0
x03: 1
x04: 0
x05: 1
y00: 0
y01: 0
y02: 1
y03: 1
y04: 0
y05: 1

x00 AND y00 -> z05
x01 AND y01 -> z02
x02 AND y02 -> z01
x03 AND y03 -> z03
x04 AND y04 -> z04
x05 AND y05 -> z00"

/-- `atest3` is the test string for the problem, split into rows. -/
def atest3 := (test3.splitOn "\n").toArray

structure state where
  /-- `1 ↦ some true`, `0 ↦ some false`, `. ↦ none`. -/
  values : Std.HashMap String (Option Bool)
  /-- 4 entries representing first value, operation, second value, where to store the result. -/
  gates : Std.HashSet (String × String × String × String)

def inputToState (dat : Array String) : state :=
  dat.foldl (init := {values := {}, gates := {}}) fun s i =>
    match i.splitOn with
    | [v, tf] =>
      {s with values := s.values.insert (v.dropRight 1) (tf == "1")}
    | [v1, op, v2, _, tgt] => {s with
        values := [v1, v2, tgt].foldl (init := s.values) fun vs n => (vs.alter n (some <| ·.getD none))
        gates := s.gates.insert  <| if v1 < v2 then (v1, op, v2, tgt) else (v2, op, v1, tgt)

        }
    | [""] => s
    | _ => dbg_trace "oh no!"; s

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

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let s := inputToState dat
  out <| runAll s

#assert part1 atest2 == 2024

solve 1 58639252480880

/-!
#  Question 2
-/

def pad (width n : Nat) : String :=
  let n := s!"{n}"
  let l := width - n.length
  ⟨List.replicate l '0'⟩ ++ n

def natToValues (x : Nat) (s : String) (padl : Nat := 2) (lth : Nat := 45) : List (String × Bool) :=
  let xs := (Nat.toDigits 2 x).reverse
  (List.range lth).map fun i => (s ++ pad padl i, xs.getD i '0' == '1')

def setValues (s : state) (x y : Nat) (padl : Nat := 2) (lth : Nat := 45) : state :=
  {s with
    values := (natToValues x "x" padl lth ++ natToValues y "y" padl lth).foldl (init := ∅)
                fun vs (var, value) => vs.insert var value}

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

partial
def getDownstream (seen : Std.HashSet String) (s : state) : Std.HashSet String :=
  let next := s.gates.fold (init := ∅) fun h (s1, _op, s2, tgt) =>
    if (seen.contains s1 || seen.contains s2) && !seen.contains tgt then h.insert tgt else h
  if next.isEmpty then seen else
  getDownstream (seen.union next) s

def valHash {α} [BEq α] [Hashable α] [Inhabited α] (h : Std.HashSet α) (s : String) : α :=
  match h.toArray with
    | #[a] => a
    | _ => dbg_trace "'{s}' has size {h.size}, not 1"; default

def fc (s : Std.HashSet (String × String × String × String)) : Bool × Array (String × String) :=
  let xy := s.filter fun (s1, _, s2, _) =>
    s1.startsWith "x" && s2.startsWith "y" && s1.drop 1 == s2.drop 1
  let OR := s.filter fun (_, op, _, _) =>
    op == "OR"
  let ANDxs := s.filter fun (s1, op, s2, _) =>
    op == "AND" && s1.startsWith "x" && s2.startsWith "y" && s1.drop 1 == s2.drop 1
  let XORxs := s.filter fun (s1, op, s2, _) =>
    op == "XOR" && s1.startsWith "x" && s2.startsWith "y" && s1.drop 1 == s2.drop 1

  let (xi, _, _, andXtgt) := valHash ANDxs "ANDxs"

  let i :=
    if xi.getNats.length == 1 then xi.getNats[0]! else dbg_trace "{xi.getNats}: no index found!"; 0

  let XORzi := s.filter fun (_, op, _, z) =>
    op == "XOR" && z == "z" ++ (pad 2 i)
  let (s1zi, _, s2zi, _) := valHash XORzi s!"\
    {"\n".intercalate <| (s.filter fun (_, op, _, _) =>
      op == "XOR").toList.map (s!"{·}")}\nXORzi, expecting z{pad 2 i}"

  let XORzisucc := s.filter fun (_, op, _, z) =>
    op == "XOR" && z == "z" ++ (pad 2 i.succ)

  let pair := (s.filter fun (not_x, op, _, not_z) =>
      op == "XOR" && "z" != not_z.take 1 && "x" != not_x.take 1).toArray.map (s!"z{pad 2 i.succ}", ·.2.2.2)

  let (s1zisucc, _, s2zisucc, _) := valHash XORzisucc s!"\
    {"\n".intercalate <| (s.filter fun (not_x, op, _, not_z) =>
      op == "XOR" && "z" != not_z.take 1 && "x" != not_x.take 1).toList.map (s!"(z{pad 2 i.succ}, {·.2.2.2})")}\nXORzisucc, expecting z{pad 2 i.succ}"

  let ANDzi := s.filter fun (s1, op, s2, _) =>
    op == "AND" && s1 == s1zi && s2 == s2zi
  let (_, _, _, tzi) := valHash ANDzi "ANDzi"

  let ANDzisucc := s.filter fun (s1, op, s2, _) =>
    op == "AND" && s1 == s1zisucc && s2 == s2zisucc

  let (orL, _, orR, _) := valHash OR "OR"

  let toPrint := default ==
    if (andXtgt == orL || andXtgt == orR) then default else
      valHash (ANDxs.insert default) s!"\
    \nANDxs{ANDxs.toArray}\n\nANDzi:\n{ANDzi.toArray}\n\nOR:\n{OR.toArray}"

  (s.size == 7 &&
  xy.size == 2 &&
  ANDxs.size == 1 && XORxs.size == 1 &&
  XORzi.size == 1 && XORzisucc.size == 1 &&
  ANDzi.size == 1 && ANDzisucc.size == 1 &&
  OR.size == 1 &&
  (tzi == orL || tzi == orR) && (andXtgt == orL || andXtgt == orR) && toPrint, pair)

/--
info: 1
2
3
4
5
6
7
'(gvw, (XOR, (kgn, vvr)))
XORzisucc, expecting z08' has size 0, not 1
Error at 7
8
'(ggf, (XOR, (vvr, z09)))
(gvw, (XOR, (kgn, vvr)))
(x08, (XOR, (y08, kgn)))
XORzi, expecting z08' has size 0, not 1
'ANDzi' has size 0, not 1
Error at 8
9
10
11
12
13
14
15
(gms, (OR, (ngf, pvv)))
(x15, (XOR, (y15, sfc)))
(sdv, (XOR, (sfc, z15)))
(x15, (AND, (y15, gms)))
(sdv, (AND, (sfc, ngf)))
(pvv, (XOR, (rnq, z16)))
(pvv, (AND, (rnq, mcc)))
16
'
ANDxs#[(x16, (AND, (y16, rnq)))]

ANDzi:
#[(pvv, (AND, (rnq, mcc)))]

OR:
#[(bkr, (OR, (mcc, kbg)))]' has size 2, not 1
Error at 16
17
18
19
20
21
22
23
24
25
26
27
'(djn, (XOR, (ptk, tfb)))
XORzisucc, expecting z28' has size 0, not 1
Error at 27
28
'(djn, (XOR, (ptk, tfb)))
(gwp, (XOR, (vst, z29)))
(x28, (XOR, (y28, ptk)))
XORzi, expecting z28' has size 0, not 1
'ANDzi' has size 0, not 1
'
ANDxs#[(x28, (AND, (y28, z28)))]

ANDzi:
#[]

OR:
#[(gws, (OR, (tfb, vst)))]' has size 2, not 1
Error at 28
29
30
31
32
33
34
35
36
37
38
'(thk, (XOR, (wnk, mqh)))
XORzisucc, expecting z39' has size 0, not 1
Error at 38
39
'(thk, (XOR, (wnk, mqh)))
(gqd, (XOR, (jds, z40)))
(x39, (XOR, (y39, wnk)))
XORzi, expecting z39' has size 0, not 1
'ANDzi' has size 0, not 1
Error at 39
40
41
42
43
-/
#guard_msgs in
#eval do
  let dat ← IO.FS.lines input
  let pairs := #[] --#[("vvr", "z08"), ("tfb", "z28"), ("mqh", "z39"), ("rnq", "bkr")]
  let mut swaps := inputToState dat
  for (l, r) in pairs do
    swaps := swaps.swap l r
  let x := "y"
  for i in [1:44] do
--  for i in [14:15] do
    IO.println i
    let prev := getDownstream {x ++ pad 2 i} swaps
    let curr := getDownstream {x ++ pad 2 (i + 1)} swaps
    let onlyPrev := prev.filter (!curr.contains ·)
    --let onlyCurr := curr.filter (!prev.contains ·)
    let mut overlap : Std.HashSet _ := ∅
    for s in onlyPrev do
      overlap := overlap.union <| (swaps.gates.filter fun ((s1, _, s2, _): String × String × String × String) =>
        s1 == s || s2 == s)
    let (err?, pair) := fc overlap
    if ! err? then
      IO.println s!"Error at {i}"
    if i == 15 then
      for o in overlap do IO.println s!"{o}"

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day24
