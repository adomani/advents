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
      | none, _, _ | _, none, _ => vs -- dbg_trace "runOnce error!"; vs -- the check can fail in part 2
      | some none, _, _ | _, some none, _ => vs
      | some (some s1), some (some s2), op => vs.insert tgt <| some ((oper op) s1 s2)
      --| _, _, _ => dbg_trace "runOnce error!"; vs
  {s with values := newValues}

def runAll (s : state) : state := Id.run do
  let mut s := s
  while s.values.valuesArray.contains none do
    s := runOnce s
  s

def run2 (s : state) (zs : Std.HashSet String) : state := Id.run do
  let mut s := s
  while !(zs.filter (! s.values.keysArray.contains ·)).isEmpty do
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

def showOut (s : state) (op : Nat → Nat → Nat := (· + ·)) : IO Unit := do
  let mut tots := #[]
  for xyz in #["x", "y", "z"] do
    let xs := s.values.filterMap fun str bl => if str.startsWith xyz then bl else none
    tots := tots.push <| toNum <| xs.toArray.qsort (·.1 < ·.1) |>.map (·.2)
  let sum := op tots[0]! tots[1]!
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

def act (s : state) (x y : Nat) (padl : Nat := 2) (lth : Nat := 45) : state :=
  runAll <| setValues s x y padl lth

#eval do
  let dat := atest3
  showState <| act (inputToState dat) 12 5 2 6

#eval do
  for dat in [atest1, atest2, ← IO.FS.lines input] do
    showState <| act (inputToState dat) 12 0

def showBinOut (s : state) : IO Unit := do
  let mut tots := #[]
  for xyz in #["x", "y", "z"] do
    let xs := s.values.filterMap fun str bl => if str.startsWith xyz then bl else none
    tots := tots.push <| toNum <| xs.toArray.qsort (·.1 < ·.1) |>.map (·.2)
  let sum := tots[0]! + tots[1]!
  match tots with
    | #[x, y, z] =>
      IO.println s!"{if sum == tots[2]! then checkEmoji else crossEmoji} {x} + {y} = {sum}, computed {z}\n\
              x.rev = {Nat.toDigits 2 tots[0]! |>.reverse}\n\
              y.rev = {Nat.toDigits 2 tots[1]! |>.reverse}\n\
              z.rev = {Nat.toDigits 2 tots[2]! |>.reverse}\n\
              sum.rev {Nat.toDigits 2 sum      |>.reverse}"
    | _ => IO.println "Something went wrong"

#eval do
  for dat in [atest1, atest2, ← IO.FS.lines input] do
    showBinOut <| runAll (inputToState dat)

#eval do
  let dat ← IO.FS.lines input
  let dat := atest3

  for i in [0:10] do
    showBinOut <| act (inputToState dat) 0 i 1

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

#eval do
  let dat := atest3
  let pairs := #[("z05", "z00"), ("z02", "z01")]
  let mut swaps := inputToState dat
  for (l, r) in pairs do
    swaps := swaps.swap l r
  swaps := setValues swaps 1 2 2 6
  --showOut (run2 swaps) (· &&& ·)
  --showState <| swaps
  --showState <| run2 swaps
  let zs : Std.HashSet String :=
    swaps.gates.fold (init := ∅) fun h ((s1, _, s2, tgt) : String × String × String × String) =>
      let foundZs := #[s1, s2, tgt].filter (String.startsWith · "z")
      h.insertMany foundZs
  for i in [0:2^5] do
    for j in [0:2^5] do
      swaps := setValues swaps i j 2 6
      let finState := run2 swaps zs
      --showOut finState (· &&& ·)
      let fin := out finState
      if i &&& j != fin then
        IO.println s!"\n{(i, j)} AND: {i &&& j}, computed: {fin}"

/-!
-/


--/-!
/--
info: ✅️ (x, y, z) = (34359738368, 34359738368, 68719476736), sum: 68719476736
35
✅️ (x, y, z) = (68719476736, 68719476736, 137438953472), sum: 137438953472
36
✅️ (x, y, z) = (137438953472, 137438953472, 274877906944), sum: 274877906944
37
❌️ (x, y, z) = (274877906944, 274877906944, 1099511627776), sum: 549755813888
38
✅️ (x, y, z) = (549755813888, 549755813888, 1099511627776), sum: 1099511627776
39
-/
#guard_msgs in
#eval do
  let dat ← IO.FS.lines input
  --let pairs := #[("z05", "z00"), ("z02", "z01")]
  let pairs := #[("z09", "z08"), ("bkr", "rnq"), ("z29", "z28")] --#[("z09", "z08"), ("z29", "z28"), ("bkr", "rnq")]
  let mut swaps := inputToState dat
  for (l, r) in pairs do
    swaps := swaps.swap l r
  swaps := setValues swaps 1 2 --2 6
  --showOut (run2 swaps) (· &&& ·)
  --showState <| swaps
  --showState <| run2 swaps
  let zs : Std.HashSet String :=
    swaps.gates.fold (init := ∅) fun h ((s1, _, s2, tgt) : String × String × String × String) =>
      let foundZs := #[s1, s2, tgt].filter (String.startsWith · "z")
      h.insertMany foundZs
  for i' in [0:45] do
--  for i' in [0:46] do
    --for j' in [0:41] do
--  for i in [100000 + 10:100000 + 20] do
--    for j in [100000 + 10:100000 + 20] do
      let i := 1 + 2 ^ i'
      let j := 1 + 2 ^ (i' + 0 * 1)
      swaps := setValues swaps i j --2 6
      let finState := run2 swaps zs
      showOut finState
      IO.println i'
      --let fin := out finState
      --if i + j != fin then
      --  IO.println s!"\n{(i, j)} sum: {i + j}, computed: {fin}"

/-!
-/
--/

abbrev TT : Std.HashSet String := {
    "x35", "y35", "qgs", "jgr", "ckr", "mjn", "z35", "bpq", "bbk", "crp", "x36", "y36",
    "fcn", "z36", "kbk", "pgn", "thk", "wnk", "mqh", "x37", "y37", "qsw", "hbw", "z37",
    "jjj", "fkp", "x38", "y38", "fdd", "fpw", "z38", "z39"}

partial
def getDownstream (seen : Std.HashSet String) (s : state) : Std.HashSet String :=
  let next := s.gates.fold (init := ∅) fun h (s1, _op, s2, tgt) =>
    if (seen.contains s1 || seen.contains s2) && !seen.contains tgt then h.insert tgt else h
  if next.isEmpty then seen else
  getDownstream (seen.union next) s

#eval Nat.factors 131072
#eval Nat.factors 549755813888

def checkOne (s : state) (i : Nat) : Array Nat :=
  let x := "x"
  let prev := getDownstream {x ++ pad 2 i} s
  let curr := getDownstream {x ++ pad 2 (i + 1)} s
  let onlyPrev := prev.filter (!curr.contains ·)
  let onlyCurr := curr.filter (!prev.contains ·)
  let overlap : Std.HashSet _ := onlyPrev.fold (init := ∅) fun overlap str =>
    overlap.union <| (s.gates.filter fun (s1, _op, s2, tgt) => s1 == str || s2 == str || s2 == tgt)
  let (tally, mults) : Std.HashMap String Nat × Array Nat := overlap.fold (init := (∅, #[]))
    fun (h, rest) (s1, op, s2, tg) =>
      let r :=
        if op == "AND" && (s1 == tg || s2 == tg) then #[1] else #[0] ++
        if op == "XOR" && (s1 == tg || s2 == tg) then #[1] else #[0] ++
        if op == "OR" && (s1 == tg || s2 == tg) then #[1] else #[0]
      (h  |>.alter s1         (some <| ·.getD 0 + 1)
          |>.alter s2         (some <| ·.getD 0 + 1)
          |>.alter op         (some <| ·.getD 0 + 1)
          |>.alter tg         (some <| ·.getD 0 + 1)
          |>.alter (op ++ s2) (some <| ·.getD 0 + 1)
          |>.alter (op ++ s1) (some <| ·.getD 0 + 1)
          |>.alter (tg ++ s2) (some <| ·.getD 0 + 1)
          |>.alter (tg ++ s1) (some <| ·.getD 0 + 1),
      rest ++ r)
  let withCurr : Std.HashMap String Nat := onlyCurr.fold (init := tally) fun h s =>
    h.alter s (some <| ·.getD 0 + 1)
  let ands : Std.HashMap String Nat := s.gates.fold (init := ∅) fun h (s1, op, s2, tg) =>
    if op == "OR" then
      h |>.alter tg (some <| ·.getD 0 + 1)
        |>.alter s1 (some <| ·.getD 0 + 1)
        |>.alter s2 (some <| ·.getD 0 + 1)
    else h
  let tgZ : Std.HashMap String Nat := s.gates.fold (init := ∅) fun h (s1, op, s2, tg) =>
    if s1.startsWith "x" && op == "OR" then
      h |>.alter tg (some <| ·.getD 0 + 1)
        |>.alter s1 (some <| ·.getD 0 + 1)
        |>.alter s2 (some <| ·.getD 0 + 1)
    else h
  tgZ.fold (fun h _ m => h.push m) #[] |>.qsort (· < ·)
  --(tally.fold (fun h _ m => h.push m) #[] |>.qsort (· < ·)) ++ --mults
  --  (withCurr.fold (fun h _ m => h.push m) #[] |>.qsort (· < ·)) --++ mults
/--
info:
-- y40, y41: 6 3
AND: gqd jds = wdg
AND: x40 y40 = vnr
AND: jmm qvf = pgm
OR: vnr wdg = jmm
XOR: jmm qvf = z41
XOR: x40 y40 = gqd
XOR: gqd jds = z40

-- y41, y42: 6 3
AND: x41 y41 = gmm
AND: jmm qvf = pgm
AND: jdf nmw = cvn
OR: gmm pgm = nmw
XOR: jmm qvf = z41
XOR: x41 y41 = qvf
XOR: jdf nmw = z42

-- y42, y43: 6 3
AND: jdf nmw = cvn
AND: x42 y42 = nnn
AND: gdc ncj = pbj
OR: cvn nnn = gdc
XOR: jdf nmw = z42
XOR: gdc ncj = z43
XOR: x42 y42 = jdf
---
warning: unused variable `op`
note: this linter can be disabled with `set_option linter.unusedVariables false`
---
warning: unused variable `tgt`
note: this linter can be disabled with `set_option linter.unusedVariables false`
-/
#guard_msgs in
#eval do
  let dat ← IO.FS.lines input
  let pairs := #[("z09", "z08"), ("z29", "z28"), ("bkr", "rnq")] ----, ("bkr", "kbg")
  let mut swaps := inputToState dat
  for (l, r) in pairs do
    swaps := swaps.swap l r
  swaps := setValues swaps 1 2 --2 6
  let x := "y"
  for i in [0:45] do
    --let prev := getDownstream {x ++ pad 2 i} swaps
    --let curr := getDownstream {x ++ pad 2 (i + 1)} swaps
    let co := checkOne swaps i
    if #[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] != co then IO.println <| s!"Error! {i}: {co}"
#exit
    let onlyPrev := prev.filter (!curr.contains ·)
    let onlyCurr := curr.filter (!prev.contains ·)
    if (onlyPrev.size, onlyCurr.size) != (6, 3) then
      IO.println s!"WARNING: {(onlyPrev.size, onlyCurr.size)} should be (6, 3)"
    IO.println s!"\n-- {x ++ pad 2 i}, {x ++ pad 2 (i + 1)}: {onlyPrev.size} {onlyCurr.size}"
    let mut overlap : Std.HashSet _ := ∅
    for s in onlyPrev do
      overlap := overlap.union <| (swaps.gates.filter fun ((s1, op, s2, tgt): String × String × String × String) =>
        s1 == s || s2 == s)
    for (s1, op, s2, tgt) in overlap.toArray.qsort (·.2.1 < ·.2.1) do
      IO.println s!"{op}: {s1} {s2} = {tgt}"
    --IO.println s!"ds {"x" ++ pad 2 i} only: {      (prev.filter (!curr.contains ·)).toArray}"
    --IO.println s!"ds {"x" ++ pad 2 (i + 1)} only: {(curr.filter (!prev.contains ·)).toArray}"


#eval do
  let dat ← IO.FS.lines input
  --let pairs := #[] --#[("z05", "z00"), ("z02", "z01")]
  let pairs := #[("z09", "z08"), ("z29", "z28"), ("bkr", "kbg")]
  let mut swaps := inputToState dat
  for (l, r) in pairs do
    swaps := swaps.swap l r
  swaps := setValues swaps 1 2 --2 6
  --showOut (run2 swaps) (· &&& ·)
  --showState <| swaps
  --showState <| run2 swaps
  IO.println s!"ds x38: {(getDownstream {"x38"} swaps).toArray.qsort (· < ·)}"
  IO.println s!"ds x39: {(getDownstream {"x39"} swaps).toArray.qsort (· < ·)}"
  let zs : Std.HashSet String :=
    swaps.gates.fold (init := ∅) fun h ((s1, _, s2, tgt) : String × String × String × String) =>
      let foundZs := #[s1, s2, tgt].filter (String.startsWith · "z")
      h.insertMany foundZs
  for i' in [2 ^ 46 - 1] do
    --for j' in [0:41] do
--  for i in [100000 + 10:100000 + 20] do
--    for j in [100000 + 10:100000 + 20] do
      let i := i'
      let j := i'
      swaps := setValues swaps i j --2 6
      let finState := run2 swaps zs
      showBinOut finState
      showOut finState
      --let fin := out finState
      --if i + j != fin then
      --  IO.println s!"\n{(i, j)} sum: {i + j}, computed: {fin}"
/-!
-/
#eval 2 ^ 16

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
