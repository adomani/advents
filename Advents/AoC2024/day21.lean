import Advents.Utils
open Lean

namespace Day21

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day21.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "029A
980A
179A
456A
379A"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def numericKP : String := "+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+"

def directionalKP : String := "    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+"

def answer1 := "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
v<<A>>^A<A>AvA<^AA>A<vAAA>^A
<A^A>^^AvvvA
029A"

def answerAll := "029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"

def posToChar : pos → Char
  | (  0,   1) => '>'
  | (  0, - 1) => '<'
  | (  1,   0) => 'v'
  | (- 1,   0) => '^'
  | (  0,   0) => '·'
  | p => panic s!"Not expecting {p} as a position!"

structure numKP where
  keys : Std.HashMap Char pos := Std.HashMap.union {('A', (3, 2)), ('0', (3, 1))} <|
    (Array.range 9).foldl (init := (∅ : Std.HashMap Char pos)) fun h n =>
        h.insert (s!"{n + 1}".get 0) (2 - n.cast / 3, 2 - (8 - n.cast) % 3)
  S : pos := keys['A']!
  deriving Inhabited

def drawNum (n : numKP) : IO Unit := do
  let val := n.keys.fold (init := #[]) fun h c p => if p == n.S then h.push c else h
  if val.size != 1 then panic "Expecting only one value!"
  let str := "789 456 123 ·0A".replace ("".push val[0]!) checkEmoji --"*"
  let gr := str.splitOn
  draw gr.toArray
  IO.println s!"value: '{val[0]!}', S: {n.S}"

#eval do
  drawNum {}

/--
info: ((3, 2),
 Std.HashMap.ofList [('9', (0, 2)),
  ('8', (0, 1)),
  ('7', (0, 0)),
  ('6', (1, 2)),
  ('5', (1, 1)),
  ('4', (1, 0)),
  ('3', (2, 2)),
  ('2', (2, 1)),
  ('A', (3, 2)),
  ('1', (2, 0)),
  ('0', (3, 1))])
-/
#guard_msgs (whitespace := lax) in
#eval
  let n : numKP := {}
  (n.S, n.keys)

structure dirKP where
  keys : Std.HashMap Char pos := { ('^', (0, 1)), ('A', (0, 2)),
                    ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2)) }
  S : pos := keys['A']!
  deriving Inhabited

def drawDir (n : dirKP) : IO Unit := do
  let val := n.keys.fold (init := #[]) fun h c p => if p == n.S then h.push c else h
  if val.size != 1 then panic "Expecting only one value!"
  let str := "·^A <v>".replace ("".push val[0]!) checkEmoji --"*"
  let gr := str.splitOn
  draw gr.toArray
  IO.println s!"value: '{val[0]!}', S: {n.S}"

#eval do
  drawDir {}

/-
structure keyboard where
  keys : Std.HashMap Char pos
  S : pos := keys['A']!

def nk : keyboard :=
  let keys := Std.HashMap.union {('A', (3, 2)), ('0', (3, 1))} <|
    (Array.range 9).foldl (init := (∅ : Std.HashMap Char pos)) fun h n =>
        h.insert (s!"{n + 1}".get 0) (2 - n.cast / 3, 2 - (8 - n.cast) % 3)
  { keys := keys
    S := keys['A']! }

def dk : keyboard :=
  let keys := { ('^', (0, 1)), ('A', (0, 2)),
                    ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2)) }
  { keys := keys
    S := keys['A']! }

def movesOne (n : keyboard) (c : Char) : numKP × Array Char :=
  let start := n.S
  let tgt := n.keys[c]!
  let mv := tgt - start
  dbg_trace "start: {start}, tgt: {tgt}, mv: {mv}"
  let rep :=  List.replicate mv.1.natAbs (posToChar (mv.1.sign, 0)) ++
              List.replicate mv.2.natAbs (posToChar (0, mv.2.sign))
  ({n with S := tgt}, rep.toArray.push 'A')
-/

partial
def seqs {α} [BEq α] [Hashable α] : List α → List α → Std.HashSet (List α)
  | [], l => {l}
  | l, [] => {l}
  | L@(l::ls), M@(m::ms) =>
    ((seqs ls M).fold (init := (∅ : Std.HashSet (List α))) (·.insert <| l::·)).union <|
      (seqs L ms).fold (init := (∅ : Std.HashSet (List α))) (·.insert <| m::·)
--termination_by fun a b => _ --exact L.length + M.length
--decreasing_by _ --L.length + M.length

def findPaths (p q : pos) : Std.HashSet String :=
  let mv := p - q
  let right := List.replicate mv.2.natAbs (posToChar (0, mv.2.sign))
  let left :=  List.replicate mv.1.natAbs (posToChar (mv.1.sign, 0))
  seqs left right |>.fold (init := ∅) (·.insert <| (⟨·⟩ : String).push 'A')

#eval do
  for p in findPaths (1, - 2) default do
    IO.println p

def mkString (mv : pos) : String :=
  let rep :=
              List.replicate mv.2.natAbs (posToChar (0, mv.2.sign))
               ++
              List.replicate mv.1.natAbs (posToChar (mv.1.sign, 0))
  (⟨rep⟩ : String).push 'A'

def movesNumOne (n : numKP) (c : Char) : numKP × String :=
  let start := n.S
  let tgt := n.keys[c]!
  let mv := tgt - start
  --dbg_trace "start: {start}, tgt: {tgt}, mv: {mv}"
  ({n with S := tgt}, mkString mv)

/--
info: --012-
0|789|
1|456|
2|123|
3|·✅️A|
--012-

value: '0', S: (3, 1)
<A
--012-
0|789|
1|456|
2|1✅️3|
3|·0A|
--012-

value: '2', S: (2, 1)
^A
--0123-
0|78✅️|
1|456|
2|123|
3|·0A|
--0123-

value: '9', S: (0, 2)
>^^A
-/
#guard_msgs in
#eval do
  let mut n : numKP := {}
  for c in "029".toList do
    let (_, cs) := movesNumOne n c
    let tgt := n.keys[c]!
    n := {n with S := tgt}
    drawNum n
    IO.println s!"{cs}"

def movesNum (n : numKP) (s : String) : numKP × String :=
  s.toList.foldl (init := (n, "")) fun (n', arr) c =>
    let (newNum, newStr) := movesNumOne n' c
    (newNum, arr ++ ⟨newStr.toList⟩)

#assert (movesNum {} "029A").2 == "<A^A>^^AvvvA"

#eval do
  let n : numKP := {}
  let str := "029A"
  let (n, cs) := movesNum n str
  --drawNum n
  IO.println (⟨cs.toList⟩ : String)
  --let (n, cs) := movesNumOne n '2'
  --drawNum n
  --IO.println s!"{cs}"
  --let (n, cs) := movesNumOne n '9'
  --drawNum n
  --IO.println s!"{cs}"

def numAll (n : numKP) (s : String) : Array (numKP × String) :=
  default

def movesDirOne (n : dirKP) (c : Char) : dirKP × String :=
  let start := n.S
  let tgt := n.keys[c]!
  let mv := tgt - start
  --dbg_trace "Dir -- start: {start}, tgt: {tgt}, mv: {mv}"
  ({n with S := tgt}, mkString mv)

#eval do
  let str := "<A^A^^>AvvvA"
  let mut n : dirKP := {}
  for i in [0:5] do
    let (n', cs) := movesDirOne n <| str.get ⟨i⟩
    n := n'
    drawDir n
    IO.println s!"'{(⟨cs.toList⟩ : String)}'\n"

def movesDir (n : dirKP) (s : String) : dirKP × String :=
  s.toList.foldl (init := (n, "")) fun (n', arr) c =>
    let (newDir, newArr) := movesDirOne n' c
    (newDir, arr ++ ⟨newArr.toList⟩)

--  < | A |^|A|>| ^||A|
-- v<<A>>^A<A>AvA<^AA>A<vAAA>^A -- copied

--  < | A |^|A|^|| >|A|
-- <<vA>>^A<A>A<AA>vA^A<vAAA>^A -- computed

def convert (n : numKP) (d : dirKP) (str : String) : String :=
  let (_, str) := movesNum n str  -- moves sufficient for the numeric keypad
  let (d, str) := movesDir d str  -- moves sufficient for the first directional keypad
  let (_, str) := movesDir {} str  -- moves sufficient for the second directional keypad
  str

#eval do
  let dat := atest
  for d in dat do
    let conv := convert {} {} d
    IO.println s!"{d}: {conv.length}, {conv}"

#eval do
  let n : numKP := {}
  let d : dirKP := {}
  let str := "029A"
  let (_, str) := movesNum n str
  let (d, str) := movesDir d str
--  let (d, str) := movesDir d str
  let (d, str) := movesDir d str
  --let (d, cs) := movesDir d str
  --drawNum n
  IO.println str
  --let (n, cs) := movesNumOne n '2'
  --drawNum n
  --IO.println s!"{cs}"
  --let (n, cs) := movesNumOne n '9'
  --drawNum n
  --IO.println s!"{cs}"
-- <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A  -- copied
-- <<vAA>A>^AvAA<^A>A<<vA>>^AvA^A<<vA>>^AAvA<A>^A<A>A<<vA>A>^AAAvA<^A>A  -- computed


def operate (k : two) (c : Char) : two :=
  let n := k.num
  let d := k.dir
  {k with

    }

/--
info: ((0, 2),
Std.HashMap.ofList [('>', (1, 2)), ('^', (0, 1)), ('<', (1, 0)), ('v', (1, 1)), ('A', (0, 2))])
-/
#guard_msgs in
#eval
  let n : dirKP := {}
  (n.S, n.keys)




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

end Day21
