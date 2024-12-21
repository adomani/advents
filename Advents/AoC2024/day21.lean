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
  | p => panic s!"Not expecting '{p}' as a position!"

def charToPos : Char → pos
  | '^' => (0, 1)
  | 'A' => (0, 2)
  | '<' => (1, 0)
  | 'v' => (1, 1)
  | '>' => (1, 2)
  | '·' => (0, 0)
  | p => panic s!"Not expecting '{p}' as a character!"

def charToMove : Char → pos
  | '^' => (- 1,   0)
  | 'A' => (  0,   0)
  | '<' => (  0, - 1)
  | 'v' => (  1,   0)
  | '>' => (  0,   1)
  | '·' => (0, 0)
  | p => panic s!"Not expecting '{p}' as a character!"

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

/--
info: --012-
0|789|
1|456|
2|123|
3|·0✅️|
--012-

value: 'A', S: (3, 2)
-/
#guard_msgs in
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

/--
info: --0123-
0|·^✅️|
1|<v>|
--0123-

value: 'A', S: (0, 2)
-/
#guard_msgs in
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

/--
info: <<vA
v<<A
<v<A
-/ -- >-/
#guard_msgs in
#eval do
  for p in findPaths (1, - 2) default do
    IO.println p

def mkString (mv : pos) : String :=
  let rep :=
              List.replicate mv.2.natAbs (posToChar (0, mv.2.sign))
               ++
              List.replicate mv.1.natAbs (posToChar (mv.1.sign, 0))
  (⟨rep⟩ : String).push 'A'

structure KP where
  keys : Std.HashMap Char pos
  S : pos
  deriving Inhabited

def mkNum (p : Option pos := none) : KP :=
  let keys := Std.HashMap.union {('A', (3, 2)), ('0', (3, 1))} <|
    (Array.range 9).foldl (init := (∅ : Std.HashMap Char pos)) fun h n =>
        h.insert (s!"{n + 1}".get 0) (2 - n.cast / 3, 2 - (8 - n.cast) % 3)
  {keys := keys, S := p.getD keys['A']!}

def mkDir (p : Option pos := none) : KP :=
  let keys :=    { ('^', (0, 1)), ('A', (0, 2)),
    ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2)) }
  {keys := keys, S := p.getD keys['A']!}

def validate (n : KP) (s : String) : Bool := Id.run do
  let mut curr := n.S
  for si in s.toList do
    curr := curr + charToMove si --n.keys[si]!
    if (n.keys.filter fun _c (q : pos) => curr == q).isEmpty then return false
    --let si := n.keys[s.get ⟨i⟩]!
    --tot := tot + vecLth (charToPos prev - charToPos si)
    --prev := si
  return true



/--
Converts a string, such as "029A", into all the strings of instructions, starting from the
position recorded in `n` and the continuing with the ones in `s`.
-/
def strToPaths (n : KP) (s : String) : Std.HashSet String := Id.run do
  let mut start := n.S
  let mut fin : Std.HashSet String := {""}
  for c in s.toList do
    let tgt := n.keys[c]!
    let next := findPaths tgt start
    --dbg_trace "{start} -- {tgt}, {next.toArray}"
    fin :=  fin.fold (init := ∅) fun h st => h.union (next.fold (·.insert <| st ++ ·) ∅)
    start := tgt
  return fin.filter (validate n)

/-- info:
'<A^A^>^AvvvA'
'<A^A^^>AvvvA'
'<A^A>^^AvvvA'
-/
#guard_msgs in
#eval do
  let str := "029A"
  let steps := strToPaths mkNum str
  for s in steps do
    IO.println s!"'{s}'"

def vecLth (p : pos) : Nat := p.1.natAbs + p.2.natAbs

def lth (s : String) : Nat := Id.run do
  let mut tot := 0
  let mut prev := s.get 0
  for i in [1:s.length] do
    let si := s.get ⟨i⟩
    tot := tot + vecLth (charToPos prev - charToPos si)
    prev := si
  return tot

def nextDirLayer (currLayer : Std.HashSet String) : Std.HashSet String :=
  let nextLayer : Std.HashSet String :=
    currLayer.fold (init := ∅) fun h f =>
      let thirdLayer := strToPaths mkDir f
      h.union thirdLayer
  let vals : Std.HashSet Nat :=
    nextLayer.fold (init := (∅ : Std.HashSet Nat)) (·.insert <| String.length ·)
  let minT := vals.fold min vals.toArray[0]!
  nextLayer.filter (!minT < ·.length)

def repeatDirLayers (currLayer : Std.HashSet String) : Nat → Std.HashSet String
  | 0 => currLayer
  | n + 1 => repeatDirLayers (nextDirLayer currLayer) n

def mkNlayers (str : String) (start : pos) (n : Nat) : Std.HashSet String :=
  let firstLayer := strToPaths (mkNum start) str
  repeatDirLayers firstLayer n

def minOne (seed : String) (n : Nat := 2) : Nat :=
  let (_, mins) := seed.toList.foldl (init := (mkNum.S, 0)) fun (start, mins) i =>
    let str : String := ⟨[i]⟩
    let thL := mkNlayers str start n
    let vals : Std.HashSet Nat :=
      thL.fold (init := (∅ : Std.HashSet Nat)) (·.insert <| String.length ·)
    let min := vals.fold (init := vals.toArray[0]!) (fun m v => min m v)
    (mkNum.keys[i]!, mins + min)
  mins

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  dat.foldl (init := 0) fun tally d =>
    let newMin := minOne d
    tally + newMin * d.getNats[0]!

#assert part1 atest == 126384

set_option trace.profiler true in solve 1 197560

/-
#eval do -- 126384
  let dat ← IO.FS.lines input
  let dat := atest
  let mut tally := 0
  for d in dat do
    let newMin := minOne d
    tally := tally + newMin * d.getNats[0]!
  IO.println tally
-/

def increaseMultiplicitiesOld (base : Std.HashMap String Nat) (currLayer : Std.HashSet String) :
    Std.HashMap String Nat :=
  currLayer.fold (init := base) fun h s =>
    let parts := s.splitOn "A" |>.map (·.push 'A')
    parts.foldl (init := h) fun h'' p =>
      h''.alter p (some <| ·.getD 0 + 1)

def increaseMultiplicitiesSemiOld (base : Std.HashMap String Nat) : Std.HashMap String Nat :=
  let new := nextDirLayer <| base.fold (init := ∅) fun h p _ => h.insert p
  new.fold (init := ∅) fun h s =>
    let parts := s.splitOn "A" |>.map (·.push 'A')
    parts.foldl (init := h) fun h'' p =>
      h''.alter p (some <| ·.getD 0 + 1)
    --h'

def showMults (mults : Std.HashMap String Nat) : IO Unit := do
  let mut tots := 0
  for (s, val) in mults do
    IO.println s!"{val} × {s}"
    tots := tots + val -- * s.length
  IO.println s!"---\n{tots} total\n"
  --{mults.fold (init := 0) (fun h (s : String) m => h + (s.length - 1) * m)} total\n"

def mkMultiplicities (base : String) : Std.HashMap String Nat := Id.run do
  let mut fin : Std.HashMap String Nat := ∅
  let parts := (base).splitOn "A" |>.map ("A" ++ ·.push 'A')
  for q in parts do
    fin := fin.alter q (some <| ·.getD 0 + 1)
  return fin.alter "AA" (some <| ·.getD 0 - 1)
  --return fin.fold (init := ∅) fun h (s : String) m => h.alter (s.drop 1) (some <| ·.getD 0 + m)

#eval do
  IO.println <| (strToPaths mkDir "<A").toArray
  IO.println <| (strToPaths mkDir "<A").contains "v<<A>>^A"

def increaseMultiplicities (base : Std.HashMap String Nat) : Std.HashMap String Nat := Id.run do
  let mut fin := ∅
  for (s, mult) in base do
    --let pieces := nextDirLayer {s.drop 1}
    --dbg_trace s.drop 1
    let pieces := strToPaths mkDir (s.drop 1) --nextDirLayer {s.drop 1} --(s.drop 1)
    dbg_trace "pieces: {pieces.toArray}"
    for p in [pieces.toArray[pieces.toArray.size - 4]?.getD pieces.toArray[0]!] do
      let parts := p.splitOn "A" |>.map ("A" ++ ·.push 'A')
      for q in parts do
        fin := fin.alter q (some <| ·.getD 1 + mult) --((base.getD s 1) * mult)
  return fin --.alter fin "AA" (some <| ·.getD 0 - 1)

/-
1 × <v<A
1 × >>^A
1 × <A
1 × vA
1 × ^<A
2 × >A
1 × <vA
1 × ^>A

3 × A


---
16 total

3 × AA
2 × A>A
1 × A<A
1 × AvA
1 × A^<A
1 × A>>^A
1 × A^>A
1 × A<v<A
1 × A<vA
---
28 total >
-/

def findPair (str : String) : String := Id.run do
  let firstLayer := strToPaths mkDir str
  let later := repeatDirLayers firstLayer 1
  for f in firstLayer do
    for cand in strToPaths mkDir f do
--      for cand1 in strToPaths mkDir cand do
        if later.contains cand then return f
  return ""

def insertString (h : Std.HashMap String Nat) (s : String) : Std.HashMap String Nat := Id.run do
  let mut arr := h
  for i in [0:s.length - 1] do
    let newString : String := ⟨[s.get ⟨i⟩, s.get ⟨i + 1⟩]⟩
    arr := arr.alter newString (some <| ·.getD 0 + 1)
  return arr

def increaseOne (reps : Std.HashMap String Nat) (memo : Std.HashMap String (Array String)) :
    Std.HashMap String Nat × Std.HashMap String (Array String) := Id.run do
  let mut newReps := ∅
  let mut newMemo := memo
  for (s, rep) in reps do
    match newMemo[s]? with
      | none =>
        let newVal := findPair s
        dbg_trace "{s}, newVal: {newVal}"
        let mut arr := #[]
        for i in [0:newVal.length - 1] do
          let newString : String := ⟨[newVal.get ⟨i⟩, newVal.get ⟨i + 1⟩]⟩
          arr := arr.push newString
          newReps := newReps.alter newString (some <| ·.getD 0 + rep)
        newMemo := newMemo.insert s arr
      | some outs =>
        for newString in outs do
          newReps := newReps.alter newString (some <| ·.getD 0 + rep)
  return (newReps, newMemo)
/-
<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
v<<A>>^A<A>AvA<^AA>A<vAAA>^A
-/ -- >>>>>>>>> '-/
#eval do
  let str := "<A^A>^^AvvvA"
  let mut (mults, memos) := (insertString ∅ ("A" ++ str), {})
  showMults mults
  for i in [0:1] do
    (mults, memos) := increaseOne mults memos
  IO.println mults.toArray
  showMults mults
  IO.println memos.size
  IO.println memos.toArray

-- <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A  -- actual
-- v<A<AA>^>AvAA^<A>A<v<A>>^AvA^A -- computed
#eval do
  --let dat := atest
  let mut start : pos := mkNum.S
  --let mut mins := 0
  let seed := "980A"
  let seed := "029A"
  let firstLayer := mkNlayers seed start 1
  IO.println <| findPair "<A" ++ findPair "^A"
  --let firstLayer := strToPaths mkDir "<<"
  --let later := repeatDirLayers firstLayer 2
  --for f in firstLayer do
  --  for cand in strToPaths mkDir f do
  --    for cand1 in strToPaths mkDir cand do
  --      IO.println <| later.contains cand1

  --IO.println <| firstLayer.toArray --contains "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"
#exit
  let secondLayer := repeatDirLayers firstLayer
  IO.println <| secondLayer.contains "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"

  let mut mults := mkMultiplicities firstLayer.toArray[2]! --|>.alter "A" (some <| ·.getD 0 - 1)
  showMults mults
--  let mut mults := increaseMultiplicities {(firstLayer.toArray[0]!, 1)} --firstLayer
  for i in [0:1] do
  --for (s, val) in mults do
  --  IO.println s!"s: {s}, val: {val}"
    mults := increaseMultiplicities mults --<| nextDirLayer (mults.fold (init := ∅) fun h p _ => h.insert p)
  showMults mults
  --IO.println firstLayer.toArray
  --let dat := atest
  --let seeds := dat
--  let firstLayer := strToPaths (mkNum start) str


def mkMultiplicities (currLayer : Std.HashSet String) : Std.HashMap String Nat :=
  currLayer.fold (init := ∅) fun h s =>
    let parts := s.splitOn "A" |>.map (·.push 'A')
    parts.foldl (init := h) fun h'' p =>
      h''.alter p (some <| ·.getD 0 + 1)
    --h'


--#guard_msgs in
#eval do
  let mut start : pos := mkNum.S
  let mut mins := 0
  let seed := "029A"
  let dat := atest
  let seeds := dat
  IO.println seeds
  for i in seed.toList do
    let str : String := ⟨[i]⟩
    let thL := mkThirdLayer str start
    let vals : Std.HashSet Nat :=
      thL.fold (init := (∅ : Std.HashSet Nat)) (·.insert <| String.length ·)
    IO.println s!"\n{i}: {thL.size}"
    let min := vals.fold (init := vals.toArray[0]!) (fun m v => min m v)
    mins := mins + min
    IO.println s!"thL.size: {thL.size}, min: {min}, {vals.toArray}"
    start := mkNum.keys[i]!
  IO.println s!"\n* Min for {seed}: {mins}"

--#guard_msgs in
#eval do
  let mut digsToMoves : Std.HashMap Nat String := ∅
  for i in [0:0] do
    let str := s!"{i}"
    let firstLayer := strToPaths (mkNum (some (2, 1))) str
    IO.println s!"\n{i}: {firstLayer.toArray}"
    let mut secL : Std.HashSet String := ∅
    let mut thL : Std.HashSet String := ∅
    for f in firstLayer do
      let secondLayer := strToPaths mkDir f
      secL := secL.union secondLayer
    for f in secL do
      let thirdLayer := strToPaths mkDir f
      thL := thL.union thirdLayer
      --IO.println s!"{f}: {secondLayer.toArray.qsort (· < ·)}"
    let vals : Std.HashSet Nat :=
      thL.fold (init := (∅ : Std.HashSet Nat)) (·.insert <| String.length ·)
    let min := vals.fold (init := vals.toArray[0]!) (fun m v => min m v)
    IO.println s!"thL.size: {thL.size}, min: {min}, {vals.toArray}"
  --let str := "<A"
  --let steps := strToPaths mkDir str
  --IO.println <| steps.contains "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"
  --let mut tot := 0
  --IO.println steps.size
  --let mut mn := 80
  --for s in steps do --[steps.toArray[0]!] do
  --  for t in strToPaths mkDir s do
  --    if t.length < mn then IO.println s!"corto {t.length}"
  --    mn := t.length




--#guard_msgs in
#eval do
  let str := "<A^A>^^AvvvA"
  let str := "<A"
  let steps := strToPaths mkDir str
  IO.println <| steps.contains "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"
  let mut tot := 0
  IO.println steps.size
  let mut mn := 80
  for s in steps do --[steps.toArray[0]!] do
    for t in strToPaths mkDir s do
      if t.length < mn then IO.println s!"corto {t.length}"
      mn := t.length

    --for t in strToPaths mkDir s do
    --  IO.println s!"length: {lth t}"
    --let new := strToPaths mkDir s
    --let dists := (s.push 'A').toList.foldl (init := 0) fun t c =>
--      let st :=
      --tot := tot + lth s
    --tot := tot + new.size
    --IO.println <| (new.fold (init := (∅ : Std.HashSet Nat)) (fun (h : Std.HashSet Nat) (st : String) => h.insert st.length) : Std.HashSet Nat).size
  --IO.println tot

#exit

def movesNumOnePath (n : numKP) (s : Array Char) : numKP × String := Id.run do
  let mut n := n
  let mut str := ""
  for c in s do
    let tgt := n.keys[c]!
    let mv := tgt - n.S
    n := {n with S := tgt}
    str := str ++ mkString mv
    --dbg_trace "start: {start}, tgt: {tgt}, mv: {mv}"
  (n, str)

def movesNumOneString (n : numKP) (s : String) : numKP × String := Id.run do
  let mut n := n
  let mut str := ""
  for c in s.toList do
    let tgt := n.keys[c]!
    let mv := tgt - n.S
    n := {n with S := tgt}
    str := str ++ mkString mv
    --dbg_trace "start: {start}, tgt: {tgt}, mv: {mv}"
  (n, str)

#eval do
  let n : numKP := {}
  let strs := findPaths (3, 1) (3, 2)
  let strs := #["029A"]
  dbg_trace strs.toList
  for s in strs do
    let (_, moves) := movesNumOneString n s
    IO.println s!"{moves}"

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




/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day21
