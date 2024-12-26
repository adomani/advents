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

def dirToChar : pos → Char
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

def numKeys : Std.HashMap Char pos := .union {('A', (3, 2)), ('0', (3, 1))} <|
    (Array.range 9).foldl (init := (∅ : Std.HashMap Char pos)) fun h n =>
        h.insert (s!"{n + 1}".get 0) (2 - n.cast / 3, 2 - (8 - n.cast) % 3)

def drawNum (n : pos) : IO Unit := do
  let val := numKeys.fold (init := #[]) fun h c p => if p == n then h.push c else h
  if val.size != 1 then panic "Expecting only one value!"
  let str := "789 456 123 ·0A".replace ("".push val[0]!) checkEmoji --"*"
  let gr := str.splitOn
  draw gr.toArray
  IO.println s!"value: '{val[0]!}', S: {n}"

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
#eval do drawNum (3, 2)

/--
info: Std.HashMap.ofList [('9', (0, 2)),
 ('8', (0, 1)),
 ('7', (0, 0)),
 ('6', (1, 2)),
 ('5', (1, 1)),
 ('4', (1, 0)),
 ('3', (2, 2)),
 ('2', (2, 1)),
 ('A', (3, 2)),
 ('1', (2, 0)),
 ('0', (3, 1))]
-/
#guard_msgs in #eval numKeys

def dirKeys : Std.HashMap Char pos := {
                   ('^', (0, 1)), ('A', (0, 2)),
    ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2))
  }

def drawDir (n : pos) : IO Unit := do
  let val := dirKeys.fold (init := #[]) fun h c p => if p == n then h.push c else h
  if val.size != 1 then panic "Expecting only one value!"
  let str := "·^A <v>".replace ("".push val[0]!) checkEmoji --"*"
  let gr := str.splitOn
  draw gr.toArray
  IO.println s!"value: '{val[0]!}', S: {n}"

/--
info: --0123-
0|·^✅️|
1|<v>|
--0123-

value: 'A', S: (0, 2)
-/
#guard_msgs in
#eval do
  drawDir (0, 2)

/--
A `keyboard` represents either
* a `num`eric keyboard with buttons `0` through `9` and `A`; or
* a `dir`ectional kyeboard with buttons `<`, `^`, `>`, `v` and `A`.
-/
inductive keyboard where
  | /-- A `num`eric keyboard with buttons `0` through `9` and `A`. -/
    num
  | /-- A `dir`ectional kyeboard with buttons `<`, `^`, `>`, `v` and `A`. -/
    dir

instance : ToString keyboard where toString | .dir => "directional" | .num => "numerical"

/--
`keys k` converts the `keyboard` `k` into its `HashMap` of keys, mapping a character to
the corresponding position.
-/
def keyboard.keys : keyboard → Std.HashMap Char pos
  | .dir => dirKeys | .num => numKeys

def posToChar : keyboard → pos → Char
  | .dir, (0, 1) => '^'
  | .dir, (0, 2) => 'A'
  | .dir, (1, 0) => '<'
  | .dir, (1, 1) => 'v'
  | .dir, (1, 2) => '>'
  | .dir, (0, 0) => '·'

  | .num, (0, 0) => '7'
  | .num, (0, 1) => '8'
  | .num, (0, 2) => '9'
  | .num, (1, 0) => '4'
  | .num, (1, 1) => '5'
  | .num, (1, 2) => '6'
  | .num, (2, 0) => '3'
  | .num, (2, 1) => '2'
  | .num, (2, 2) => '1'
  | .num, (3, 1) => '0'
  | .num, (3, 2) => 'A'

  | d, p => panic s!"posToChar: Not expecting '{p}' as a position on a {d} keyboard!"

partial
def seqs {α} [BEq α] [Hashable α] : List α → List α → Std.HashSet (List α)
  | [], l => {l}
  | l, [] => {l}
  | L@(l::ls), M@(m::ms) =>
    ((seqs ls M).fold (init := (∅ : Std.HashSet (List α))) (·.insert <| l::·)).union <|
      (seqs L ms).fold (init := (∅ : Std.HashSet (List α))) (·.insert <| m::·)
--termination_by fun a b => _ --exact L.length + M.length
--decreasing_by _ --L.length + M.length

def straight (s : String) : String :=
  let new := s |>.replace "^>A" ">^A" |>.replace "<v<" "v<<" |>.replace "vA<^A>A" "<Av>A^A"
  if s != new then
    --dbg_trace "found!"
    new
  else
    new

/--
Computes all paths from `q` to `p` that are minimal with respect to the L¹-distance,
written as strings of instructions `<`, `^`, `>`, `v`.
`findPaths` then returns each such path, with `A` appended at the end.
-/
def findPaths (p q : pos) : Std.HashSet String :=
  let mv := p - q
  let right := List.replicate mv.2.natAbs (dirToChar (0, mv.2.sign))
  let left :=  List.replicate mv.1.natAbs (dirToChar (mv.1.sign, 0))
  seqs left right |>.fold (init := ∅) (·.insert <| straight <| (⟨·⟩ : String).push 'A')

/-- info:
- <<vA
- v<<A
-/
#guard_msgs in
#eval do
  for p in findPaths (1, - 2) default do
    IO.println s!"- {p}"

/-
def mkString (mv : pos) : String :=
  let rep :=
              List.replicate mv.2.natAbs (posToChar (0, mv.2.sign))
               ++
              List.replicate mv.1.natAbs (posToChar (mv.1.sign, 0))
  (⟨rep⟩ : String).push 'A'
-/

structure KP where
  keys : Std.HashMap Char pos
  S : pos
  deriving Inhabited

def mkNum (p : Option pos := none) : KP where
  keys := numKeys
  S := p.getD numKeys['A']!

def mkDir (p : Option pos := none) : KP where
  keys := dirKeys
  S := p.getD dirKeys['A']!

def validate (n : KP) (s : String) : Bool := Id.run do
  let mut curr := n.S
  for si in s.toList do
    curr := curr + charToMove si
    if (n.keys.filter fun _c (q : pos) => curr == q).isEmpty then return false
  return true

/--
Converts a string, such as "029A", into all the strings of instructions
It starts from the position recorded in `n` and then continues with the ones in `s`.
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

def cutBasic2 (s : String) : Std.HashMap String Nat := Id.run do
  let mut fin := ∅
  for i in [1:s.length] do
    let si := "".push (s.get ⟨i - 1⟩) |>.push (s.get ⟨i⟩)
    fin := fin.alter si (some <| ·.getD 0 + 1)
  return fin

#assert (cutBasic2 "123123").toList == [("31", 1), ("12", 2), ("23", 2)]

/-- Prepend the starting button and segment with pieces of length 2. -/
def inputToHash (s : String) : Std.HashMap String Nat :=
  cutBasic2 <| "A" ++ s



def interleaveA (h : Std.HashMap String Nat) : Std.HashMap String Nat :=
  h.fold (init := h) fun h p m =>
    h.alter ((p.take 1).push 'A') (some <| ·.getD 0 + m)
     |>.alter ((p.drop 1).push 'A') (some <| ·.getD 0 + m)

def cut2 (s : String) : Std.HashMap String Nat := Id.run do
  let mut fin := ∅
  for i in [1:s.length - 1] do
    let si := "".push (s.get ⟨i - 1⟩) |>.push (s.get ⟨i⟩)
    fin := fin.alter si (some <| ·.getD 0 + 1)
  return interleaveA fin

def cut2' (s : String) : Std.HashMap String Nat := Id.run do
  let mut fin := ∅
  for i in [0:s.length] do
    --let si1 := "".push (s.get ⟨i - 1⟩) |>.push (s.get ⟨i⟩)
    let si := "".push (s.get ⟨i ⟩) |>.push 'A'
    --let si := "A".push (s.get ⟨i - 1⟩) --|>.push (s.get ⟨i⟩)
    fin := (fin.alter si (some <| ·.getD 0 + 1)) --.alter si1 (some <| ·.getD 0 + 1)
  return fin

def extractMin [BEq α] [Hashable α] (h : Std.HashSet α) (lth : α → Nat) : Std.HashSet α :=
  if let some a := h.toArray.back? then
    let minT := h.fold (fun m n => min m (lth n)) (lth a)
    h.filter (!minT < lth ·)
  else ∅

def mergeMults (h1 h2 : Std.HashMap String Nat) (mult1 : Nat := 1) : Std.HashMap String Nat :=
  h2.fold (init := h1) fun h' p mult => h'.alter p (some <| ·.getD 0 * mult1 + mult)

/-- -/
def moveEdge (h : Std.HashMap String Nat) (s : String) (type : String) :
    Std.HashMap String Nat := Id.run do
  let mut fin := h
  let keys := if type == "num" then numKeys else dirKeys
  match s.toList with
    | [a, b] =>
      --let mut h := h
      --for c in s.toList do
      --  --let tgt := n.keys[c]!
        let next := findPaths keys[b]! keys[a]!

        for n in next do
          dbg_trace "dealing with {n}"
          let parts := cut2 n
          --let mut tot := 0
          for (str, mult) in parts do
            let secL := findPaths dirKeys[str.get ⟨0⟩]! dirKeys[str.get ⟨1⟩]!
            let minS := extractMin secL String.length
            dbg_trace "(str, mult) = {(str, mult)}\nsecL: {(secL.fold (·.insert <| ·.length) (∅ : Std.HashSet Nat)).toArray}"
            dbg_trace ((str, mult), secL.toArray)
            dbg_trace ""
            if minS.size == 0 then fin := fin else continue
          --dbg_trace parts.toArray
        --dbg_trace "{start} -- {tgt}, {next.toArray}"
        --fin :=  fin.fold (init := ∅) fun h st => h.union (next.fold (·.insert <| st ++ ·) ∅)
        --let start := tgt
        dbg_trace next.toArray
        return fin
    | _ => panic s!"Only string of length 2, unlike {s}, please!"

def minWeight (s : String) : Nat × Std.HashMap String Nat := Id.run do
  let mut (minimum, mults) := (1000, ∅)
  match s.toList with
    | [a, b] =>
      --dbg_trace "characters: {(a, b)}"
      let firstL := findPaths dirKeys[b]! dirKeys[a]!
      for pathChoice in firstL do
        let newMults := cutBasic2 pathChoice
        let totMults := newMults.fold (fun t _ m => t + m) 0
        if totMults < minimum then
          (minimum, mults) := (totMults, newMults)
      return (minimum, mults)
    | _ => panic s!"Only paths of length two are allowed, unlike {s}!"


def localMinimizer (s : String) (type : String) : Std.HashMap String Nat := Id.run do
  let keys := if type == "num" then numKeys else dirKeys
  let mut final := ∅
  let parts := cutBasic2 s
--  let mut choices := #[]
  --let mut tot := 0
  --dbg_trace "there are {parts.size} parts {parts.toArray}"
  for (str, mult) in parts do
    --dbg_trace "\n** (str, mult) = ('{str}', {mult})"
    let firstL := findPaths keys[str.get ⟨1⟩]! keys[str.get ⟨0⟩]!
    dbg_trace "Starts: {firstL.fold (fun h s => h.push (s.take 1)) #[]}"
    let mut (minChoice, multChoice) := (1000, ∅)
    let mut firstStepMults := ∅
    for pathChoice in firstL do

      let newMults := cutBasic2 <| pathChoice --mergeMults parts (cut2 pathChoice) mult
      --dbg_trace "'{pathChoice}' newMults summed: {newMults.fold (fun h s m => dbg_trace "within {(minWeight s).1}"; h + m) 0}\n\
      --            {newMults.toArray}"
      let expContribution : Nat × Std.HashMap String Nat :=
        newMults.fold (fun (h, mus) s m => ((minWeight s).1 + h + m, mergeMults mus (minWeight s).2)) (0, ∅)
      if expContribution.1 < minChoice then
        minChoice := expContribution.1
        multChoice := expContribution.2
        firstStepMults := newMults
      --dbg_trace "NEW: {expContribution.1}, {expContribution.2.toArray}"
      --dbg_trace "newMults {newMults.toArray} plus within: {newMults.fold (fun h s m =>
      --  dbg_trace "using {(minWeight s).2.toArray} for {s}"
      --  (minWeight s).1 + h + m) 0}"
      --if newMults.size == 0 then final := newMults else final := newMults
    --dbg_trace "{minChoice} realized via {multChoice.toArray}, with first choice:\n{firstStepMults.toArray}"
    final := mergeMults final firstStepMults mult
--      _
--    let minS := secL --extractMin secL String.length
--    choices := choices.push minS
--    dbg_trace ((str, mult), (secL.fold (·.insert <| ·.length) (∅ : Std.HashSet Nat)).toArray)
--    dbg_trace ((str, mult), secL.toArray)
--    dbg_trace ""
    --if minS.size == 0 then fin := fin else continue
  return final

-- -- v<<A>>^A<A>AvA<^AA>A<vAAA>^A
-- -- <A A^| ^A A>| >^ ^^ ^A Av| vv vv vA
-- -- #[(<A, 1), (^A, 2), (vA, 1), (>^, 1), (vv, 2), (^^, 1)] >> --/



#eval do
  let sts := #["A456A", "A029A"]
  let st := sts[1]!
  let locMin := localMinimizer (st) "num"
  IO.println s!"\ntotal: {locMin.fold (fun h _ m => h + m) 0} from {locMin.toArray}"
  --for (p, mult) in locMin do
  --  dbg_trace "{(p, mult)}"
  --  dbg_trace "{(localMinimizer p "dir").toArray}\n"
  let sec : Std.HashMap String Nat :=
    (locMin).fold (fun h p m => dbg_trace "missing out on {m}"; mergeMults h (localMinimizer p "dir") m) ∅
  IO.println s!"\ntotal: {sec.fold (fun h _ m => h + m) 0} from {sec.toArray}"

  let sec : Std.HashMap String Nat :=
    sec.fold (fun h p m => dbg_trace "missing out on {m}"; mergeMults h (localMinimizer p "jjj") m) ∅

  IO.println s!"\ntotal: {sec.fold (fun h _ m => h + m) 0} from {sec.toArray}"
  --let pths := moveEdge ∅ st "num"
  --IO.println pths.toList --size
  --let pths := strToPaths mkNum st
  --IO.println pths.toList

/--
A `window` has
* a `seed`, the current character, that starts out being `A` and
* a `mv`, a sequence of moves, encoded by a string.
-/
structure window where
  /-- A `seed` is the character representing a button to which the robot is currently pointing.
  It starts out being `A`. -/
  seed : Char := 'A'
  /-- A `mv` is a sequence of moves, encoded by a string.
  It represents the next buttons that the robot should push. -/
  mv : String
  deriving BEq, Hashable

/-- The L¹-length of an integer vector. -/
def length (p : pos) : Nat := p.1.natAbs + p.2.natAbs

def expectedDist (s : String) (start : Char) (type : keyboard) : Nat := Id.run do
  let keys := type.keys
  let mut startPos := keys[start]?.getD (3, 2)
  let mut dist := 0
  for next in s.toList do
    let nextPos := keys[next]!
    dist := dist + length (nextPos - startPos)
    startPos := nextPos
  return dist

def validate' (s : String) (start : Char) (type : keyboard) : Bool := Id.run do
  let keys := type.keys
  let mut curr := keys[start]!
  for si in s.toList do
    curr := curr + charToMove si
    if (keys.filter fun _c q => curr == q).isEmpty then return false
  return true

def switchAt (s : String) (n : Nat) : Bool := s.get ⟨n⟩ != s.get ⟨n + 1⟩

def alts (s : String) : Nat :=
  (Array.range s.length).foldl (· + if switchAt s · then 1 else 0) 0

def minimizeFirstStep (s : Std.HashSet String) (type : keyboard) : Std.HashSet String := Id.run do
  let kb := match type with | keyboard.num => mkNum | _ => mkDir
  let ps := s.fold (init := ∅)
    fun (h : Std.HashSet String) s => h.union (strToPaths kb s)
  let mut min := 10000
  let mut minOnes : Std.HashSet String := ∅
  for p in ps do
    if p.length < min then
      minOnes := {p}
      min := p.length
    if p.length == min then
      minOnes := minOnes.insert p
  return minOnes

def genFirstMin (s : String) (num? : Bool) : String :=
  let ps1 := minimizeFirstStep {s} (if num? then .num else .dir)
  let ps2 := minimizeFirstStep ps1 .dir --|>.toArray[0]!
  let ps3 := minimizeFirstStep ps2 .dir
  ps3.toArray[0]!

def performPushes (w : window) (type : keyboard) : window :=
  let keys := type.keys
  let dirs := w.mv.toList.foldl (init := #[]) (·.push <| charToMove ·)
  dirs.foldl (init := {w with mv := ""})
    fun w' mv =>
      if mv == (0, 0) then {w' with mv := w'.mv.push (posToChar type keys[w'.seed]!)}
      else
        {w' with seed := posToChar type (keys[w'.seed]! + mv)}

def upAndDown (w : window) (k : keyboard) : window :=
  let fmin := genFirstMin w.mv (match k with | .num => true | .dir => false)
  performPushes (performPushes {w with mv := fmin} .dir) .dir

def splitWindow (w : window) : Std.HashMap window Nat := --Id.run do
  --let parts := ((straight w.mv).dropRight 1).splitOn "A"
  --let mut pth := "A"
  --let mut h := ∅
  --for p in parts do
  --  pth := p
  --  let val := performPushes {w with mv := pth.push 'A'} .dir |>.seed
  --  h := h.alter {mv := p.push 'A', seed := val} (some <| ·.getD 0 + 1)
  --dbg_trace "here"
  (( w.mv).dropRight 1).splitOn "A" |>.foldl (·.alter {w with mv := ·.push 'A'} (some <| ·.getD 0 + 1)) ∅
  --return h

#eval do
  let mut tally := 0
  let dat := atest
  let dat ← IO.FS.lines input
  let mut memo : Std.HashMap window (List (window × Nat)) := ∅
  for s in dat do
    --let s := dat[0]!
    let w : window := {mv := s}
    --dbg_trace w.mv
    let first := upAndDown w .num
    --dbg_trace "first step: {first.mv.length} {first.mv}"
    let mut split := splitWindow first
    for i in [0:25] do
      let mut newSplit : Std.HashMap window Nat := ∅
      for (w, m) in split do
        --dbg_trace w.mv
        if !memo.contains w then
          let new := upAndDown w .dir
          memo := memo.insert w (splitWindow new).toList
        newSplit := memo[w]!.foldl (init := newSplit) fun h ((w', m') : window × Nat) =>
              h.alter w' (some <| ·.getD 0 + m' * m)
      split := newSplit
    let fin := split.fold (init := 0) fun h (w : window) n => h + n * (w.mv.length)
    tally := tally + s.getNats[0]! * fin
    IO.println memo.size
    IO.println s!"{s} mults: {fin}"
  IO.println tally

/-!
-/
/-
#assert 142074832574328 == -- too low
964 * 49767208068 +
140 * 50575382072 +
413 * 51834670766 +
670 * 48735351682 +
593 * 55578896886
        110060123381626
        120610857096518
        239384518851636 -- not right

#assert 246822631766548 == -- too high -- misunderstanding: used test file
 29 * 133093870844 +
980 * 115740358202 +
179 * 129556635932 +
456 * 129556635930 +
379 * 124720038676

-/
#eval
964 * 85455922270+ 140 * 85935515972+ 413 * 85762730562+ 670 * 82304569918+ 593 * 91753740270

/-
17
19
22
24
-/
def splitAndStraightenWindow (w : window) : Std.HashMap window Nat :=
  splitWindow (straightenOneWindow w) |>.fold (fun h w mult => h.alter w (some <| ·.getD 0 + mult)) ∅

structure memoW where
  w : window
  memo : Std.HashMap window (List (window × Nat)) := ∅

def performPushesW (w : memoW) (type : keyboard) : memoW :=
  let keys := type.keys
  let dirs := w.w.mv.toList.foldl (init := #[]) (·.push <| charToMove ·)
  dirs.foldl (init := {w with w := {seed := w.w.seed, mv := ""}})
    fun w' mv =>
      if mv == (0, 0) then {w' with w := {mv:= w'.w.mv.push (posToChar type keys[w'.w.seed]!)}}
      else
        {w' with w := {w'.w with seed := posToChar type (keys[w'.w.seed]! + mv)}}

def upAndDown (w : memoW) (k : keyboard) : memoW :=
  let fmin := genFirstMin w.w.mv (match k with | .num => true | .dir => false)
  performPushesW (performPushesW {w with w := {mv := fmin}} .dir) .dir

def splitWindow (w : window) : Std.HashMap window Nat :=
  ((straight w.mv).dropRight 1).splitOn "A" |>.foldl (·.alter {mv := ·.push 'A'} (some <| ·.getD 0 + 1)) ∅

def straightenOneWindow (w : window) : window :=
  {w with mv := straight w.mv}

def splitAndStraightenWindow (w : window) : Std.HashMap window Nat :=
  splitWindow (straightenOneWindow w) |>.fold (fun h w mult => h.alter w (some <| ·.getD 0 + mult)) ∅

def moveWindow (w : memoW) (type : keyboard) : String × memoW :=
  ((upAndDown w type).w.mv, {w with w := {seed := w.w.mv.get 0, mv := w.w.mv.drop 1}})

def wholeRun (w : memoW) (type : keyboard) : String := Id.run do
  let mut (s, w) : String × memoW := ("", w)
  while !w.w.mv.isEmpty do
    --dbg_trace w.mv
    let (s', w') := moveWindow w type
    (s, w) := (s ++ s', w')
  s.replace "<v<" "v<<"

def tallyMoveWindow (h : Std.HashMap window Nat) : Std.HashMap window Nat := Id.run do
  let mut fin := ∅
  for (w, mult) in h do
    let s := wholeRun w .dir
    let pieces := splitAndStraightenWindow {mv := s}
    for (p, m') in pieces do
      fin := fin.alter p (some <| ·.getD 0 + mult * m')
  return fin

#eval do
  let dat := atest
  let mut memo : Std.HashMap window (List (window × Nat)) := ∅
  for s in dat do
    --let fmin : window := {mv := genFirstMin s true}
    let fw : window := {mv := s}

    let fmin := if memo.contains fw then Std.HashMap.ofList memo[fw]! else
      let new := upAndDown fw .num
      let ws := splitWindow new
      ws
    memo := memo.insert fw fmin.toList
    IO.println <| fmin.mv == "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"
    IO.println s!"{fmin.mv.length}: {fmin.mv}"
#exit
def minPath (start tgt : Char) (type : keyboard) : String := Id.run do
  let keys := type.keys
  let startPos := keys[start]!
  let tgtPos := keys[tgt]!
  let paths := findPaths tgtPos startPos |>.filter (validate' · start type)
  --dbg_trace paths.toArray
  let mut (minDist, minPath) := (1000, "")
  let mut minAlts := 1000
  for candPath in paths do
    minAlts := min minAlts (alts candPath)
    --dbg_trace "alts `{candPath}` = {alts candPath}"
    let newDist := expectedDist candPath start .dir
    if newDist ≤ minDist then
      --if !minPath.isEmpty then
      --  dbg_trace "min changes? {(newDist < minDist : Bool)}, oldPath: '{minPath}'"
      (minDist, minPath) := (newDist, candPath)
    --if tgtPos == tgtPos then
    --dbg_trace "expDist candPath {candPath} {expDist candPath}"
    --else continue
--    x := 0
--  dbg_trace x
  --dbg_trace "chosen `{minPath}` with {alts minPath} alternations, minimum: {minAlts}"
  if minAlts < alts minPath then dbg_trace "***************** Oh no!"
  return straight minPath

-- `#[v<<A, <v<A]`
#eval show Elab.Term.TermElabM _ from do
  if let [s, t] := "A<".toList then
    guard <| minPath s t .dir == "v<<A"

-- `#[>>^A, >^>A]`
#eval show Elab.Term.TermElabM _ from do
  if let [s, t] := "<A".toList then
    guard <| minPath s t .dir == ">>^A"

-- `#[<vA, v<A]`
#eval show Elab.Term.TermElabM _ from do
  if let [s, t] := "Av".toList then
    guard <| minPath s t .dir == "v<A"

#eval show Elab.Term.TermElabM _ from do
  if let [s, t] := "A^".toList then
    guard <| minPath s t .dir == "<A"

#eval show Elab.Term.TermElabM _ from do
  let st := "029A"
  let step := wholeRun {mv := st} .num
  guard <| step == "<A^A>^^AvvvA"

#eval show Elab.Term.TermElabM _ from do
  let dat := "<A^A>^^AvvvA"
  let mut (s, w) : String × window := ("", {mv := dat})
  while !w.mv.isEmpty do
    let (s', w') := moveWindow w .dir
    (s, w) := (s ++ s', w')
  guard <| s.length == 28
  guard <| s == "v<<A>>^A<A>AvA<^AA>Av<AAA>^A" -- input `v<<A>>^A<A>AvA<^AA>A<vAAA>^A`
  guard <| s == wholeRun {mv := dat} .dir
  guard <| wholeRun {mv := s} .dir == "v<A<AA>>^AvAA<^A>Av<<A>>^AvA^Av<A>^Av<<A>^A>AAvA^Av<A<A>>^AAAvA<^A>A"
  -- input `<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A`

#eval show Elab.Term.TermElabM _ from do
  let dat := "<A^A>^^AvvvA"
  let mut w : window := {mv := dat}
  let mut lengths := #[]
  for _ in [0:2] do
    let s := wholeRun w .dir
    w := {mv := s}
    lengths := lengths.push w.mv.length
  guard <| lengths == #[28, 68]

/-
029A: 68
`<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A`
`<vA<AA>>^AvAA<^A>Av<<A>>^AvA^A<vA^>Av<<A>^A>AAvA^Av<<A>A^>AAA<Av>A^A`

980A: 60
`<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A`
`v<<A>>^AAAvA^A<vA<AA>>^AvAA<^A>Av<<A>A^>AAA<Av>A^A<vA^>A<A>A`

179A: 68
`<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A`
`v<<A>>^A<vA<A>>^AAvAA<^A>Av<<A>>^AAvA^A<vA^>AA<A>Av<<A>A^>AAA<Av>A^A`

456A: 66
`<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A`
`v<<A>>^A<vA<A>>^AAvA<^A>AvA^A<vA^>A<A>A<vA^>A<A>Av<<A>A^>AA<Av>A^A`

379A: 64
`<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A`
`v<<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA^>AA<A>Av<<A>A^>AAA<Av>A^A`
-/

/-- info:
68: `v<A<AA>>^AvAA<^A>Av<<A>>^AvA^Av<A>^Av<<A>^A>AAvA^Av<A<A>>^AAAvA<^A>A`
60: `v<<A>>^AAAvA^Av<A<AA>>^AvAA<^A>Av<A<A>>^AAAvA<^A>Av<A>^A<A>A`
68: `v<<A>>^Av<A<A>>^AAvAA<^A>Av<<A>>^AAvA^Av<A>^AA<A>Av<A<A>>^AAAvA<^A>A`
64: `v<<A>>^AAv<A<A>>^AAvAA<^A>Av<A>^A<A>Av<A>^A<A>Av<A<A>>^AAvA<^A>A`
64: `v<<A>>^AvA^Av<A<AA>>^AAvA<^A>AAvA^Av<A>^AA<A>Av<A<A>>^AAAvA<^A>A`
-/
#guard_msgs in
#eval show Elab.Term.TermElabM _ from do
  let dat := atest
  let mut tot := 0
  for st in dat do
    let mut step := wholeRun {mv := st} .num
    let mut w : window := {mv := step}
    for _ in [0:2] do
      let s := wholeRun w .dir
      w := {mv := s}
    tot := tot + st.getNats[0]! * w.mv.length
    IO.println <| s!"{w.mv.length}: `{w.mv}`"
  guard <| tot == 126384

/-- info:
1 × `vvvA`
1 × `^A`
1 × `<A`
1 × `>^^A`
28
68
168
426
1072
2708
6834
17272
43630
110230
278466
703530
1777396
4490432
11344588
28660984
72409146
182934610
462166124
1167616914
2949868574
7452551250
18828133304
47567415420
120174367796
-/
#guard_msgs in
#eval do
  let dat := "<A^A>^^AvvvA"
  let mut hw := splitAndStraightenWindow {mv := dat}
  for (w, m) in hw do
    IO.println <| s!"{m} × `{w.mv}`"
  for _ in [0:25] do
    hw := tallyMoveWindow hw
    IO.println <| hw.fold (fun tot (s : window) m => tot + m * String.length s.mv) 0

#eval do
  let is := #[
    "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A",
    "v<<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA^>AA<A>Av<<A>A^>AAAvA<^A>A",
    "v<<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA^>AA<A>Av<<A>A^>AAA<Av>A^A"]
  for a in is do
    let ta := splitAndStraightenWindow {mv := a}
    let ta := tallyMoveWindow ta
    let ta := tallyMoveWindow ta
    let ta := tallyMoveWindow ta
    let ta := tallyMoveWindow ta
    let ta := tallyMoveWindow ta
    IO.println <| ta.fold (fun tot (s : window) m => tot + m * String.length s.mv) 0

def minimizeOne (s : String) (type : keyboard) : String :=
  let ps := strToPaths (mkNum) s
  dbg_trace ps.size
  dbg_trace ps.toArray
  if ps.size == 0 then default else default

#exit
#eval do
  let s := "379A"
  let ps1 := minimizeFirstStep {s} .num
  let ps2 := minimizeFirstStep ps1 .dir
  let ps3 := minimizeFirstStep ps2 .dir
  IO.println ps3.size
  IO.println ps3.toArray[0]!.length
  let type : keyboard := .num
  let ps := strToPaths (match type with | keyboard.num => mkNum | _ => mkDir) s
  let mut min := 1000
  let mut minOnes : Std.HashSet String := ∅
  for p in ps do
    if p.length < min then
      minOnes := {p}
      min := p.length
    if p.length == min then
      minOnes := minOnes.insert p
  IO.println minOnes.toArray

  --if ps.size == 0 then default else default
  --minimizeOne "029A"



def performPushes (s : String) (type : keyboard) (c : Char := 'A') : String :=
  --performPushesW {seed := c, mv := s} type |>.mv
  let keys := type.keys
  let dirs := s.toList.foldl (init := #[]) (·.push <| charToMove ·)
  let (_, buttons) : pos × String := dirs.foldl (init := (keys[c]!, ""))
    fun (currPos, buttons) mv =>
      if mv == ((0, 0) : pos) then (currPos, buttons.push (posToChar type currPos))
      else
        (currPos + mv, buttons)
  buttons

#assert "<A^A>^^AvvvA" == performPushes "v<<A>>^A<A>AvA<^AA>A<vAAA>^A" .dir
#assert "v<<A>>^A<A>AvA<^AA>A<vAAA>^A" ==
  performPushes "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" .dir

def decodeNdir (s : String) : Nat → String
  | 0 => s
  | n + 1 => performPushes (decodeNdir s n) .dir

def decodeN (s : String) (n : Nat) : String :=
  performPushes (decodeNdir s n) .num

def decodePart1 (s : String) : String := decodeN s 2

#eval "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A".splitOn "A" |>.length
#eval "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A".splitOn "A" |>.length

#eval (performPushesW {mv := "<vA<AA>>^AvAA<^A>A"} .dir).seed
#eval decodeNdir "<vA<AA>>^AvAA<^A>A" 2
#eval (performPushesW {mv := "<v<A>>^AvA^A<vA"} .dir).seed
#eval decodeNdir                   "<v<A>>^AvA^A<vA" 2
#eval (performPushesW {seed := 'v', mv := ">^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"} .dir).seed
#eval decodeNdir ">^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" 2
#eval decodeNdir "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" 2

#assert "029A" == decodePart1 "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"
#assert "980A" == decodePart1 "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A"
-- This may be an error in the input: the sequence of steps corresponds to `379A` below.
#assert "179A" == decodePart1 "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
#assert "456A" == decodePart1 "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A"
-- This may be an error in the input: the sequence of steps corresponds to `179A` above.
#assert "379A" == decodePart1 "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
#eval 68 * 29 + 60 * 980 + 68 * 179 + 64 * 456 + 64 * 379

#eval "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A".length
/--
info:
122667368278
124667559426
127753762758
119474332746
136317388492
349351119674262

---
warning: unused variable `dat`
note: this linter can be disabled with `set_option linter.unusedVariables false`
-/
#guard_msgs in
#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut tally := 0
  for st in dat do
  --let st := "029A"
  --let st := "980A"

    let mut step := wholeRun {mv := st} .num
    --let mut w : window := {mv := step}
    let mut hw := splitAndStraightenWindow {mv := step}
    for _ in [0:25] do
      hw := tallyMoveWindow hw
    let tot := hw.fold (fun tot (s : window) m => tot + m * String.length s.mv) 0
    tally := tally + tot * st.getNats[0]!
    IO.println <| tot
  IO.println <| tally

#eval show Elab.Term.TermElabM _ from do
  let dat := atest
  let mut tally := 0
  let mut totals := #[]
  for st in dat do
    let step := wholeRun {mv := st} .num
    let mut hw := splitAndStraightenWindow {mv := step}
    for _ in [0:2] do
      hw := tallyMoveWindow hw
    let tot := hw.fold (fun tot (s : window) m => tot + m * String.length s.mv) 0
    tally := tally + tot * st.getNats[0]!
    totals := totals.push tot
  totals := totals.push tally
  guard <| totals == #[68, 60, 68, 64, 64, 126384]

/--
Returns the answer to the puzzle, where `reps = 2` for part 1 and `reps = 25` for part 2.
The first output values are the lengths of the final strings of each input, in order;
the last output value is the answer.

**Warning.** Yields the *wrong* answer for part 2, certainly for the actual puzzle,
likely also for the test.
-/
def combined (dat : Array String) (reps : Nat) :
    Array Nat := Id.run do
  let mut tally := 0
  let mut totals := #[]
  for st in dat do
    let step := wholeRun {mv := st} .num
    let mut hw := splitAndStraightenWindow {mv := step}
    for _ in [0:reps] do
      hw := tallyMoveWindow hw
    let tot := hw.fold (fun tot (s : window) m => tot + m * String.length s.mv) 0
    tally := tally + tot * st.getNats[0]!
    totals := totals.push tot
  totals.push tally

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  IO.println <| combined dat 25

/-- info:
`<A^A^>^AvvvA`
`<A^A>^^AvvvA`
-/
#guard_msgs in
#eval do
  let str := "029A"
  let steps := strToPaths mkNum str
  for s in steps do
    IO.println s!"`{s}`"

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

def getMults (mults : Std.HashMap String Nat) : Nat := Id.run do
  let mut tots := 0
  for (_s, val) in mults do
    tots := tots + val -- * s.length
  tots

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

def findFirst (str : String) : String := Id.run do
  let firstLayer := strToPaths mkNum str --(mkDir <| grid[str.get ⟨0⟩]!) str
  let later := repeatDirLayers firstLayer 2
  for f in firstLayer do
    for cand in strToPaths mkDir f do -- (mkDir <| grid[str.get ⟨1⟩]!) f do
      for cand1 in strToPaths mkDir cand do
        if later.contains cand1 then return f
  panic "oh no!"
  return ""

def findPair (str : String) : String := Id.run do
  --let grid := mkDir.keys
  let firstLayer := strToPaths mkDir str --(mkDir <| grid[str.get ⟨0⟩]!) str
  let later := repeatDirLayers firstLayer 1
  for f in firstLayer do
    for cand in strToPaths mkDir f do -- (mkDir <| grid[str.get ⟨1⟩]!) f do
      --for cand1 in strToPaths mkDir cand do
        if later.contains cand then return f
  panic "oh no!"
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
        let newVal := (findPair s).dropWhile (· != 'A')
        --dbg_trace "{s}, newVal: {newVal}"
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
-- <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
-- v<<A>>^A<A>AvA<^AA>A<vAAA>^A
-/
#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut (mults, memos) := (insertString ∅ ("A" ++ ""), ∅)
  let mut res := 0
  let mut msg := #[]
  IO.println "#eval"
  for str in dat do
    --let inDir := findFirst str --(strToPaths mkNum str).toArray[0]!
    IO.print s!"{str.dropRightWhile (· == 'A')} * "
    let inDirs := strToPaths mkNum str --(strToPaths mkNum str).toArray[0]!
    --for i in inDirs do IO.println <| findPair i
    let mut mint := 100
    let mut correct := ""
    for inDir in inDirs do
      (mults, memos) := (insertString ∅ ("A" ++ inDir), memos)
      for i in [0:2] do
        (mults, memos) := increaseOne mults memos
      --showMults mults
      --IO.println str
      if getMults mults ≤ mint then
        IO.println s!"                                     -- {mint}: {inDir}"
        mint := getMults mults
        correct := inDir
    (mults, memos) := (insertString ∅ ("A" ++ correct), memos)
    msg := msg.push s!"{correct}: {mint}"
    memos := ∅
    for i in [0:25] do
      (mults, memos) := increaseOne mults memos
    IO.println s!"{getMults mults} + "
    res := res + (str.dropRight 1).toNat! + getMults mults
  IO.println <| "\n".intercalate <| "" :: "Used:" :: msg.toList
  IO.println res
/-!
-/

#assert 142074832574328 == -- too low
964 * 49767208068 +
140 * 50575382072 +
413 * 51834670766 +
670 * 48735351682 +
593 * 55578896886

#assert 246822631766548 == -- too high -- misunderstanding: used test file
 29 * 133093870844 +
980 * 115740358202 +
179 * 129556635932 +
456 * 129556635930 +
379 * 124720038676
--

#assert 359440929327624 == -- too high
964 * 125908094772 +
140 * 127952728204 +
413 * 131138653760 +
670 * 123297559500 +
593 * 140611324032

#assert 363423815590716 == -- beyond too high
964 * 128760902214 +
140 * 127952728204 +
413 * 131138653760 +
670 * 123297559500 +
593 * 142690210860

#assert 390073922586586 == -- beyond too high
964 * 136392453062 +
140 * 137930468098 +
413 * 141834898620 +
670 * 133093870844 +
593 * 154351814006

#exit


  let second := (strToPaths mkNum "980A").toArray[0]!
  IO.println <| (strToPaths mkNum "029A").toArray
  let str := "<A^A>^^AvvvA"
  let str := second
  let third := (strToPaths mkNum "179A").toArray[1]!
  let str := third
  let mut (mults, memos) := (insertString ∅ ("A" ++ str), {})
  showMults mults
  for i in [0:2] do
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
