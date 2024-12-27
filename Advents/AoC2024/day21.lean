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

/--
Converts a *direction* (an integer vector with distance 1 to `(0, 0)`) to the corresponding
move among `<`, `^`, `v`, `>`.
The vector `(0, 0)` is mapped to `·` and everything else panics, returning `(0, 0)`.

The inverse is `charToDir`.
-/
def dirToChar : pos → Char
  | (  0,   1) => '>'
  | (  0, - 1) => '<'
  | (  1,   0) => 'v'
  | (- 1,   0) => '^'
  | (  0,   0) => '·'
  | p => panic s!"`dirToChar`: Not expecting '{p}' as a position!"

/--
Converts a character to a *direction* (an integer vector with distance 1 to `(0, 0)`).
The character is one among `<`, `^`, `v`, `>`.
The character `·` is mapped to the vector `(0, 0)` and everything else panics, returning `A`.

The inverse is `dirToChar`.
-/
def charToDir : Char → pos
  | '^' => (- 1,   0)
  | 'A' => (  0,   0)
  | '<' => (  0, - 1)
  | 'v' => (  1,   0)
  | '>' => (  0,   1)
  | '·' => (0, 0)
  | p => panic s!"`charToDir`: Not expecting '{p}' as a character!"

def charToPos : Char → pos
  | '^' => (0, 1)
  | 'A' => (0, 2)
  | '<' => (1, 0)
  | 'v' => (1, 1)
  | '>' => (1, 2)
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

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
Applies to the numeric keyboard. Use `generatePathFromPos` for a directional keyboard.

*Note.* Probably these two functions should be merged, once I get part 2 to work.
-/
def generatePathFromPos1 (p : pos) : Array pos :=
  let horMove := List.replicate p.2.natAbs (0, p.2.sign) |>.toArray
  let verMove := List.replicate p.1.natAbs (p.1.sign, 0) |>.toArray
  -- if I need to move left, then I move vertically first
  if p.2 < 0 then verMove ++ horMove else
  horMove ++ verMove

def generatePathFromPos (k : keyboard) (p : pos) : Array pos :=
  let horMove := List.replicate p.2.natAbs (0, p.2.sign) |>.toArray
  let verMove := List.replicate p.1.natAbs (p.1.sign, 0) |>.toArray
  match k with
    | .num =>
      ---- if I know that I am going left and up, then I can move horizontally first
      --if p.1 < 0 && p.2 < 0 then horMove ++ verMove else
      -- if I know that I am going down, then I can move horizontally first
      if 0 < p.1 then horMove ++ verMove else
      -- if I know that I am going up and right, then I can move horizontally first
      if p.1 < 0 && 0 < p.2 then horMove ++ verMove else
      -- if I know that I am going left and down, then I can move horizontally first
      if 0 < p.1 && p.2 < 0 then horMove ++ verMove else
      verMove ++ horMove
    | .dir =>
      -- if I know that I am going up, then I can move horizontally first
      if p.1 < 0 then horMove ++ verMove else
      -- if I know that I am going down and right, then I can move horizontally first
      if 0 < p.1 && 0 < p.2 then horMove ++ verMove else
      -- if I know that I am going left and up, then I can move horizontally first
      if p.1 < 0 && p.2 < 0 then horMove ++ verMove else
      verMove ++ horMove

-- In the numeric keyboard, avoid going through the bottom-left entry.
/-- info: #[>, v] -/
#guard_msgs in
#eval do
  let x := generatePathFromPos .num (1, 1)
  IO.println <| x.map dirToChar

-- In the numeric keyboard, avoid going through the bottom-left entry.
/-- info: #[^, <] -/
#guard_msgs in
#eval do
  let x := generatePathFromPos .num (- 1, - 1)
  IO.println <| x.map dirToChar

-- In the directional keyboard, avoid going through the top-left entry.
/-- info: #[>, ^] -/
#guard_msgs in
#eval do
  let x := generatePathFromPos .dir (-1, 1)
  IO.println <| x.map dirToChar

-- In the directional keyboard, avoid going through the top-left entry.
/-- info: #[v, <] -/
#guard_msgs in
#eval do
  let x := generatePathFromPos .dir (1, -1)
  IO.println <| x.map dirToChar

def charToPresses (k : keyboard) (c d : Char) : Array Char :=
  let keys := k.keys
  let diff := keys.getD d default - keys.getD c default
  generatePathFromPos k diff |>.map dirToChar

def numToDir (str : String) : String :=
  let (tot, _) := str.toList.foldl (init := (#[], 'A')) fun (tot, prev) ci =>
    (tot ++ charToPresses .num prev ci |>.push 'A', ci)
  ⟨tot.toList⟩

----------`<A^A>^^AvvvA`
/-- info: `<A^A>^^AvvvA` -/
#guard_msgs in
#eval do
  let str := "029A"
  IO.println s!"`{numToDir str}`"
  if numToDir str != "<A^A>^^AvvvA" then
    IO.println "Difference with example"

/--
*Adds* and extra first character `A` and, starting from there, produces the movements that continue
on to the input characters.

The difference with `stringToPathString'` is that `stringToPathString'` does not start at `A`,
but from the first input character.
-/
def stringToPathString (k : keyboard) (str : String) : String :=
  let (tot, _) := str.toList.foldl (init := (#[], 'A')) fun (tot, prev) ci =>
    (tot ++ charToPresses k prev ci |>.push 'A', ci)
  ⟨tot.toList⟩

/--
Assumes that the first character is the starting point of the movement and,
starting from there, produces the movements that continue on to the following characters.

The difference with `stringToPathString` is that `stringToPathString` starts at `A` and,
from `A`, continues on to the characters in the string.
-/
def stringToPathString' (k : keyboard) (str : String) : String :=
  if str.isEmpty then "" else
  let (tot, _) := (str.drop 1).toList.foldl (init := (#[], str.get 0)) fun (tot, prev) ci =>
    (tot ++ charToPresses k prev ci |>.push 'A', ci)
  ⟨tot.toList⟩

#eval stringToPathString' .num "A0"
#eval stringToPathString' .num "02"
#eval stringToPathString' .num "29"
#eval stringToPathString' .num "9A"
#eval stringToPathString .num "029A"

def stringToTally (str : String) : Std.HashMap (Char × Char) Nat :=
  --if str.length ≤ 1 then ∅ else
  let (tot, _) := (str).toList.foldl (init := (#[], 'A')) fun (tot, prev) ci =>
    (tot.push (prev, ci), ci)
  tot.foldl (init := ∅) fun h cs => h.alter cs (some <| ·.getD 0 + 1)

def ltTally [LT α] [DecidableRel (LT.lt (α := α))] (x y : α × Nat) : Bool :=
  y.2 < x.2 || (x.2 == y.2 && x.1 < y.1)

def showTally [ToString α] [BEq α] [Hashable α] [LT α] [DecidableRel (LT.lt (α := α))]
    (h : Std.HashMap α Nat) (count : α → Nat := fun _ => 1) : IO Unit := do
  let mut tot := 0
  for (x, m) in h.toArray.qsort ltTally do
    tot := tot + m * count x
    IO.println s!"{m} time{if m == 1 then " " else "s"} {x}"
  IO.println s!"\n{tot} total multiplicities"

def showTally! [ToString α] [BEq α] [Hashable α] [LT α] [DecidableRel (LT.lt (α := α))]
    (h : Std.HashMap α Nat) (count : α → Nat := fun _ => 1) : IO Unit := do
  let h := h.filter fun _ => (· != 0)
  showTally h count

def showMults [BEq α] [Hashable α] [LT α] [DecidableRel (LT.lt (α := α))]
    (h : Std.HashMap α Nat) (count : α → Nat := fun _ => 1) : IO Unit := do
  IO.println s!"{h.fold (init := 0) fun tot x m => tot + m * count x} with multiplicities."

#eval do
  let s := "02922929A"
  showTally (stringToTally s)

#eval do
  showTally (stringToTally "029A")

/-- info: `<A^A>^^AvvvA` -/
#guard_msgs in
#eval do
  let str := "029A"
  IO.println s!"`{stringToPathString .num str}`"

#eval do
  let str := "<A^A>^^AvvvA"
  IO.println s!"`{stringToPathString .dir str}`"

--        `v<<A>>^A<A>AvA<^AA>A<vAAA>^A`
/-- info: `v<<A>>^A<A>AvA<^AA>Av<AAA>^A`
-- `v<A<AA>>^AvAA<^A>Av<<A>>^AvA^Av<A>^Av<<A>^A>AAvA^Av<A<A>>^AAAvA<^A>A`
-/
-- `<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A`
#guard_msgs in
#eval do
  let str := "029A"
  let first := stringToPathString .num str
  let second := stringToPathString .dir first
  IO.println s!"`{second}`"
  IO.println s!"-- `{stringToPathString .dir second}`"

def genericTally [BEq α] [Hashable α] (h : Std.HashMap α Nat) (f : α → Std.HashMap α Nat) :
    Std.HashMap α Nat :=
  h.fold (init := ∅) fun h a m =>
    (f a).fold (init := h) fun h b n => h.alter b (some <| ·.getD 0 + m * n)

def genericTallyFor [ToString α] [BEq α] [Hashable α] (h : Std.HashMap α Nat) (f : α → Std.HashMap α Nat) :
    Std.HashMap α Nat := Id.run do
  let mut fin := ∅
  for (a, m) in h do
    for (b, n) in f a do
      --dbg_trace "inserting {b} with mult {n}"
      fin := fin.alter b (some <| ·.getD 0 + m * n)
  return fin

#eval do
  let f : Nat → Std.HashMap Nat Nat :=
    fun t => (Std.HashMap.empty.insert (t - 1) t).insert (t - 2) (t - 1)
  let mut g : Std.HashMap Nat Nat := {(15, 1)}
  for _ in [0:14] do
    g := genericTallyFor g f
  showTally! <| g
  --showTally <| genericTallyFor {(5, 1)} fun t => {(t - 1, t), (t + 1, t - 1)}

/-- info:
3 times abbac
3 times abbad
2 times abbba
1 time  abbbc
1 time  abbbd
1 time  bbbac
1 time  bbbad

12 total multiplicities
-/
#guard_msgs in
#eval do
  let s : Std.HashMap String Nat := {("abbbac", 1), ("abbbad", 1)}
  let f (st : String) : Std.HashMap String Nat :=
    (Array.range st.length).foldl (init := ∅) fun h i => h.alter (⟨st.toList.eraseIdx i⟩) (some <| ·.getD 0 + 1)
  showTally <| genericTally s f

#eval do
  let s : Std.HashMap String Nat := {("abbbac", 1), ("abbbad", 4)}
  let f (st : String) : Std.HashMap String Nat :=
    (Array.range st.length).foldl (init := ∅) fun h i => h.alter (⟨st.toList.eraseIdx i⟩) (some <| ·.getD 0 + 1)
  showTally <| genericTally s f

#eval do
  let s : Std.HashMap String Nat := {("abbbac", 1)}
  let f (st : String) : Std.HashMap String Nat :=
    (Array.range st.length).foldl (init := ∅) fun h i => h.alter (⟨st.toList.eraseIdx i⟩) (some <| ·.getD 0 + 1)
  showTally (genericTally s f)

def pairToTally (c : Char × Char) : Std.HashMap (Char × Char) Nat :=
  stringToTally (stringToPathString' .dir (("".push c.1).push c.2))

#eval
  let s := "v<<A>>^A<A>AvA<^AA>Av<AAA>^A"
  (s.length, s.splitOn "A" |>.length)

#eval do
  let s := "029A"
  let strTo := stringToPathString .num s
  let mut init := stringToTally strTo
  for _ in [0:2] do
    init := genericTallyFor init pairToTally
  showTally init


#eval do
  let dat := atest
  for s in dat do
    let strTo := stringToPathString .num s
    let mut init := stringToTally strTo
    for _ in [0:2] do
      init := genericTallyFor init pairToTally
    showMults init
  IO.println ""



partial
def seqs {α} [BEq α] [Hashable α] : List α → List α → Std.HashSet (List α)
  | [], l => {l}
  | l, [] => {l}
  | L@(l::ls), M@(m::ms) =>
    ((seqs ls M).fold (init := (∅ : Std.HashSet (List α))) (·.insert <| l::·)).union <|
      (seqs L ms).fold (init := (∅ : Std.HashSet (List α))) (·.insert <| m::·)

/--
Computes all paths from `q` to `p` that are minimal with respect to the L¹-distance,
written as strings of instructions `<`, `^`, `>`, `v`.
`findPaths` then returns each such path, with `A` appended at the end.
-/
def findPaths (p q : pos) : Std.HashSet String :=
  let mv := p - q
  let right := List.replicate mv.2.natAbs (dirToChar (0, mv.2.sign))
  let left :=  List.replicate mv.1.natAbs (dirToChar (mv.1.sign, 0))
  seqs left right |>.fold (init := ∅) (·.insert <| (⟨·⟩ : String).push 'A')

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
    curr := curr + charToDir si
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

/-!
-/
#eval do
  IO.println <| strToPaths mkDir "<A" |>.toArray
  IO.println <| strToPaths mkNum "029A" |>.toArray
/-!
-/

def extendPathOne (s : Array (String × Nat)) (next : String) : Array (String × Nat) :=
  match s.findIdx? (·.1 == next) with
    | none => s.push (next, 1)
    | some idx => s.modify idx fun (_, oldMult) => (next, oldMult + 1)

def splitAtAs (s : String) : Array String :=
  (s.dropRight 1).splitOn "A" |>.foldl (init := #[]) (·.push <| ·.push 'A')

/--
Adds the entries of `ns` to each entry of `s`, while memoizing over "splits at `A`".

In particular, all the entries of `s` are expected to end with `A` and contain no further `A`.
The entries of `ns` could have non-trailing `A`s, as they get split before extending the tally.
-/
def extendPath (s : Std.HashSet (Array (String × Nat))) (ns : Std.HashSet String) :
    Std.HashSet (Array (String × Nat)) := Id.run do
  let mut fin := ∅
  for os in s do
    for nextLong in ns do
      let newOs := (splitAtAs nextLong).foldl (init := os) extendPathOne
      fin := (fin.erase os).insert newOs
  return fin

/-!
-/
#eval strToPaths mkNum "0"

#eval do
  let first := extendPath {#[]} (strToPaths mkDir "<A")
  let second := extendPath first (strToPaths mkDir "<A")
  for f in first do IO.println f
  IO.println ""
  for f in second do IO.println f
  --IO.println <| extendPath mkNum #[("029A", 1)]

#exit

def shortestSeq (keys : String) (d : Nat) (cache : Std.HashMap (String × Nat) Nat) (con : Nat) :
    Nat × Std.HashMap (String × Nat) Nat := Id.run do
  if con == 0 then dbg_trace "keys: {keys}"; return default
  let mut cache := cache
  let mut tot := 0
  match d with
  | 0 => return (keys.length, cache.insert (keys, 0) keys.length)
  | d + 1 => if cache.contains (keys, d) then return (cache[(keys, d)]!, cache) else
    let subkeys := (keys.dropRight 1).splitOn "A" |>.foldl (init := #[]) (·.push <| ·.push 'A')
    for sk in subkeys do
      let currSeq := strToPaths mkDir sk
      let (recMin, cache') := currSeq.fold (init := (10^100, cache)) fun (m, cc) pth =>
        let (newMin, newCache) := shortestSeq pth d cc (con - 1)
        (min m newMin, cc) --.union newCache)
      tot := tot + recMin
      cache := cache'.insert (keys, d) tot
    --dbg_trace cache.size
    return (tot, cache)

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut tot := 0
  let mut cache := ∅
  for str in dat do
    let parts := strToPaths mkNum str
    let mut strMin := 10^100
    for p in parts do
      let (newMin, newCache) := shortestSeq p 5 cache 3
      strMin := min strMin newMin
      cache := cache.union newCache
    IO.println <| s!"{str} * {strMin}"
    tot := tot + strMin * str.getNats[0]!
  IO.println tot



/-- info: 126384 = 68 * 29 + 60 * 980 + 68 * 179 + 64 * 456 + 64 * 379 -/
#guard_msgs in
#eval do
  let dat := atest
  let mut tot := 0
  let mut msg := #[]
  for d in dat do
    let first := stringToPathString .num d
    let second := stringToPathString .dir first
    let third := stringToPathString .dir second
    msg := msg.push s!"{third.length} * {d.getNats[0]!}"
    tot := tot + d.getNats[0]! * third.length
  IO.println <| s!"{tot} = " ++ " + ".intercalate msg.toList




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
