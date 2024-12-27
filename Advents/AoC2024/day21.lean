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

def numKeys : Std.HashMap Char pos := .union {('A', (3, 2)), ('0', (3, 1))} <|
    (Array.range 9).foldl (init := (∅ : Std.HashMap Char pos)) fun h n =>
        h.insert (s!"{n + 1}".get 0) (2 - n.cast / 3, 2 - (8 - n.cast) % 3)

def dirKeys : Std.HashMap Char pos := {
                   ('^', (0, 1)), ('A', (0, 2)),
    ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2))
  }

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

def splitAtAs (s : String) : Array String :=
  (s.dropRight 1).splitOn "A" |>.foldl (init := #[]) (·.push <| ·.push 'A')

def shortestSeq (keys : String) (d : Nat) (cache : Std.HashMap (String × Nat) Nat) :
    Nat × Std.HashMap (String × Nat) Nat := Id.run do
  let mut cache := cache
  let mut tot := 0
  match d with
  | 0 => return (keys.length, cache.insert (keys, 0) keys.length)
  | j + 1 =>
    if cache.contains (keys, j + 1) then
      return (cache[(keys, j + 1)]!, cache)
    else
    let subkeys := splitAtAs keys
    for sk in subkeys do
      if cache.contains (sk, j + 1) then
        tot := tot + cache[(sk, j + 1)]!
      else
        let currSeq := strToPaths mkDir sk
        let (recMin, cache') := currSeq.fold (init := (10^100, cache)) fun (m, cc) pth =>
          let (newMin, newCache) := shortestSeq pth j cc
          (min m newMin, cc.union newCache)
        tot := tot + recMin
        cache := cache'.insert (keys, j + 1) tot
    return (tot, cache)

def minOne (str : String) (d : Nat) (cache : Std.HashMap (String × Nat) Nat) :
    Nat × Std.HashMap (String × Nat) Nat := Id.run do
  let parts := strToPaths mkNum str
  let mut cache := cache
  let mut strMin := 10^100
  for p in parts do
    let (newMin, newCache) := shortestSeq p d cache
    strMin := min strMin newMin
    cache := cache.union newCache
  (strMin, cache)

def parts (dat : Array String) (n : Nat) : Nat := Id.run do
  let mut tot := 0
  let mut cache := ∅
  for str in dat do
    let (strMin, cc) := minOne str n cache
    cache := cache.union cc
    tot := tot + strMin * str.getNats[0]!
  return tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := parts dat 2

#assert part1 atest == 126384

solve 1 197560

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := parts dat 25
--def part2 (dat : String) : Nat :=

-- set_option trace.profiler true in #assert part2 atest == 154115708116294

-- set_option trace.profiler true in solve 2 242337182910752  -- takes approx 34s

end Day21
