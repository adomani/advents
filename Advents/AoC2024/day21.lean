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

/--
The numeric keyboard: a conversion between a character in the table
```
7 8 9
4 5 6
1 2 3
  0 A
```
and a position.  For instance, the character `A` corresponds to `(3, 2)`.
-/
def numKeys : Std.HashMap Char pos := .union {('A', (3, 2)), ('0', (3, 1))} <|
    (Array.range 9).foldl (init := (∅ : Std.HashMap Char pos)) fun h n =>
        h.insert (s!"{n + 1}".get 0) (2 - n.cast / 3, 2 - (8 - n.cast) % 3)

/--
The directional keyboard: a conversion between a character in the table
```
  ^ A
< v >
```
and a position.  For instance, the character `A` corresponds to `(0, 2)`.
-/
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

/--
`keys k` converts the `keyboard` `k` into its `HashMap` of keys, mapping a character to
the corresponding position.
-/
def keyboard.keys : keyboard → Std.HashMap Char pos
  | .dir => dirKeys | .num => numKeys

/--
`seqs l r` takes as input two lists `l` and `r` and returns the `HashSet` of lists
obtained by interleaving the entries of `l` and of `r` in all possible ways.
-/
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

/--
Makes sure that the given string is a sequence of instructions that make the robot move within
the input `keyboard`.
The initial robot's position is assumed to be `A`, since the robots always return to this key.
-/
def validate (k : keyboard) (s : String) : Bool := Id.run do
  let mut curr := k.keys['A']!
  for si in s.toList do
    curr := curr + charToDir si
    if (k.keys.filter fun _c (q : pos) => curr == q).isEmpty then return false
  return true

/--
Converts a string, such as "029A", into all the strings of instructions
It starts from the position recorded in `n` and then continues with the ones in `s`.
-/
def strToPaths (k : keyboard) (s : String) : Std.HashSet String :=
  let (_start, fin) : pos × Std.HashSet String :=
    s.toList.foldl (init := (k.keys['A']!, {""})) fun (start, fin) c =>
    let tgt := k.keys[c]!
    let next := findPaths tgt start
    (tgt, fin.fold (init := ∅) fun h st => h.union (next.fold (·.insert <| st ++ ·) ∅))
  fin.filter (validate k)

/--
Splits a string at the positions of the `A`s, thus isolating all paths starting and ending at `A`.
-/
def splitAtAs (s : String) : Array String :=
  (s.dropRight 1).splitOn "A" |>.foldl (init := #[]) (·.push <| ·.push 'A')

/--
A recursive function to compute the shortest sequence of key pushes that returns the input `keys`.
The input `d` is the number of intermediate robots in directional `keyboard`s.
The `cache` is, well, a cache of the values that get computed along the way.

This function assumes that all `keyboard`s are directional.
The function `shortestSeq` prepares the input, converting it
from a numeric `keyboard` to a directional one.

This solution is taken from
https://www.reddit.com/r/adventofcode/comments/1hjx0x4/2024_day_21_quick_tutorial_to_solve_part_2_in/
-/
def shortestSeqDir (keys : String) (d : Nat) (cache : Std.HashMap (String × Nat) Nat) :
    Nat × Std.HashMap (String × Nat) Nat :=
  match d with
  | 0 => (keys.length, cache.insert (keys, 0) keys.length)
  | j + 1 =>
    if cache.contains (keys, j + 1) then
      (cache[(keys, j + 1)]!, cache)
    else
    let subkeys := splitAtAs keys
    subkeys.foldl (init := (0, cache)) fun (tot, cache) sk =>
      if cache.contains (sk, j + 1) then
        (tot + cache[(sk, j + 1)]!, cache)
      else
        let currSeq := strToPaths .dir sk
        let (recMin, cache') := currSeq.fold (init := (10^100, cache)) fun (m, cc) pth =>
          let (newMin, newCache) := shortestSeqDir pth j cc
          (min m newMin, newCache)
        (tot + recMin, cache'.insert (keys, j + 1) (tot + recMin))

/--
A recursive function to compute the shortest sequence of key pushes that returns the input `keys`.
The input `d` is the number of intermediate robots in directional `keyboard`s.
The `cache` is, well, a cache of the values that get computed along the way.

The function converts the input string `str` to all sequences of pushes of keys in a directional
`keyboard` that return `str`.
Then, computes the value of `shortestSeq` on each such value, returning the smallest that it finds.
-/
def shortestSeq (str : String) (d : Nat) (cache : Std.HashMap (String × Nat) Nat) :
    Nat × Std.HashMap (String × Nat) Nat :=
  strToPaths .num str |>.fold (init := (10^100, cache)) fun (strMin, cache) p =>
    let (newMin, newCache) := shortestSeqDir p d cache
    (min strMin newMin, newCache)

/--
The common function for the two parts: the inputs are the puzzle input and the number of
robots aimed at directional `keyboard`s.
-/
def parts (dat : Array String) (n : Nat) : Nat :=
  let (tot, _cache) := dat.foldl (init := (0, ∅)) fun (tot, cache) str =>
    let (strMin, cc) := shortestSeq str n cache
    (tot + strMin * str.getNats[0]!, cc)
  tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := parts dat 2

#assert part1 atest == 126384

solve 1 197560

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := parts dat 25

set_option trace.profiler true in #assert part2 atest == 154115708116294

set_option trace.profiler true in solve 2 242337182910752  -- takes approx 4s

end Day21
