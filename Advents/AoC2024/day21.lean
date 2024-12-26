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
def generatePathFromPos (p : pos) : Array pos :=
  let horMove := List.replicate p.2.natAbs (0, p.2.sign) |>.toArray
  let verMove := List.replicate p.1.natAbs (p.1.sign, 0) |>.toArray
  -- if I need to move left, then I move vertically first
  if p.2 < 0 then verMove ++ horMove else
  horMove ++ verMove

-- In the numeric keyboard, avoid going through the bottom-left entry.
/-- info: #[>, v] -/
#guard_msgs in
#eval do
  let x := generatePathFromPos (1, 1)
  IO.println <| x.map dirToChar

-- In the numeric keyboard, avoid going through the bottom-left entry.
/-- info: #[^, <] -/
#guard_msgs in
#eval do
  let x := generatePathFromPos (- 1, - 1)
  IO.println <| x.map dirToChar

-- In the directional keyboard, avoid going through the top-left entry.
/-- info: #[>, ^] -/
#guard_msgs in
#eval do
  let x := generatePathFromPos (-1, 1)
  IO.println <| x.map dirToChar

-- In the directional keyboard, avoid going through the top-left entry.
/-- info: #[v, <] -/
#guard_msgs in
#eval do
  let x := generatePathFromPos (1, -1)
  IO.println <| x.map dirToChar

def charToPresses (k : keyboard) (c d : Char) : Array Char :=
  let keys := k.keys
  let diff := keys[d]! - keys[c]!
  generatePathFromPos diff |>.map dirToChar

def numToDir (str : String) : String :=
  let (tot, _) := str.toList.foldl (init := (#[], 'A')) fun (tot, prev) ci =>
    (tot ++ charToPresses .num prev ci |>.push 'A', ci)
  tot.toList.toString

/-- info: [<, A, ^, A, >, ^, ^, A, v, v, v, A] -/
#guard_msgs in
#eval do
  let str := "029A"
  IO.println <| numToDir str
--def generatePath (s t : Char) : String :=
--  let

def stringToDir (k : keyboard) (str : String) : String :=
  let (tot, _) := str.toList.foldl (init := (#[], 'A')) fun (tot, prev) ci =>
    (tot ++ charToPresses k prev ci |>.push 'A', ci)
  ⟨tot.toList⟩

/-- info: `<A^A>^^AvvvA` -/
#guard_msgs in
#eval do
  let str := "029A"
  IO.println s!"`{stringToDir .num str}`"

#eval do
  let str := "<A^A>^^AvvvA"
  IO.println s!"`{stringToDir .dir str}`"

--        `v<<A>>^A<A>AvA<^AA>A<vAAA>^A`
/-- info: `v<<A>>^A<A>AvA^<AA>Av<AAA>^A` -/
#guard_msgs in
#eval do
  let str := "029A"
  let first := stringToDir .num str
  let second := stringToDir .dir first
  IO.println s!"`{second}`"
  IO.println s!"`{stringToDir .dir second}`"

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
