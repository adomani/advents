import Advents.Utils
open Lean

namespace Day18

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day18.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

inductive snail where
  | i   : Nat → snail
  | cat : snail → snail → snail
  deriving BEq

def snail.toString : snail → String
    | .i n => s!"{n}"
    | .cat s t => s!"[{s.toString}, {t.toString}]"

instance : ToString snail where toString := snail.toString

variable (n : Nat) in
instance : OfNat snail n where
  ofNat := .i n

open snail

@[match_pattern]
notation "[" s ", " t "]" => snail.cat s t

#check ([1,2] : snail)
#check [[9, 3], [[9, 9], [6, [4, 9]]]]

def magnitude : snail → Nat
  | .i d => d
  | .cat a b => 3 * magnitude a + 2 * magnitude b

#eval magnitude [[1,2],[[3,4],5]]
#eval magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]

instance : Add snail where
  add := .cat

#eval ([1, 2] : snail) + [2, [3, 4]]

def level : snail → Nat
  | .i _ => 0
  | [a, b] => max (level a) (level b) + 1

def split : snail → snail
  | .i n@(_ + 10) => .cat (.i (n / 2)) (.i ((n + 1) / 2))
  | .i n => .i n
  | [a, b] =>
    let sa := split a
    if sa != a then .cat sa b
    else .cat sa (split b)


--def split : snail → snail
--  | .i n => .i n
--  | .cat (.i a) (.i b) => .cat (.i (a / 2)) (.i ((b + 1) / 2))
--  | .cat a b =>
--    let sa := split a
--    if sa != a then .cat sa b
--    else .cat sa (split b)
#assert ([[[[4,3],4],4],[7,[[8,4],9]]] : snail) + ([1,1] : snail) == ([[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] : snail)
#assert split [[[[0,7],4],[15,[0,13]]],[1,1]] == [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
#assert split [[[[0,7],4],[[7,8],[0,13]]],[1,1]] == [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]

#eval level [[[[[9,8],1],2],3],4]

variable (f : snail → snail) in
def modifyLeftmost : snail → snail
  | .cat a b =>
    let ea := f a
    if ea != a
    then
      [ea, b]
    else
      [a, f b]
  | d => f d

def addLeftmost (n : Nat) : snail → snail
  | .i x => dbg_trace "Ladding {n} to {x} = {n + x}"; .i (x + n)
  | .cat a b => .cat (addLeftmost n a) b

#assert addLeftmost 4 ([1, [2, [3, 4]]] : snail) == ([5, [2, [3, 4]]] : snail)
#assert addLeftmost 4 ([[1, 5], [2, [3, 4]]] : snail) == ([[5, 5], [2, [3, 4]]] : snail)

def addRightmost (n : Nat) : snail → snail
  | .i x => dbg_trace "Radding {n} to {x} = {n + x}"; .i (x + n)
--  | .cat a (.i b) =>
--    .cat a (.i (b + n))
  | .cat a b => .cat a (addRightmost n b)

inductive loc where | l | r
  deriving Inhabited, DecidableEq

instance : ToString loc where toString := (match · with | .l => "l"| .r => "r")

variable (cond : Array loc → Bool) in
-- consider making `leftMostNestedPair` `Option (Array loc)`-valued to distinguish between the
-- "head" `snail` and a never-satisfied condition.
def leftMostNestedPair : snail → (locs : Array loc := ∅) → Array loc
  | [(.i _), (.i _)], locs => if cond locs then locs else ∅
  | [a, b], locs =>
    let la := leftMostNestedPair a (locs.push .l)
    if cond la then la
    else
    let lb := leftMostNestedPair b (locs.push .r)
    if cond lb then lb
    else ∅
  | .i _, _ => ∅

#assert leftMostNestedPair (5 ≤ ·.size) [[[[[9,8],1],2],3],4] == #[]
#assert leftMostNestedPair (4 ≤ ·.size) [[[[[9,8],1],2],3],4] == #[.l, .l, .l, .l]
#assert leftMostNestedPair (4 ≤ ·.size) [7,[6,[5,[4,[3,2]]]]] == #[.r, .r, .r, .r]
#assert leftMostNestedPair (4 ≤ ·.size) [[6,[5,[4,[3,2]]]],1] == #[.l, .r, .r, .r]
#assert leftMostNestedPair (4 ≤ ·.size) [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] == #[.l, .r, .r, .r]
#assert leftMostNestedPair (4 ≤ ·.size) [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] == #[.r, .r, .r, .r]
#assert leftMostNestedPair (4 ≤ ·.size) [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] == #[.l, .l, .l, .l]
#assert leftMostNestedPair (4 ≤ ·.size) [[[[0,7],4],[7,[[8,4],9]]],[1,1]] == #[.l, .r, .r, .l]
#assert leftMostNestedPair (4 ≤ ·.size) [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]] == #[.l, .r, .r, .r]

def goTo : snail → Array loc → Option snail
  | s, #[] => s
  | [a, b], l => if l[0]! == .l then goTo a (l.erase .l) else goTo b (l.erase .r)
  | .i _, _ => none

#assert goTo [[[[[9,8],1],2],3],4] #[] == ([[[[[9,8],1],2],3],4] : snail)
#assert goTo [[[[[9,8],1],2],3],4] #[.l, .l, .l, .l] == ([9, 8] : snail)
#assert goTo [7,[6,[5,[4,[3,2]]]]] #[.r, .r, .r, .r] == ([3, 2] : snail)
#assert goTo [[6,[5,[4,[3,2]]]],1] #[.l, .r, .r, .r] == ([3, 2] : snail)
#assert goTo [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] #[.l, .r, .r, .r] == ([7, 3] : snail)
#assert goTo [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] #[.r, .r, .r, .r] == ([3, 2] : snail)
#assert goTo [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] #[.l, .l, .l, .l] == ([4, 3] : snail)
#assert goTo [[[[0,7],4],[7,[[8,4],9]]],[1,1]] #[.l, .r, .r, .l] == ([8, 4] : snail)
#assert goTo [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]] #[.l, .r, .r, .r] == ([6, 7] : snail)


def explodeCore : snail → Array loc → snail × Array loc
  | .cat (.cat goR (.cat (.i a) (.i b))) goL, locs => --| .cat goR (.cat (.cat (.i a) (.i b)) goL) =>
    dbg_trace "pattern [[goR, [{a}, {b}]], goL]: {(.cat (.i a) (.i b) : snail)}"
    (.cat (.cat (addRightmost a goR) 0) (addLeftmost b goL), locs.push .l |>.push .r)

  | .cat (.cat (.i a) (.i b)) goL, locs =>
    dbg_trace "pattern [[{a}, {b}], goL]: {(.cat (.i a) (.i b) : snail)}"
    (.cat 0 (addLeftmost b goL), locs.push .l |>.push .l)

  | .cat goR (.cat (.i a) (.i b)), locs =>
    dbg_trace "pattern [goR, [{a}, {b}]]: {(.cat (.i a) (.i b) : snail)}"
    (.cat (addRightmost a goR) 0, locs.push .r)

  | .cat a b, locs =>
    let (newA, locsA) := explodeCore a (locs.push .r)
    (.cat newA b, locsA)
  | d, locs => (d, locs)

--variable (patt : snail) in
--def snail.locateLIn : (s : snail) → Array loc
--  |

def explode : snail → Array loc → snail × Array loc
  | s@(.cat a b), locs =>
    let (ea, lea) := explodeCore a (locs.push .l)
    if ea != a then (.cat ea b, lea)
    else
      let (Ea, lE) := explode a (locs.push .l)
      if Ea != a then (.cat Ea b, lE)
      else
      let (es, les) := explodeCore s (locs.push .l)
      if es != s then (es, les)
      else
        dbg_trace "ignoring {a}-branch, entering {b}, hence computing {explodeCore b locs}"
        let (lb, locB) := explode b (locs.push .r)
        (.cat a lb, locB)
  | d, locs => (d, locs)

--#assert explode [[[[[9,8],1],2],3],4] == [[[[0,9],2],3],4]
#assert (explode [[[[[9,8],1],2],3],4] ∅).1 == [[[[0,9],2],3],4]
#eval (explode [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] ∅)
--== [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
#assert (explode [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] ∅).1 == [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
#eval explodeCore [4,[3,2]] ∅
#eval explode [4,[3,2]] ∅
#eval (explode [[6,[5,[4,[3,2]]]],1] ∅).1

/--
info: [[[1, [2, 3]], [4, 5]], 6]
focus on: [2, 3]
Radding 2 to 1 = 3
Ladding 3 to 4 = 7
[[3, [7, 5]], 6]
-/
#guard_msgs in
#eval do
  let s : snail := .cat (.cat (.cat 1 (.cat (.i 2) (.i 3))) (.cat 4 5)) 6 --.cat 6 (.cat ((.cat (.cat 5 62) (.cat 3 4))) <| .cat 7 5)
  IO.println s
  IO.println <| explode s

def explode' : snail → snail
  --| n@(.i _) => n
  | .cat (.cat goR (.cat (.i a) (.i b))) goL => --| .cat goR (.cat (.cat (.i a) (.i b)) goL) =>
    dbg_trace "focus on: {(.cat (.i a) (.i b) : snail)}"
    --dbg_trace "left: {a} {level a}, right: {b} {level b}"
    --if level a == 4 then
    --  dbg_trace "{a} has level 4"
    .cat ( (addRightmost 17 goR) /-(.cat a b)-/) (addLeftmost 179 goL)
      --0 + b
    --else if level b == 4
    --then
    --  a + 0
    --else
    --  let ca := explode a
    --  if ca != a then
    --    ca + b
    --  else
    --    let cb := explode b
    --    if cb != b then a + cb
    --  else
    --    a + b
  | d => d

#eval explode [[[[[9,8],1],2],3],4]
#reduce explode [[[[[9,8],1],2],3],4]

#eval explode [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
#eval explode [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day18
