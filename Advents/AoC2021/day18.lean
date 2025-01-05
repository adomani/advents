import Advents.Utils

namespace Day18

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day18.input"

/-!
#  Question 1
-/

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

/-- A `Snail` -/
inductive Snail where
  | i   : Nat → Snail
  | cat : Snail → Snail → Snail
  deriving Inhabited, BEq

def Snail.toString : Snail → String
    | .i n => s!"{n}"
    | .cat s t => s!"[{s.toString}, {t.toString}]"

instance : ToString Snail where toString := Snail.toString

variable (n : Nat) in
instance : OfNat Snail n where
  ofNat := .i n

@[match_pattern]
notation "[" s ", " t "]" => Snail.cat s t

def magnitude : Snail → Nat
  | .i d => d
  | [a, b] => 3 * magnitude a + 2 * magnitude b

#assert magnitude [[1,2],[[3,4],5]] == 143
#assert magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] == 3488

instance : Add Snail where
  add := .cat

#assert ([1, 2] : Snail) + [2, [3, 4]] == [[1, 2], [2, [3, 4]]]

def split : Snail → Snail
  | .i n@(_ + 10) => [.i (n / 2), .i ((n + 1) / 2)]
  | .i n => .i n
  | [a, b] =>
    let sa := split a
    if sa != a then [sa, b] else [sa, (split b)]

#assert ([[[[4,3],4],4],[7,[[8,4],9]]] : Snail) + ([1,1] : Snail) == ([[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] : Snail)
#assert split [[[[0,7],4],[15,[0,13]]],[1,1]] == [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
#assert split [[[[0,7],4],[[7,8],[0,13]]],[1,1]] == [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]

def addLeftmost (n : Nat) : Snail → Snail
  | .i x => .i (x + n)
  | [a, b] => [(addLeftmost n a), b]

#assert addLeftmost 4 ([1, [2, [3, 4]]] : Snail) == ([5, [2, [3, 4]]] : Snail)
#assert addLeftmost 4 ([[1, 5], [2, [3, 4]]] : Snail) == ([[5, 5], [2, [3, 4]]] : Snail)

def addRightmost (n : Nat) : Snail → Snail
  | .i x =>
    --dbg_trace "Radding {n} to {x} = {n + x}"
    .i (x + n)
--  | .cat a (.i b) =>
--    .cat a (.i (b + n))
  | .cat a b => .cat a (addRightmost n b)

inductive loc where | l | r
  deriving Inhabited, DecidableEq

instance : ToString loc where toString := (match · with | .l => "l"| .r => "r")

variable (cond : Array loc → Bool) in
-- consider making `leftMostNestedPair` `Option (Array loc)`-valued to distinguish between the
-- "head" `snail` and a never-satisfied condition.
def leftMostNestedPair : Snail → (locs : Array loc := ∅) → Option (Array loc × Snail × Nat × Nat)
  | [(.i l), (.i r)], locs => if cond locs then (some (locs, 0, l, r)) else none
  | [a, b], locs =>
    match leftMostNestedPair a (locs.push .l) with
      | none =>
        match leftMostNestedPair b (locs.push .r) with
          | none => none
          | some (lb, s, l, r) =>
            if cond lb then (lb, ([a, s] : Snail), l, r)
            else none
      | some (la, s, l, r) =>
        if cond la then (la, ([s, b] : Snail), l, r)
        else
        match leftMostNestedPair b (locs.push .r) with
          | none => none
          | some (lb, s, l, r) =>
            if cond lb then (lb, ([a, s] : Snail), l, r)
            else none
  | .i _, _ => none

def addRightmostBefore (s : Snail) (locs : Array loc) (n : Nat) : Snail :=
  match locs[0]?, s with
    | none, s => addRightmost n s
    | some loc.r, [a, b] => [a, addRightmostBefore b (locs.erase .r) n]
    | some loc.l, [a, b] => [addRightmostBefore a (locs.erase .l) n, b]
    | some _, .i a => .i (a + n)

def addLeftmostAfter (s : Snail) (locs : Array loc) (n : Nat) : Snail :=
  match locs[0]?, s with
    | none, s => addLeftmost n s
    | some loc.l, [a, b] => [addLeftmostAfter a (locs.erase .l) n, b]
    | some loc.r, [a, b] => [a, addLeftmostAfter b (locs.erase .r) n]
    | some _, .i a => .i (a + n)

#assert leftMostNestedPair (5 ≤ ·.size) [[[[[9,8],1],2],3],4] == none

def explode (s : Snail) : Snail :=
  match leftMostNestedPair (4 ≤ ·.size) s with
    | none => s
    | some (locs, with0, l, r) =>
      let locsLeft := locs.popWhile (· == loc.l)
      let with0Left := if locsLeft.isEmpty then with0 else addRightmostBefore with0 (locsLeft.pop.push .l) l
      let locsRight := locs.popWhile (· == loc.r)
      if locsRight.isEmpty then with0Left else addLeftmostAfter with0Left (locsRight.pop.push .r) r

#assert explode [[[[[9,8],1],2],3],4] == [[[[0,9],2],3],4]
#assert explode [7,[6,[5,[4,[3,2]]]]] == [7,[6,[5,[7,0]]]]
#assert explode [[6,[5,[4,[3,2]]]],1] == [[6,[5,[7,0]]],3]
#assert explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] == [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
#assert explode [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] == [[3,[2,[8,0]]],[9,[5,[7,0]]]]
#assert explode [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] == [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
#assert explode [[[[0,7],4],[7,[[8,4],9]]],[1,1]] == [[[[0,7],4],[15,[0,13]]],[1,1]]
#assert explode [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]] == [[[[0,7],4],[[7,8],[6,0]]],[8,1]]

def reduce (s : Snail) : Snail := Id.run do
  let mut old : Snail := 0
  let mut s := s
  while s != old do
    old := s
    let ex := explode s
    if ex != s then
      s := ex
    else
      s := split s
  return s

#assert
  let dat : Snail := [[[[4,3],4],4],[7,[[8,4],9]]] + ([1,1] : Snail)
  reduce dat == [[[[0,7],4],[[7,8],[6,0]]],[8,1]]

#assert
  let dat : Array Snail := #[
    [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],
    [7,[[[3,7],[4,3]],[[6,3],[8,8]]]],
    [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]],
    [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]],
    [7,[5,[[3,8],[1,4]]]],
    [[2,[2,2]],[8,[8,1]]],
    [2,9],
    [1,[[[9,3],9],[[9,0],[0,7]]]],
    [[[5,[7,4]],7],1],
    [[[[4,2],2],6],[8,7]]
  ]
  let f := reduce dat[0]
  let tot := (dat.erase f).foldl (init := f) fun t n => reduce (t + n)
  tot == ([[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] : Snail)

def takeUntilClosedBrackets (s : String) : String × String := Id.run do
  let mut con := 0
  let mut first := ""
  for c in s.toList do
    first := first.push c
    if c == '[' then con := con + 1
    if c == ']' then con := con - 1
    if con == 0 then
      break
  return (first, s.drop first.length)

#assert takeUntilClosedBrackets "[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]" ==
  ("[[0,[5,8]],[[1,7],[9,6]]]", ",[[4,[1,2]],[[1,4],2]]")

partial
def parseSnail (s : String) : Snail :=
  let digs := s.takeWhile (·.isDigit)
  if !digs.isEmpty then .i digs.toNat! else
  let (l, r) := takeUntilClosedBrackets ((s.drop 1).dropRight 1)
  let r := r.drop 1
  [parseSnail l, parseSnail r]

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let dat := dat.foldl (·.push <| parseSnail ·) #[]
  let f := dat[0]!
  let tot := (dat.erase f).foldl (reduce <| · + ·) f
  magnitude tot

#assert part1 atest == 4140

solve 1 4057

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let dat := dat.foldl (·.push <| parseSnail ·) #[]
  let mut maxMag := 0
  for i in dat do
    for j in dat do
      maxMag := max maxMag (magnitude (reduce (i + j)))
  maxMag

#assert part2 atest == 3993

set_option trace.profiler true in solve 2 4683  -- takes approx 11s

end Day18
