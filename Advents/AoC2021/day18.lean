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
  | .cat a b => max (level a) (level b) + 1

def split : snail → snail
  | .i n@(_ + 10) => .cat (.i (n / 2)) (.i ((n + 1) / 2))
  | .i n => .i n
  | .cat a b =>
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

def explode : snail → snail
  | n@(.i _) => n
  | .cat a b =>
    dbg_trace "left: {a}, right: {b}"
    if level a == 4 then
      dbg_trace "{a} has level 4"
      0 + b
    else if level b == 4
    then
      a + 0
    else
      let ca := explode a
      if ca != a then
        ca + b
      else
        let cb := explode b
        if cb != b then a + cb
      else
        a + b

#eval explode [[[[[9,8],1],2],3],4]
#reduce explode [[[[[9,8],1],2],3],4]

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
