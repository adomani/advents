import Advents.Utils
open Lean

namespace Day10

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day10.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- If the input character is a closed parenthesis, return the corresponding open parenthesis, otherwise return `A`. -/
def close : Char → Char
  | ')' => '('
  | ']' => '['
  | '}' => '{'
  | '>' => '<'
  | _ => 'A'

/-- If the input character is an open parenthesis, return the corresponding closed parenthesis, otherwise return `A`. -/
def openC : Char → Char
  | '(' => ')'
  | '[' => ']'
  | '{' => '}'
  | '<' => '>'
  | _ => 'A'

/-- The illegal character penalty for each character. -/
def points : Char → Nat
  | ')' => 3
  | ']' => 57
  | '}' => 1197
  | '>' => 25137
  | _ => 0

/--
For each input string, compute whether the string has malformed parentheses or is well-formed, but missing some
trailing closed parentheses.
In the former case, append the total penalty to the first output array.
In the latter case, append the list of missing closed parentheses to complete a correct syntax.
-/
def errorsOrClosing (dat : Array String) : Array Nat × Array (List Char) :=
 Id.run do
  let mut pts := #[]
  let mut closeParens := #[]
  for lin in dat do
    let mut queue := #[]
    let mut err := #[]
    for c in lin.toList do
      if openC c != 'A' then
        queue := queue.push c
      else if queue.back? == close c
      then
        queue := queue.pop
      else
        err := err.push [queue.back?.map openC, some c].reduceOption
    if let some e := err[0]? then pts := pts.push <| points e[1]!
    else
      let closes := queue.reverse.map openC
      closeParens := closeParens.push closes.toList
  return (pts, closeParens)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := (errorsOrClosing dat).1.sum

#assert part1 atest == 26397

solve 1 167379

/-!
#  Question 2
-/

/-- The closing character value of each character. -/
def pointsC : Char → Nat
  | ')' => 1
  | ']' => 2
  | '}' => 3
  | '>' => 4
  | _ => 0

/-- The total score of a list of closing parentheses. -/
def evalCompletion (l : List Char) : Nat := Id.run do
  let mut score := 0
  for c in l do
    score := 5 * score + pointsC c
  return score

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let (_, npts) := errorsOrClosing dat
  let scores := npts.map evalCompletion
  let sorted := scores.qsort (· < ·)
  sorted[sorted.size / 2]!

#assert part2 atest == 288957

solve 2 2776842859

end Day10
