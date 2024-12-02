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


def isOpen : Char → Bool
  | '(' | '[' | '{' | '<' => true
  | _ => false

def close : Char → Option Char
  | ')' => some '('
  | ']' => some '['
  | '}' => some '{'
  | '>' => some '<'
  | _ => none

def openC : Char → Char
  | '(' => ')'
  | '[' => ']'
  | '{' => '}'
  | '<' => '>'
  | _ => 'A'

def points : Char → Nat
  | ')' => 3
  | ']' => 57
  | '}' => 1197
  | '>' => 25137
  | _ => 0

#eval do
  let mut pts := 0
  let dat := atest
  let dat ← IO.FS.lines input
  for lin in dat do
  --let lin := atest[2]!
    let mut queue := #[]
    let mut err := #[]
    for c in lin.toList do
      if isOpen c then
        queue := queue.push c
      else if some queue.back? == close c
      then
        queue := queue.pop
      else
        err := err.push [queue.back?.map openC, some c].reduceOption
    match err[0]? with
      | none => continue --IO.println "No error found."
      | some e => pts := pts + points e[1]!; --IO.println s!"Expected '{e[0]!}', found '{e[1]!}'"
  IO.println s!"Total points: {pts}"

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let mut pts := 0
  for lin in dat do
    let mut queue := #[]
    let mut err := #[]
    for c in lin.toList do
      if isOpen c then
        queue := queue.push c
      else if some queue.back? == close c
      then
        queue := queue.pop
      else
        err := err.push [queue.back?.map openC, some c].reduceOption
    if let some e := err[0]? then pts := pts + points e[1]!
  pts

#assert part1 atest == 26397

solve 1 167379

/-!
#  Question 2
-/


def pointsC : Char → Nat
  | ')' => 1
  | ']' => 2
  | '}' => 3
  | '>' => 4
  | _ => 0

def evalCompletion (l : List Char) : Nat := Id.run do
  let mut score := 0
  for c in l do
    score := 5 * score + pointsC c
  return score

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut pts := #[]
  for lin in dat do
  --let lin := atest[2]!
    let mut queue := #[]
    let mut err := #[]
    for c in lin.toList do
      if isOpen c then
        queue := queue.push c
      else if some queue.back? == close c
      then
        queue := queue.pop
      else
        err := err.push [queue.back?.map openC, some c].reduceOption
    if err[0]?.isNone then
      let closes := queue.reverse.map openC
      pts := pts.push closes.toList
      --IO.println s!"{closes}"
  let scores := pts.map evalCompletion
  IO.println s!"Total points: {scores}"
  let sorted := scores.qsort (· < ·)
  IO.println sorted[sorted.size/2]!

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let mut pts := #[]
  for lin in dat do
    let mut queue := #[]
    let mut err := #[]
    for c in lin.toList do
      if isOpen c then
        queue := queue.push c
      else if some queue.back? == close c
      then
        queue := queue.pop
      else
        err := err.push [queue.back?.map openC, some c].reduceOption
    if err[0]?.isNone then
      let closes := queue.reverse.map openC
      pts := pts.push closes.toList
  let scores := pts.map evalCompletion
  let sorted := scores.qsort (· < ·)
  sorted[sorted.size / 2]!

#assert part2 atest == 288957

solve 2 2776842859

end Day10
