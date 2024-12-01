import Advents.Utils
open Lean

namespace Day04

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day04.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"

/-- Convert the input string into the array of numbers called in the game, and the bingo cards. -/
def inputToDat (i : String) : Array Nat × Array (Array (Array Nat)) :=
  if let nums :: cards := i.splitOn "\n\n" then
    let splitCards := cards.toArray.map fun d => (d.splitOn "\n").map (·.getNats.toArray)
    (nums.getNats.toArray, splitCards.map (·.toArray))
  else
    default

/--
Takes as input the array of numbers extracted in a game of bingo and a bingo card and returns
how many extractions are needed for the card to form a bingo in one of its rows,
or `none` if the card never wins.
-/
def findMinBingoRows (nums : Array Nat) (card : Array (Array Nat)) : Option Nat :=
  let ref := nums.size
  let arr := Array.range ref
  Id.run do
  let mut min := ref
  for row in card do
    let fd? := row.map fun r => arr.find? (nums[·]! == r)
    if fd?.contains none then continue
    let newMin := fd?.reduceOption.maxD ref
    if newMin < min then min := newMin
  if min == ref then none else some min

/--
Takes as input the array of numbers extracted in a game of bingo and a bingo card and returns
how many extractions are needed for the card to win, or `none` if the card never wins.
-/
def findMinBingo (nums : Array Nat) (card : Array (Array Nat)) : Option (Nat × Nat) :=
  let mins := #[findMinBingoRows nums card, findMinBingoRows nums card.transpose1]
  mins.reduceOption.min?.map fun d => (d, nums[d]!)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let (nums, cards) := inputToDat dat
  let mins := cards.filterMap fun c => (findMinBingo nums c).map (Prod.mk · c)
  let mins := mins.qsort (·.1 < ·.1)
  let ((rg, val), card) := mins[0]!
  let called := nums.take (rg + 1)
  (val * (card.flatten.filter (!called.contains ·) |>.sum))

#assert part1 test == 4512

solve 1 14093 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let (nums, cards) := inputToDat dat
  let mins := cards.filterMap fun c => (findMinBingo nums c).map (Prod.mk · c)
  let mins := mins.qsort (·.1 > ·.1)
  let ((rg, val), card) := mins[0]!
  let called := nums.take (rg + 1)
  (val * (card.flatten.filter (!called.contains ·) |>.sum))

#assert part2 test == 1924

solve 2 17388 file

end Day04
