import Advents.Utils

namespace Day01

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day01.input"

/-!
#  Question 1
-/

--  Uncomment to take a look at the contents of the input file
--#eval do IO.println (← IO.FS.readFile input)

/-- `first_digit? chars` given a list of characters `chars`,
* if `chars` contains at least one digit, then
  return the corresponding natural number as `some n`,
* if `chars` contains no digits, then return `none`.
-/
def first_digit? : List Char → Option Nat
  | [] => none
  | a::as => if a.isDigit then a.toString.toNat! else first_digit? as

/-- `calibration s` takes a string `s` as input, returning the two-digit natural number
obtained by extracting from `s` the first appearing digit and the last appearing digit. -/
def calibration (s : String) : Nat :=
  let chars := s.toList
  (first_digit? chars).get! * 10 + (first_digit? chars.reverse).get!

/-- The test string for the first part. -/
def test1 := "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

#assert ((test1.splitOn "\n").map calibration).sum == 142

/-- `part1 rows` takes as input a list of strings `rows`.
It extracts the first and last digit appear in each row, forms the corresponding two-digit number
and adds up the result.

This is the function that answers the first question of Day 1. -/
def part1 (rows : Array String) : Nat :=
  (rows.map calibration).sum

#assert part1 (test1.splitOn "\n").toArray = 142

solve 1 54644

/-!
#  Question 2
-/

/-- `nums` is the list of strings representing the English words for a single non-zero digits,
in order from `"one"` to `"nine"`. -/
def nums : List String := ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

--#eval nums

/-- `smun` is the list `nums`, but where each entry is spelled backwards,
i.e. it consists of `"eno"`, `"owt"`, ..., `"enin"`. -/
def smun : List String := nums.map fun x => ⟨x.toList.reverse⟩

--#eval smun

/-- This instance allows us to add an `Option Nat` and a `Nat`:
* `none` absorbs every sum to itself (e.g. `none` is "`-∞`"),
*  while adding `some a` and `b` gives `some (a + b)`.
-/
instance : HAdd (Option Nat) Nat (Option Nat) where
  hAdd | some x, y => some (x + y) | _, _ => none

/-  The code in this comment has the same effect as the one in the previous instance.
instance : HAdd (Option Nat) Nat (Option Nat) where
  hAdd x y := return (← x) + y
-/

/-- `word_position_in s names` takes a string `s` and a list of strings `names`.
It checks if `s` begins with one of the string in `names`.
If so, then it returns `some (position_of_s_in_names)`, otherwise it returns `none`. -/
def word_position_in (s : String) (names : List String) : Option Nat :=
  let poss := List.findIdx? (String.isPrefixOf · s) names
  poss + 1

--  uncomment to check the answer
--#eval word_position_in "three" nums == some 3
--#eval word_position_in "evif" smun == some 5

--  uncomment to check that both answers are `[some 1, ..., some 9]`
--#eval nums.map (word_position_in · nums) == (List.range 9).map fun x => some (x + 1)
--#eval smun.map (word_position_in · smun) == (List.range 9).map fun x => some (x + 1)

/-- The test string for the second part. -/
def test2 := "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

/-- `first_digit_in? names chars` takes as input
* a list of strings `names` and
* a list of characters `chars`.
The function scans `chars`, looking for
* either a single digit `d`, returning `some d`;
* or a consecutive list of characters spelling a string in `names`, returning `some <position_in_names>`.
If it concludes scanning without finding any match, it returns `none`.
-/
def first_digit_in? (names : List String) : List Char → Option Nat
  | [] => none
  | l@(a::as) =>
    if a.isDigit then first_digit? l else
    let init := word_position_in ⟨l⟩ names
    match init with
      | none => first_digit_in? names as
      | n    => n

/-- `part2 rows` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (rows : Array String) : Nat :=
  let rows := rows.toList
  let firsts := rows.map fun x => Option.getD (first_digit_in? nums x.toList) 0
  let secs   := rows.map fun x => Option.getD (first_digit_in? smun x.toList.reverse) 0
  let vals := (firsts.map (10 * ·)).zipWith (· + ·) secs
  vals.sum

#assert part2 (test2.splitOn "\n").toArray == 281

solve 2 53348
