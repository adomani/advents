import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/i01.txt"

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

/-- `last_digit? chars` behaves like `first_digit?`, but starting from the end of the list. -/
def last_digit? (l : List Char) : Option Nat :=
  first_digit? l.reverse

/-- `calibration s` takes a string `s` as input, returning the two-digit natural number
obtained by extracting from `s` the first appearing digit and the last appearing digit. -/
def calibration (s : String) : Nat :=
  let chars := s.toList
  (first_digit? chars).get! * 10 + (last_digit? chars).get!

/-- The test string for the first part. -/
def test : String :=
"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

#eval show MetaM _ from do
  let rows := test.splitOn "\n"
  let answer := (rows.map calibration).sum
--  IO.println <| answer
  guard (answer == 142)

/-- `total_calibration rows` takes as input a list of strings `rows`.
It extracts the first and last digit appear in each row, forms the corresponding two-digit number
and adds up the result.

This is the function that answers the first question of Day 1. -/
def total_calibration (rows : List String) : Nat :=
  (rows.map calibration).sum

--  Question 1: answer 54644
#eval show MetaM _ from do
  let _rows := test.splitOn "\n"
  let _rows := (← IO.FS.lines input).toList
  let answer := total_calibration _rows
  IO.println <| answer
  guard (answer = 54644)

/-- `nums` is the list of strings representing the English words for a single non-zero digits,
in order from `"one"` to `"nine"`. -/
def nums : List String := ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

--#eval nums

/-- `smun` is the list `nums`, but where each entry is spelled backwards,
i.e. it consists of `"eno"`, `"owt"`, ..., `"enin"`. -/
def smun : List String := nums.map fun x => ⟨x.toList.reverse⟩

--#eval smun

instance : HAdd (Option Nat) Nat (Option Nat) where
  hAdd | none, _ => none | some x, y => some (x + y)

/-- `word_isNat? s` takes a string `s` and checks if it begins with one of the string in `nums`,
i.e. if it begins with the English word for a single non-zero digit.
If so, then it returns `some corresponding_nat`, otherwise it returns `none`. -/
def word_isNat? (s : String) : Option Nat :=
  let poss := List.findIdx? (String.isPrefixOf · s) nums
  poss + 1

--  uncomment to check that the answer is `3`
--#eval word_isNat? "three" == some 3

/-- `drow_isNat? s` is like `word_isNat? s`, except that it looks for digits spelled backwards. -/
def drow_isNat? (s : String) : Option Nat :=
  let poss := List.findIdx? (String.isPrefixOf · s) smun
  poss + 1

--  uncomment to check that the answer is `[some 1, ..., some 9]`
--#eval nums.map word_isNat? == (List.range 9).map fun x => some (x + 1)

def test2 : String :=
"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

def first_digit2? : List Char → Option Nat
  | [] => none
  | l@(a::as) =>
    if a.isDigit then first_digit? l else
    let init := word_isNat? (⟨l⟩)
    match init with
      | none => first_digit2? as
      | n    => n

partial
def last_digit2? (s : List Char) : Option Nat :=
  match s.reverse with
    | [] => none
    | l@(a::as) =>
      if a.isDigit then last_digit? s else
      let init := drow_isNat? (⟨l⟩)
      match init with
        | none => last_digit2? as.reverse
        | n    => n

#eval show MetaM _ from do
  let rows := test2.splitOn "\n"
  let rows := (← IO.FS.lines input).toList
  let firsts := rows.map ((Option.getD · 0) ∘ first_digit2? ∘ String.toList)
  let secs   := rows.map ((Option.getD · 0) ∘  last_digit2? ∘ String.toList)
--  IO.println <| firsts.zip secs
  let vals := (firsts.map (10 * ·)).zipWith (· + ·) secs
  let answer := vals.sum
  IO.println answer
  guard (answer = 53348)
