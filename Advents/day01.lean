import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/i01.txt"

--  Let's take a look at the contents of the input file
#eval do
  IO.println (← IO.FS.readFile input)

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

def test : String :=
"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

#eval do
  let rows := test.splitOn "\n"
  IO.println rows
  IO.println <| rows.map (first_digit? ∘ String.toList)
  IO.println <| rows.map calibration
  IO.println <| (rows.map calibration).sum

def total_calibration (rows : List String) : Nat :=
  (rows.map calibration).sum

--  Question 1: answer 54644
#eval show MetaM _ from do
  let rows := test.splitOn "\n"
  let rows := (← IO.FS.lines input).toList
  let answer := total_calibration rows
  IO.println <| answer
  guard (answer = 54644)

instance : HAdd (Option Nat) Nat (Option Nat) where
  hAdd | none, _ => none | some x, y => some (x + y)

def word_isNat? (s : String) : Option Nat :=
  let words := ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  let poss := List.findIdx? (String.isPrefixOf · s) words
  poss + 1

#eval word_isNat? "three"

def drow_isNat? (s : String) : Option Nat :=
  if "eno".isPrefixOf s   then some 1 else
  if "owt".isPrefixOf s   then some 2 else
  if "eerht".isPrefixOf s then some 3 else
  if "ruof".isPrefixOf s  then some 4 else
  if "evif".isPrefixOf s  then some 5 else
  if "xis".isPrefixOf s   then some 6 else
  if "neves".isPrefixOf s then some 7 else
  if "thgie".isPrefixOf s then some 8 else
  if "enin".isPrefixOf s  then some 9 else
  none

#eval
  let words := ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  dbg_trace words.map word_isNat?
  0

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
