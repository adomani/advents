import Std

def List.sum {α} [Add α] [Inhabited α] : List α → α
  | []    => default
  | [m]   => m
  | m::ms => m + ms.sum

def Array.sum {α} [Add α] [Inhabited α] (l : Array α) : α :=
  l.toList.sum


def input : System.FilePath := "Advents/i01.txt"

#eval do
  dbg_trace (← IO.FS.lines input)

def first_digit? : List Char → Option Nat
  | [] => none
  | a::as => if a.isDigit then (⟨[a]⟩ : String).toNat! else first_digit? as

def last_digit? (l : List Char) : Option Nat :=
  first_digit? l.reverse

def calibration (s : String) : Nat :=
  let chars := s.toList
  (first_digit? chars).get! * 10 +
  (last_digit? chars).get!

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
#eval do
  let rows := test.splitOn "\n"
  let rows := (← IO.FS.lines input).toList
  IO.println <| total_calibration rows
#check String.isPrefixOf

def word_isNat? (s : String) : Option Nat :=
  if "one".isPrefixOf s   then some 1 else
  if "two".isPrefixOf s   then some 2 else
  if "three".isPrefixOf s then some 3 else
  if "four".isPrefixOf s  then some 4 else
  if "five".isPrefixOf s  then some 5 else
  if "six".isPrefixOf s   then some 6 else
  if "seven".isPrefixOf s then some 7 else
  if "eight".isPrefixOf s then some 8 else
  if "nine".isPrefixOf s  then some 9 else
  none

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

#eval do
  let rows := test2.splitOn "\n"
  let rows := (← IO.FS.lines input).toList
  let firsts := rows.map ((Option.getD · 0) ∘ first_digit2? ∘ String.toList)
  let secs   := rows.map ((Option.getD · 0) ∘  last_digit2? ∘ String.toList)
--  IO.println <| firsts.zip secs
  let vals := (firsts.map (10 * ·)).zipWith (· + ·) secs
--  IO.println <| vals
  IO.println <| vals.sum
