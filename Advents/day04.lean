import Advents.Utils

open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/i04.txt"

--#eval do IO.println <| ← IO.FS.readFile input

/-!
#  Question 1
-/

def count_powers (l r : Array Nat) : Nat :=
  let appearing := r.filter (· ∈ l)
  let appsize := appearing.size
  if appsize == 0 then 0 else 2 ^ (appsize - 1)

partial
def getNumbers (l : List Char) : List Nat :=
  let l1 := l.dropWhile (!Char.isDigit ·)
  if l1.length == 0 then [] else
    let d1 := String.toNat! ⟨l1.takeWhile (Char.isDigit ·)⟩
    let fin := getNumbers (l1.dropWhile (Char.isDigit ·))
  d1 :: fin

--#assert getNumbers "askdlkaj12kj3lkj5".toList == [12, 3, 5]

def parseCard (s : String) : Array Nat × Array Nat :=
  let sdrop := s.dropWhile (· != ':')
  if let [s1, s2] := sdrop.splitOn "|" then
    ((getNumbers s1.toList).toArray, (getNumbers s2.toList).toArray)
  else
    default

def part1 (rows : Array String) : Nat :=
  Id.run do
  let mut tot := 0
  for row in rows do
    let (l, r) := parseCard row
    tot := tot + count_powers l r
  return tot

def test := "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

--#assert part1 (test.splitOn "\n").toArray == 13
/-
#eval show MetaM _ from do
  let tot := part1 (← IO.FS.lines input)
  guard (tot == 17782)
--/

#eval return part1 (← IO.FS.lines input)

/-!
#  Question 2
-/

def radd : List Nat → List Nat → List Nat
  | l, [] => l
  | [], l => l
  | a::as, b::bs => (a + b) :: radd as bs

instance : Add (List Nat) where
  add := radd

instance : HMul Nat (List Nat) (List Nat) where
  hMul a l := l.map (a * ·)

#eval [1, 2, 3] + [4, 5, 6, 7]

def get_value (s : String) : Nat :=
  let (l, r) := parseCard s
  (r.filter (· ∈ l)).size

def part2 (rows : List String) : Nat :=
  Id.run do
    let mut cards := 0
    let mut mults := List.replicate rows.length 1
    for row in rows.dropLast do
      let val := get_value row
      let curr := mults.getD 0 0
      cards := cards + curr
      mults := List.replicate val curr + (mults.drop 1)
    return cards + mults.getD 0 0

--#assert part2 (test.splitOn "\n") == 30
/-
#eval show MetaM _ from do
  let rows := (← IO.FS.lines input).toList
  guard (part2 rows == 8477787)
--/

#eval return IO.println <| part2 (← IO.FS.lines input).toList
