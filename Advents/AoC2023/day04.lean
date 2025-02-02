import Advents.Utils

namespace Day04

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day04.input"

--#eval do IO.println <| ← IO.FS.readFile input

/-!
#  Question 1
-/

/-- `test` is the test string for Day 4. -/
def test := "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `count_powers l r` takes as input two arrays of `Nat`s and returns
* `0`, if no entry of `r` appears in `l`;
* `2 ^ c`, if `c` entries of `r` appear in `l`.
-/
def count_powers (l r : Array Nat) : Nat :=
  let appearing := r.filter (· ∈ l)
  let appsize := appearing.size
  if appsize == 0 then 0 else 2 ^ (appsize - 1)

#assert String.getNats "askdlkaj12kj3lkj5" == [12, 3, 5]

/-- `parseCard s` takes as input a string, assumes that it is of the form
`Card <index>: <space_separated_nats> | <space_separated_nats>`
and returns the two  `Array` extracted from the two `<space_separated_nats>` substrings. -/
def parseCard (s : String) : Array Nat × Array Nat :=
  let sdrop := s.dropWhile (· != ':')
  if let [s1, s2] := sdrop.splitOn "|" then
    (s1.getNats.toArray, s2.getNats.toArray)
  else
    default

/-- `part1 rows` takes as input an array of strings and returns the natural number answering
the first question of Day 4. -/
def part1 (rows : Array String) : Nat :=
  Id.run do
  let mut tot := 0
  for row in rows do
    let (l, r) := parseCard row
    tot := tot + count_powers l r
  return tot

#assert part1 atest == 13

solve 1 17782

/-!
#  Question 2
-/

/-- We add two lists of natural numbers by padding with `0` the shorter of the two lists. -/
instance : Add (List Nat) where
  add := radd where /-- `radd` is the internal function to add two list of natural numbers-/ radd
    | l, [] => l
    | [], l => l
    | a::as, b::bs => (a + b) :: radd as bs

/-- We multiply a natural number and a list of natural numbers by multiplying each entry of
the list by the natural number. -/
instance : HMul Nat (List Nat) (List Nat) where
  hMul a l := l.map (a * ·)

#assert [1, 2, 3] + [4, 5, 6, 7] == [5, 7, 9, 7]

/-- `get_value s` assigns to each row of the input its value according to the rules for
the second part of Day 4. -/
def get_value (s : String) : Nat :=
  let (l, r) := parseCard s
  (r.filter (· ∈ l)).size

/-- `part2 rows` takes as input an array of strings and returns the natural number answering
the second question of Day 4. -/
def part2 (rows : Array String) : Nat :=
  let rows := rows.toList
  Id.run do
    let mut cards := 0
    let mut mults := List.replicate rows.length 1
    for row in rows.dropLast do
      let val := get_value row
      let curr := mults.getD 0 0
      cards := cards + curr
      mults := List.replicate val curr + (mults.drop 1)
    return cards + mults.getD 0 0

#assert part2 atest == 30

solve 2 8477787

end Day04
