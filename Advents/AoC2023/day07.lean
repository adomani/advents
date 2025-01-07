import Advents.Utils

namespace Day07

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day07.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `parseCC s` takes as input a string `s` as in each line of `test` and returns the pair
consisting of the first part of the string, followed by the number. -/
def parseCC (s : String) : String × Nat :=
  match s.splitOn " " with
    | [s, t] => (s, t.toNat!)
    | _ => dbg_trace "parseCC: oh no!"; default

/-- `tally l` takes as input a list and returns the array of counts of each entry in the list,
sorted in decreasing order.

```lean
tally ["a", "b", "c", "b", "c", "c"] = #[3, 2, 1]
```
-/
partial
def tally {α} [BEq α] (l : List α) : Array Nat := (tallyAux l).qsort (· > ·) where
  /-- An auxiliary function to `tally`: it skips the final sorting. -/
  tallyAux {α} [BEq α] : List α → Array Nat
    | []    => #[]
    | a::rs => let (ra, rs) := rs.partition (· == a); (tallyAux rs).push (ra.length + 1)

/-  Test
#eval show MetaM _ from do
  let ls := ["abcde", "abcbd", "abacb", "aabab", "aabaa", "aaaaa"].map String.toList
  guard (ls.map tally == [#[1, 1, 1, 1, 1], #[2, 1, 1, 1], #[2, 2, 1], #[3, 2], #[4, 1], #[5]])
--/

/-- `lex` is the lexicographic order that is used sorting the "type" of game hands. -/
def lex [BEq α] [LT α] [DecidableRel (α := α) LT.lt] : List α → List α → Bool
  | [], _        => true
  | _, []        => false
  | a::as, b::bs => (b < a) || (a == b) && lex as bs

/-- `cards` is the string of cards, ordered according to the first part of the problem. -/
def cards := "AKQJT98765432"

variable (cds : String) in
/-- `orCards cds cs1 cs2` takes as input
* a string `cds`, representing an ordering of the values of the cards;
* two lists of characters `c1` and `c2` representing hands to be compared.

It returns whether the first hand beats the second one, by lexicographically
comparing the values.
-/
def orCards : List Char → List Char → Bool
  | [], _        => true
  | _, []        => false
  | a::as, b::bs => (cds.find (· == b) < cds.find (· == a)) || (a == b) && orCards as bs

variable (cds : String) (f : String × α → Array Nat) in
/-- `sortTypeStr cds f ls` sorts the list of all the cards. the function `f` converts
the parsed expression to a different format.
This is used since the hands that have the same `tally` should be compared lexicographically,
but the `tally` should be used as a first ordering. -/
def sortTypeStr (ls : Array (String × α)) : Array (String × α) :=
  ls.qsort (fun x y =>
    let tx := f x
    let ty := f y
    if tx = ty then orCards cds y.1.toList x.1.toList else
    lex (Array.toList tx) (Array.toList ty))

variable (cds : String) (f : String × Nat → Array Nat) in
/-- `parts cds f dat` combines the commons instructions of the two parts. -/
def parts (dat : Array String) : Nat :=
  let dat := dat.map parseCC
  let sorted := (sortTypeStr cds f dat).toList
  let muls := sorted.zipWith (fun x y => x.2 * y) (List.iota sorted.length)
  muls.sum

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  parts cards (fun x => tally x.1.toList) dat

#assert part1 atest == 6440

solve 1 248396258

/-!
#  Question 2
-/

/-- `addJ l` is the correction that places `J`okers in the most valuable place.
Effectively, they are assigned to the most numerous card in the hand. -/
def addJ (l : List Nat) : List Nat :=
  (l.getD 0 0 + 5 - l.sum)::l.drop 1

/-- `cards2` is the string of cards, ordered according to the second part of the problem. -/
def cards2 := "AKQT98765432J"

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  parts cards2 (fun x => (addJ <| (tally (x.1.toList.filter (· != 'J'))).toList).toArray) dat

#assert part2 atest == 5905

solve 2 246436046

end Day07
