import Advents.Utils
open Lean

namespace Day19

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day19.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `Towels` is the structure recording the available `towels` and `designs`.
* `towels` contains the available towels: these should be combined to form the given `designs`.
* `designs` contains all the input designs.
-/
structure Towels where
  /-- `towels` contains the available towels:
  these should be combined to form the given `designs`. -/
  towels : Std.HashSet String
  /-- `designs` contains all the input designs. -/
  designs : Std.HashSet String
  deriving Inhabited

/-- Converts the input data into `Towels`. -/
def inputToTowels (dat : String) : Towels :=
  match dat.splitOn "\n\n" with
    | [towels, designs] =>
      { towels := .ofList <| towels.splitOn ", "
        designs := .ofList <| designs.splitOn "\n" |>.erase ""}
    | _ => panic "Wrong input"

/--
info: patterns:
#[b, br, bwu, g, gb, r, rb, wr]
designs:
#[bbrgwb, bggr, brgr, brwrr, bwurrg, gbbr, rrbgbr, ubwu]
-/
#guard_msgs in
#eval do
  let dat := test
  let ts := inputToTowels dat
  IO.println
    s!"patterns:\n{ts.towels.toArray.qsort (· < ·)}\ndesigns:\n{ts.designs.toArray.qsort (· < ·)}"

/--
Given a set `towels` of available towels and a `pattern`, return all the strings obtained from
`pattern` by removing an element of `towels` from the start of `pattern`.
-/
def rmOneTowel (towels : Std.HashSet String) (pattern : String) : Std.HashSet String :=
  towels.fold (init := ∅) fun leftPats d =>
    if pattern.startsWith d then
      leftPats.insert (pattern.drop d.length)
    else leftPats

/--
`memo` stores the number of ways to write a given string, representing a design, as a
concatenation nof `towels`.

`addMemo` extends the currently known set of values by adding the value for `pattern` and
its trailing substrings that are visited in the process of computing the value.
-/
partial
def addMemo (memo : Std.HashMap String Nat) (towels : Std.HashSet String) (pattern : String) :
    Std.HashMap String Nat :=
  match memo[pattern]? with
    | some _ => memo
    | none =>
      if pattern == "" then memo.insert "" 1 else
      let one := rmOneTowel towels pattern
      let (m', tot) := one.fold (init := (memo, 0)) fun (memo, tot) tw =>
         let fd := addMemo memo towels tw
         (fd, tot + fd[tw]!)
      m'.insert pattern tot

/--
info: #[(, 1), (bbr, 2), (bbrgwb, 0), (bgbr, 3), (bggr, 1), (br, 2), (brgr, 2), (brgwb, 0), (brwrr, 2), (bwurrg, 1), (g, 1), (gbbr, 4), (gbr, 3), (ggr, 1), (gr, 1), (gwb, 0), (r, 1), (rbgbr, 6), (rg, 1), (rgr, 1), (rgwb, 0), (rrbgbr, 6), (rrg, 1), (rwrr, 1), (ubwu, 0), (wb, 0), (wrr, 1), (wurrg, 0)]
-/
#guard_msgs in
#eval do
  let dat := test
  let ts := inputToTowels dat
  let mem : Std.HashMap String Nat := ts.designs.fold (addMemo · ts.towels ·) ∅
  IO.println <| mem.toArray.qsort (·.1 < ·.1)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let ts := inputToTowels dat
  let memo := ts.designs.fold (addMemo · ts.towels ·) ∅
  ts.designs.fold (· + if memo[·]! != 0 then 1 else 0) 0

#assert part1 test == 6

set_option trace.profiler true in solve 1 236 file  -- takes approximately 10s

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let ts := inputToTowels dat
  let memo := ts.designs.fold (addMemo · ts.towels ·) ∅
  ts.designs.fold (· + memo[·]!) 0

#assert part2 test == 16

set_option trace.profiler true in solve 2 643685981770598 file  -- takes approximately 10s

end Day19
