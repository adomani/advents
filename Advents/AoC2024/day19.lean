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

def rmOneTowel (towels : Std.HashSet String) (part : String) : Std.HashMap String Nat :=
  towels.fold (init := ∅) fun leftPats d =>
    if part.startsWith d then
      leftPats.alter (part.drop d.length) fun oldM => some <| oldM.getD 0 + 1
      else leftPats

partial
def addMemo (memo : Std.HashMap String Nat) (towels : Std.HashSet String) (t : String) :
    Std.HashMap String Nat :=
  match memo[t]? with
    | some _ => memo
    | none =>
      if t == "" then memo.insert "" 1 else
      let one := rmOneTowel towels t
      let (m', tot) := one.fold (init := (memo, 0)) fun (memo, tot) tw mul =>
         let fd := addMemo memo towels tw
         (fd, tot + mul * fd[tw]?.getD 0)
      m'.insert t tot

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
