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

structure Towels where
  patterns : Std.HashSet String
  towels : Std.HashSet String
  deriving Inhabited

/-- `OneTowel` is the structure recording the progress of writing a towel
using the available patterns.
* `towel` is the input towel.
  `patterns` contains the available patterns.
* `partials` records the current collection of tails of the initial towel,
  obtained by iteratively removing patterns from the initial string, each with its multiplicity.
* `ways` is the number of ways that we already found to write the initial towel
  using the given patterns.
-/
structure OneTowel where
  /-- `towel` is the input towel. -/
  towel : String
  /-- `patterns` contains the available patterns. -/
  patterns : Std.HashSet String
  /-- `partials` records the current collection of tails of the initial towel,
  obtained by iteratively removing patterns from the initial string, each with its multiplicity. -/
  partials : Std.HashMap String Nat
  /-- `ways` is the number of ways that we already found to write the initial towel
  using the given patterns. -/
  ways : Nat := 0
  deriving Inhabited

def drawTowel (t : OneTowel) (pat? : Bool := false) : IO Unit := do
  IO.println <| "Towel: " ++
    s!"partials: {t.partials.toArray}" ++ if pat? then s!"patterns: {t.patterns.toArray}" else ""

def inputToTowels (dat : String) : Towels :=
  match dat.splitOn "\n\n" with
    | [pats, towels] =>
      { patterns := .ofList <| pats.splitOn ", "
        towels := .ofList <| towels.splitOn "\n" |>.erase ""}
    | _ => panic "Wrong input"

#eval do
  let dat := test
  let ts := inputToTowels dat
  IO.println s!"patterns:\n{ts.patterns.toArray}\ntowels:\n{ts.towels.toArray}"

def rmOneAll (t : OneTowel) : OneTowel := Id.run do
  let mut partials : Std.HashMap String Nat := ∅
  let mut leftPats : Std.HashMap String Nat := ∅
  for (part, mult) in t.partials do
    for d in t.patterns do
      if part.startsWith d then
        leftPats := leftPats.alter (part.drop d.length) fun oldM => some <| oldM.getD 0 + mult
  partials := partials.union leftPats
  {t with partials := partials }

partial
def addMemo (m : Std.HashMap String Nat) (pats : Std.HashSet String) (t : String) :
    Std.HashMap String Nat :=
  match m[t]? with
    | some _ => m
    | none =>
      if t == "" then m.insert "" 1 else
      let towel : OneTowel := {towel := t, patterns := pats, partials := {(t, 1)}}
      let one := rmOneAll towel
      let (m', tot) := one.partials.fold (init := (m, 0)) fun (memo, tot) tw mul =>
         let fd := addMemo memo pats tw
         (fd, tot + mul * fd[tw]?.getD 0)
      m'.insert t tot

/--
info: #[(, 1), (bbr, 2), (bbrgwb, 0), (bgbr, 3), (bggr, 1), (br, 2), (brgr, 2), (brgwb, 0), (brwrr, 2), (bwurrg, 1), (g, 1), (gbbr, 4), (gbr, 3), (ggr, 1), (gr, 1), (gwb, 0), (r, 1), (rbgbr, 6), (rg, 1), (rgr, 1), (rgwb, 0), (rrbgbr, 6), (rrg, 1), (rwrr, 1), (ubwu, 0), (wb, 0), (wrr, 1), (wurrg, 0)]
-/
#guard_msgs in
#eval do
  let dat := test
  let ts := inputToTowels dat
  let mem : Std.HashMap String Nat := ts.towels.fold (addMemo · ts.patterns ·) ∅
  IO.println <| mem.toArray.qsort (·.1 < ·.1)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let ts := inputToTowels dat
  let memo := ts.towels.fold (addMemo · ts.patterns ·) ∅
  ts.towels.fold (· + if memo[·]! != 0 then 1 else 0) 0

#assert part1 test == 6

solve 1 236 file  -- takes approximately 12s

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let ts := inputToTowels dat
  let memo := ts.towels.fold (addMemo · ts.patterns ·) ∅
  ts.towels.fold (· + memo[·]!) 0

#assert part2 test == 16

solve 2 643685981770598 file  -- takes approximately 12s

end Day19
