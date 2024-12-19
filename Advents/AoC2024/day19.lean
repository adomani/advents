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

structure OneTowel where
  patterns : Std.HashSet String
  partials : Std.HashSet String
  deriving Inhabited

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

def mkOneTowel (ts : Towels) (t : String) : OneTowel where
  patterns := ts.patterns
  partials:= {t}

#eval do
  let dat := test
  let ts := inputToTowels dat
  let tw := mkOneTowel ts ts.towels.toArray[0]!
  IO.println s!"patterns:\n{tw.patterns.toArray}\ntowels:\n{tw.partials.toArray}"

def rmOne (t : OneTowel) : OneTowel :=
  {t with
    partials := t.partials.fold (init := ∅) fun h part =>
      let sts := t.patterns.fold (init := (∅ : Std.HashSet String)) fun rest d =>
        --dbg_trace "part: {part}, d: {d}, cond: {part.startsWith d}"
        if part.startsWith d then rest.insert (part.drop d.length) else rest
      h.union sts
  }

#eval do
  let dat := test
  let ts := inputToTowels dat
  IO.println s!"patterns:\n{ts.patterns.toArray}\n"
  let tw := mkOneTowel ts ts.towels.toArray[0]!
  IO.println s!"towels:\n{tw.partials.toArray}\n"
  let tw := rmOne tw
  IO.println s!"towels:\n{tw.partials.toArray}\n"
  let tw := rmOne tw
  IO.println s!"towels:\n{tw.partials.toArray}\n"
  let tw := rmOne tw
  IO.println s!"towels:\n{tw.partials.toArray}\n"

def rmAll (t : OneTowel) : Bool × OneTowel := Id.run do
  let mut t := t
  let mut oldParts := ∅
  let mut con := 0
  while oldParts != t.partials && !t.partials.contains "" do
    oldParts := t.partials
    t := rmOne t
    con := con + 1
  return (t.partials.contains "", t)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let ts := inputToTowels dat
  ts.towels.fold (init := 0) fun tot towel =>
    let tw := mkOneTowel ts towel
    let (cond, _tw) := rmAll tw
    if cond then tot + 1 else tot

#assert part1 test == 6

--set_option trace.profiler true in solve 1 236 file  -- takes just under a minute

set_option trace.profiler true in
#eval do
  let dat := test
  let dat ← IO.FS.readFile input
  let ts := inputToTowels dat
  IO.println s!"patterns:\n{ts.patterns.toArray}\n"
  let mut tot := 0
  IO.println s!"patterns:\n{ts.patterns.toArray.map (String.length) |>.qsort (· < ·)}\n"
  IO.println s!"towels:\n{ts.towels.toArray.map (String.length) |>.qsort (· < ·)}\n"

#exit
  for towel in ts.towels do
    let tw := mkOneTowel ts towel
    --IO.println s!"towels:\n{tw.partials.toArray}\n"
    let (cond, tw) := rmAll tw
    --IO.println s!"Expressible: {cond}, final: towels:\n{tw.partials.toArray}\n"
    if cond then tot := tot + 1 else
      IO.println s!"Non-expressible: {towel}"
  IO.println s!"\nExpressible towels: {tot}"

/-!
-/

--  237 -- wrong




/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day19
