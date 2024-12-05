import Advents.Utils
open Lean

namespace Day05

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day05.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray
#check List.lookup
def inputToDat (s : Array String) (reorder? : Bool) : Nat := Id.run do
  let mut tot := 0
  let mut lookup : Std.HashSet (Nat × Nat) := {}
  for d in s do
    match d.getNats with
      | [l, r] => lookup := lookup.insert (l, r)
      | [] => continue
      | ps =>
        let as := ps.toArray
        let sorted := as.qsort fun a b => ! lookup.contains (b, a)
        if as != sorted then
          dbg_trace "extract {as[as.size / 2]!}"
          tot := tot + sorted[as.size / 2]!
        --dbg_trace "{as == sorted}: {(as, sorted)}"
  dbg_trace lookup.toArray
  return tot

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  IO.println <| inputToDat dat true


/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day05
