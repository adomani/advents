import Advents.Utils

namespace Day05

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2024"/"day05" : FilePath).withExtension "input"

/-!
#  Question 1
-/

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

/-- Converts the input array of strings into the solution to the problem.
If a line contains a pair of natural numbers, then store the reversed pair in a `HashSet`.
If a line contains a number of natural numbers that is not 2, then
* sort the corresponding array and compare it to the original;
* if the `reorder?` flag is `false`, then keep a tally of the middles pages only when the
  list is already sorted;
* if the `reorder?` flag is `true`, then keep a tally of the middles pages only when the
  list is *not* already sorted.
-/
def inputToDat (s : Array String) (reorder? : Bool) : Nat :=
  let (lookup, rest) : Std.HashSet (Nat × Nat) × Std.HashSet (List Nat) :=
    s.foldl (init := default) fun (lookup, rest) d => match d.getNats with
      | [l, r] => (lookup.insert (l, r), rest)
      | [] => (lookup, rest)
      | ps => (lookup, rest.insert ps)
  rest.fold (init := 0) fun tot as =>
    let sorted := as.mergeSort fun a b => ! lookup.contains (b, a)
    let cond := if reorder? then as != sorted else as == sorted
    if cond then tot + sorted[as.length / 2]! else tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := inputToDat dat false

#assert part1 atest == 143

solve 1 6267

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := inputToDat dat true

#assert part2 atest == 123

solve 2 5184

end Day05
