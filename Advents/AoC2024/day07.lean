import Advents.Utils
open Lean

namespace Day07

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day07.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
Replaces the entries of the `HashSet` `h` by the result of applying each operation in `ops`
to each element of `h` and the new element `n`.
Since in our situation all operations are increasing, we only extend the `HashSet` if we obtain
a value that does not exceed the maximum target `t`.
-/
def totalsWithOpsOne (t : Nat) (h : Std.HashSet Nat) (n : Nat) (ops : Array (Nat → Nat → Nat)) :
    Std.HashSet Nat := Id.run do
  let mut j := {}
  for q in h do
    for o in ops do
      let oqn := o q n
      if oqn ≤ t then j := j.insert oqn
  return j

/--
Scan the entries of `ns`, accumulating the applications of the operations in `ops`,
making sure that the result does not exceed `t`.
-/
def totalsWithOps (t : Nat) (ns : List Nat) (ops : Array (Nat → Nat → Nat)) : Std.HashSet Nat :=
  (ns.drop 1).foldl (totalsWithOpsOne t · · ops) {ns[0]!}

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let data := dat.map (·.getNats)
  data.foldl (init := 0) fun M ns =>
    let tot := ns[0]!
    let ns := ns.drop 1
    if ns.prod < tot then M else
    if (totalsWithOps tot ns #[(· * ·), (· + ·)]).contains tot
    then M + tot
    else M

#assert part1 atest == 3749

solve 1 5540634308362

/-!
#  Question 2
-/

/--
The concatenation operation on decimal digits of natural numbers.

For instance, `cat 12 345 = 12345`.
-/
def cat (n m : Nat) : Nat := n * 10 ^ (Nat.toDigits 10 m).length + m

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let data := dat.map (·.getNats)
  data.foldl (init := 0) fun M ns =>
    let tot := ns[0]!
    let ns := ns.drop 1
    -- the earlier test speeds up overall, even with the repeated computations
    if (totalsWithOps tot ns #[(· * ·), (· + ·)]).contains tot
    then M + tot
    else
    if (totalsWithOps tot ns #[cat, (· * ·), (· + ·)]).contains tot
    then M + tot
    else M

#assert part2 atest == 11387

--set_option trace.profiler true in solve 2 472290821152397 -- takes approximately 1m30s

end Day07
