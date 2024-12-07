import Advents.Utils
open Lean

namespace Day07

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day07.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

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

def applyList : List (Nat → Nat → Nat) → List Nat → Nat
  | _, [a] => a
  | _, [] => 0
  | [o], [a, b] => o a b
  | o::os, a::b::cs => applyList os (o a b :: cs)
  ---| [], [a] => a
  | [], s@(_::_::_) => panic s!"Ran out of operations {s}"
  --| [_], [a] => a
  --| [], _ => 0
--#check Perm

#eval applyList [(·+·), (·*·), (·*·), (·*·)] [0, 2, 3]

def toBin' : Nat → Nat → Array Nat
  | 0, _ => #[]
  | sz + 1, n =>
    if 2 ^ (sz + 1) ≤ n then #[] else
    if n ≤ 1 then (List.replicate (sz + 1) 0).toArray.push n else
      if n < 2 ^ (sz) then #[0] ++ toBin' (sz) n
      else  #[1] ++ toBin' (sz - 1) (n - 2 ^ (sz))


partial
def toBin (sz n : Nat) : Array Nat :=
  if 2^sz ≤ n then #[] else
  if n ≤ 1 then (List.replicate sz 0).toArray.push n else
    if n < 2^(sz-1) then #[0] ++ toBin (sz - 1) n
    else  #[1] ++ toBin (sz - 1) (n - 2^(sz-1))

#eval toBin' 5  1
#eval toBin  5  1
#eval toBin  5 10
#eval toBin' 5 10
-- 5510497716043 wrong
#check BitVec
#eval do
  let dat ← IO.FS.lines input
  let dat := [atest[]!]
  let ops : Array (Nat → Nat → Nat) := #[(· + ·), (· * ·)]
  --let mut hop : Std.HashSet (List Nat) := {}
  let mut hops : Array (List (Nat → Nat → Nat)) := #[]
  for i in [0:2^12] do
    let mut con := []
    for j in toBin 13 i do
      con := ops[j]!::con
    hops := hops.push con

  --for i in BitVec 1 do
  --  IO.println i
  --for x in Bool × Bool do
  --  IO.println x
  let mut correct := 0
  let mut con := false
  for a in dat do
    con := false
    let ns := a.getNats
    let tot := ns[0]!
    let ns := ns.drop 1
    dbg_trace (tot, ns)

    for o in hops do
      dbg_trace o.length
      if !con then
        dbg_trace o.map fun op : Nat → Nat → Nat => (op 1 1 == 2)
        if applyList o ns == tot then
          correct := correct + tot
          con := true
          --IO.println s!"Found {tot}"
  IO.println s!"Total: {correct}"
      --IO.println <| applyList o ns




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

end Day07
