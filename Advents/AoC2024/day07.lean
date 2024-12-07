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
  | [], s@(_::_::_) => 0 --panic s!"Ran out of operations {s}"
  --| [_], [a] => a
  --| [], _ => 0
--#check Perm

#eval applyList [(·+·), (·*·), (·*·), (·*·)] [0, 2, 3]

def toBin : Nat → Nat → Array Nat
  | 0, _ => #[]
  | sz + 1, n =>
    if 2 ^ (sz + 1) ≤ n then #[] else
    if n ≤ 1 then (List.replicate (sz + 1) 0).toArray.push n else
      if n < 2 ^ (sz) then #[0] ++ toBin (sz) n
      else  #[1] ++ toBin (sz - 1) (n - 2 ^ (sz))


def toBin1 : Nat → Array (List (Nat → Nat → Nat))
  | 0 => #[]
  | sz + 1 =>
    (toBin1 sz).foldl (fun os _ => (os.map ((· + ·)::·)) ++ (os.map ((· * ·)::·))) #[[]]

--#exit
partial
def toBin' (sz n : Nat) : Array Nat :=
  if 2^sz ≤ n then #[] else
  if n ≤ 1 then (List.replicate sz 0).toArray.push n else
    if n < 2^(sz-1) then #[0] ++ toBin' (sz - 1) n
    else  #[1] ++ toBin' (sz - 1) (n - 2^(sz-1))

def cat (n m : Nat) : Nat := n * 10 ^ (Nat.toDigits 10 m).length + m

def tots1 (t : Nat) (h : Std.HashSet Nat) (n : Nat) : Std.HashSet Nat := Id.run do
  let mut j := {}
  for q in h do
    let aqn := q + n
    if aqn ≤ t then j := j.insert aqn
    let mqn := q * n
    if mqn ≤ t then j := j.insert mqn
    let cqn := cat q n
    if cqn ≤ t then j := j.insert cqn
  return j

def tots (t : Nat) (ns : List Nat) : Std.HashSet Nat := Id.run do
  let mut j := {ns[0]!}
  for n in ns.drop 1 do
    j := tots1 t j n
  return j

def tots? (t : Nat) (ns : List Nat) : Bool := (tots t ns).contains t

--def tots (t : Nat) (ns : List Nat) : Bool := Id.run do
--  let mut tallies := #[ns[0]!]
--  let not := Id.run do
--    for n in ns.drop 1 do
--      tallies.map fun q =>
--        let aqn := q + n
--        let mqn := q * n
--        if qn ≤ t then mqn else none
--      tallies := tallies.filterMap fun q => let qn := q * n; if qn ≤ t then some qn else none
--  return tallies.any (· == t)

#eval do
  let mut M := 0
  let dat := atest
  let dat ← IO.FS.lines input
  for a in dat do
    let ns := a.getNats
    let tot := ns[0]!
    let ns := ns.drop 1
    if tots? tot ns then
      M := M + tot
      --dbg_trace "{tot}: {(tots tot ns).toList}"
  dbg_trace M


#exit

#eval (toBin1 3).map (·.length)
#eval (toBin1 2)[0]![0]! 1 1

#eval toBin' 5  1
#eval toBin  5  1
#eval toBin  5 10
#eval toBin' 5 10
-- 5482452214569 -- too low
-- 5510497716043 wrong
-- 5540634308362
-- 5540634308465 -- too high
--#exit
#eval do
  let dat ← IO.FS.lines input
  let dat := atest --[atest[]!]
  --let ops : Array (Nat → Nat → Nat) := #[(· + ·), (· * ·)]
  --let mut hop : Std.HashSet (List Nat) := {}
  let hops : Array (List (Nat → Nat → Nat)) := toBin1 4 --#[]
  --for i in [0:2^12] do
  --  let mut con := []
  --  for j in toBin 13 i do
  --    con := ops[j]!::con
  --  hops := hops.push con

  let mut correct := 0
  let mut con := false
  for a in dat do
    con := false
    let ns := a.getNats
    let tot := ns[0]!
    let ns := ns.drop 1
    --dbg_trace (tot, ns)

    for o in hops do
      --dbg_trace o.length
      if !con then
        --dbg_trace o.map fun op : Nat → Nat → Nat => (op 1 1 == 2)
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
