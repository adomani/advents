import Advents.Utils
open Lean

namespace Day14

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day14.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure Polymer where
  rules : Std.HashMap (List Char) Char
  poly  : Array Char

def mkPolymer (dat : Array String) : Polymer where
  rules := Id.run do
    let mut h := {}
    for d in dat do
      if let [a, b] := d.splitOn " -> " then
        h := h.insert a.toList (b.get 0)
    return h
  poly  := dat[0]!.toList.toArray

def insertOnce (p : Polymer) : Polymer where
  rules := p.rules
  poly := Id.run do
    let mut prev := p.poly[0]!
    let mut new := #[]
    for i in p.poly.erase prev do
      new := new.push prev
      match p.rules.get? [prev, i] with
        | none   => continue
        | some n => new := new.push n
      prev := i
    return new.push prev

def insert (p : Polymer) : Nat → Polymer
  | 0 => p
  | n + 1 => insert (insertOnce p) n

def tally {α : Sort _} [BEq α] [Hashable α] (as : Array α) : Std.HashMap α Nat := Id.run do
  let mut h := {}
  for a in as do
    let val := h.getD a 0
    h := h.insert a (val + 1)
  return h

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let p := mkPolymer dat
  let p := insert p 10
  let vals := (tally p.poly).toArray.qsort (·.2 < ·.2)
  vals.back!.2 - vals[0]!.2

#assert part1 atest == 1588

solve 1 2233

/-!
#  Question 2
-/

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let p := mkPolymer dat
  let p := insert p 10
  let vals := (tally p.poly).toArray.qsort (·.2 < ·.2)
  IO.println (vals.back!.2 - vals[0]!.2)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day14
