import Advents.Utils
open Lean

namespace Day14

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day14.input"

/-!
#  Question 1
-/

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
  rules : Std.HashMap (Char × Char) Char
  poly  : Std.HashMap (Char × Char) Nat
  ends  : Char × Char

def mkPolymer (dat : Array String) : Polymer where
  rules := Id.run do
    let mut h := {}
    for d in dat do
      if let [a, b] := d.splitOn " -> " then
        h := h.insert (a.get 0, a.get ⟨1⟩) (b.get 0)
    return h
  poly  := Id.run do
    let mut h := {}
    let pol := dat[0]!
    for i in [0:pol.length - 1] do
      let new := (pol.get ⟨i⟩, pol.get ⟨i + 1⟩)
      let fd := h.getD new 0
      h := h.insert new (fd + 1)
    return h
  ends := (dat[0]!.get 0, dat[0]!.back)

def insertOnce (p : Polymer) : Polymer where
  rules := p.rules
  poly := Id.run do
    let mut new := {}
    for (i@(l, r), val) in p.poly do
      match p.rules.get? i with
        | none => new := new.insert i val
        | some mid =>
          let lval := new.getD (l, mid) 0
          new := new.insert (l, mid) (lval + val)
          let rval := new.getD (mid, r) 0
          new := new.insert (mid, r) (rval + val)
    return new
  ends := p.ends

def insert (p : Polymer) : Nat → Polymer
  | 0 => p
  | n + 1 => insert (insertOnce p) n

def countSplit (p : Polymer) : Std.HashMap Char Nat := Id.run do
  let mut h := {}
  for ((l, _r), val) in p.poly do
    let acc := h.getD l 0
    h := h.insert l (acc + val)
  let acc := h.getD p.ends.2 0
  h := h.insert p.ends.2 (acc + 1)
  return h

def parts (dat : Array String) (n : Nat) : Nat :=
  let ps := mkPolymer dat
  let p' := insert ps n
  let fin := (countSplit p').toArray.qsort (·.2 < ·.2)
  fin.back!.2 - fin[0]!.2

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := parts dat 10

#assert part1 atest == 1588

solve 1 2233

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := parts dat 40

#assert part2 atest == 2188189693529

solve 2 2884513602164

end Day14
