import Advents.Utils
open Std

/-!
The inputs are switches to flips, patterns for flipping and joltages.
-/

namespace AoC2025_Day10

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day10" : FilePath).withExtension "input"

/-!
#  Question 1

In part 1, we should determine the minimum number of flips in patterns that take us from all switches being off to creating the input pattern.

We need to return the total minimum number of required switches.
-/

/-- `test` is the test string for the problem. -/
def test := "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure state where
  ls : Array Bool
  bs : Array (Array Nat)
  js : Array Nat
  con : Nat
  deriving Inhabited, BEq, Hashable

instance : ToString state where
  toString := fun
    | {ls := l, bs := b, js := _j, con := c} => s!"current: {l}\n{b}\n{c}"

def inputToM (dat : Array String) : Array state :=
  dat.foldl (init := #[]) fun tot s => tot.push <|
  let i1 := s.takeWhile (· != '(') |>.drop 1 |>.dropRight 2
  let r2 := s.dropWhile (· != '(')
  let i2 := r2.takeWhile (· != '{')
  let i3 := r2.dropWhile (· != '{')
  let ons := i1.foldl (·.push <| · == '#') #[]
  { ls := ons
    bs := (i2.splitOn " ").foldl (·.push <| ·.getNats.toArray) #[] |>.pop
    js := i3.getNats.toArray
    con := 0 }

def toggleOne (l : Array Bool) (b : Array Nat) : Array Bool :=
  b.foldl (·.modify · (!·)) l

#guard
  let l := #[false, true, true, false]
  let b := #[3]
  toggleOne l b == #[false, true, true, true]

#guard
  let l := #[false, true, true, false]
  let b := #[1, 3]
  toggleOne l b == #[false, false, true, true]

def toggle (m : state) (b : Array Nat) : state :=
  {m with ls := toggleOne m.ls b, con := m.con + 1}

def stepSingle (s : state) : HashSet state :=
  s.bs.foldl (·.insert <| toggle s ·) ∅

def step (h : HashSet state) : HashSet state :=
  h.fold (·.union <| stepSingle ·) ∅

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let ms := inputToM dat
  let mut tot := 0
  for s in ms do
    let mut h : HashSet state := {s}
    let mut found := false
    while !found do
      h := step h
      for a in h do
        found := found || a.ls.all (!·)
        if found then
          tot := tot + a.con
          break
  return tot

#assert part1 atest == 7

set_option trace.profiler true in solve 1 396

/-!
#  Question 2

In part 2, we should do something similar, but adding to joltages, instead of flipping switches.
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day10
