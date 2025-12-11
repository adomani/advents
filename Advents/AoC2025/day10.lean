import Advents.Utils
open Std

namespace AoC2025_Day10

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day10" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure machine where
  ls : Array Bool
  bs : Array (Array Nat)
  js : Array Nat
  ons : Array Bool
  deriving Inhabited, BEq, Hashable

instance : ToString machine where
  toString := fun
    | {ls := l, bs := b, js := _j, ons := os} => s!"current: {l}\n{b}\ntarget: {os}"

def inputToM (dat : Array String) : Array machine :=
  dat.foldl (init := #[]) fun tot s => tot.push <|
  let i1 := s.takeWhile (· != '(') |>.drop 1 |>.dropRight 2
  let r2 := s.dropWhile (· != '(')
  let i2 := r2.takeWhile (· != '{')
  let i3 := r2.dropWhile (· != '{')
  let ons := i1.foldl (init := #[]) fun tot s => tot.push (s == '#')
  { ls := ons.map fun _ => false
    bs := (i2.splitOn " ").foldl (init := #[]) (·.push <| ·.getNats.toArray) |>.pop
    js := i3.getNats.toArray
    ons := ons }

structure state extends machine where
  con : Nat
  deriving Inhabited, BEq, Hashable

instance : ToString state where
  toString := fun | s@{con := n, ..} => s!"{s.tomachine}\n{n}"

def toggleOne (l : Array Bool) (b : Array Nat) : Array Bool :=
  b.foldl (init := l) fun tot n => tot.modify (n) (!·)

#guard
  let l := #[false, true, true, false]
  let b := #[3]
  toggleOne l b == #[false, true, true, true]

#guard
  let l := #[false, true, true, false]
  let b := #[1, 3]
  toggleOne l b == #[false, false, true, true]

def toggle (m : machine) (b : Array Nat) : machine :=
  {m with ls := toggleOne m.ls b}

def MtoS (m : machine) (c : Nat) : state where
  ls := m.ls
  bs := m.bs
  js := m.js
  ons := m.ons
  con := c

def stepSingle (s : state) : HashSet state :=
  s.bs.foldl (init := ∅) fun tot n =>
    tot.insert (MtoS (toggle s.tomachine n) (s.con + 1))

def step (h : HashSet state) : HashSet state :=
  h.fold (init := ∅) (·.union <| stepSingle ·)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let ms := inputToM dat
  let mut tot := 0
  for m in ms do
    let s := MtoS m 0
    let mut h : HashSet state := {s}
    let mut con := 0
    let mut found := false
    while !found do
      con := con + 1
      h := step h
      for a in h do
        found := found || a.ls == a.ons
        if found then
          tot := tot + a.con
          break
  return tot

#assert part1 atest == 7

set_option trace.profiler true in solve 1 396

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day10
