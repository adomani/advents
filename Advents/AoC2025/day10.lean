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
  ons : Array Bool --:= tomachine.ls.map fun _ => false
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
  --l.zipWith (bs := b) fun tf n => _

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
  ls := m.ls --.map fun _ => false
  bs := m.bs
  js := m.js
  ons := m.ons
  con := c


def stepSingle (s : state) : HashSet state :=
  s.bs.foldl (init := ∅) fun tot n =>
    tot.insert (MtoS (toggle s.tomachine n) (s.con + 1))
  --{(∅ : HashSet state) with con := s.con + 1}

#eval do
  let dat := atest
  let ms := inputToM dat
  dbg_trace String.intercalate "\n\n" (ms.toList.map (s!"{·}"))
  dbg_trace "\n"
  let ⟨ls, bs, js, ons⟩ := ms[0]!
  dbg_trace ({con := 0, ls := ls, bs := bs, js := js, ons := ons} : state)
  dbg_trace "\n"
  for m in stepSingle (MtoS ms[0]! 0) do
    dbg_trace m

def step (h : HashSet state) : HashSet state :=
  h.fold (init := ∅) (·.union <| stepSingle ·)

set_option trace.profiler true in
#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let ms := inputToM dat
  --dbg_trace String.intercalate "\n\n" (ms.toList.map (s!"{·}"))
  --dbg_trace "\n"
  --let ⟨ls, bs, js, ons⟩ := ms[0]!
  --dbg_trace ({con := 0, ls := ls, bs := bs, js := js, ons := ons} : state)
  --dbg_trace "\n"
  --let s := MtoS ms[0]! 0
  let mut tot := 0
  for m in ms do
    let s := MtoS m 0
    --dbg_trace "\n**New" -- {s}"
    let mut h : HashSet state := {s}
    let mut con := 0
    let mut found := false
    while !found do
      con := con + 1
      h := step h
      for a in h do
        --dbg_trace a
        found := found || a.ls == a.ons
        if found then
          tot := tot + a.con
          dbg_trace "found at {a.con}" -- {a}"
          --dbg_trace "\n"
          break
  dbg_trace "total: {tot}"

--  match s.splitOn " " with
--  | [l, b, j] => default
--  | _ => default

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry
-- 389 too low
--#assert part1 atest == ???

--set_option trace.profiler true in solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day10
