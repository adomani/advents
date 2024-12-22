import Advents.Utils
open Lean

namespace Day22

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day22.input"

/-!
#  Question 1
-/

/-- `test0` is the test string for the problem. -/
def test0 := "15887950
16495136
527345
704524
1553684
12683156
11100544
12249484
7753432
5908254"

/-- `atest0` is the test string for the problem, split into rows. -/
def atest0 := (test0.splitOn "\n").toArray

/-- `test` is the test string for the problem. -/
def test := "1
10
100
2024"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `test1` is the test string for the problem. -/
def test1 := "1: 8685429
10: 4700978
100: 15273692
2024: 8667524"

/-- `atest1` is the test string for the problem, split into rows. -/
def atest1 := (test1.splitOn "\n").toArray

def mix (s n : Nat) := s.xor n

#assert (42 |>.xor 15) == 37

def prune (s : Nat) := s % 16777216

#assert prune 100000000 == 16113920

def s1 (s : Nat) := prune <| mix (64 * s) s
def s2 (s : Nat) := prune <| mix (s / 32) s
def s3 (s : Nat) := prune <| mix (s * 2048) s

def step (s : Nat) := s3 <| s2 <| s1 s

#eval do
  let t := test0
  let mut init := 123
  let dat := t.getNats
  for d in dat do
    init := step init
    if init != d then IO.println s!"Expected {d}, found {init}!"
  --s3 <| s2 <| s1 t

def iter (s : Nat) : Nat → Nat
  | 0 => s
  | n + 1 => iter (step s) n

#eval show Elab.Term.TermElabM _ from do
  let t := test
  let dat := t.getNats
  let mut msg := #[]
  for d in dat do
    msg := msg.push s!"{d}: {(iter d 2000)}"
  guard <| test1 == "\n".intercalate msg.toList

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

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

end Day22
