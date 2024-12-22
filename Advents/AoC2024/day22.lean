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

/-- `test2` is the test string for the problem. -/
def test2 := "1
2
3
2024"

/-- `atest2` is the test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/-- One of the ingredients to generate a secret: could be inlined. -/
def mix (s n : Nat) := s.xor n

-- Testing the `mix` function.
#assert (42 |>.xor 15) == 37

/-- One of the ingredients to generate a secret: could be inlined. -/
def prune (s : Nat) := s % 16777216

-- Testing the `prune` function.
#assert prune 100000000 == 16113920

/-- The first part of generating a new secret. -/
def s1 (s : Nat) := prune <| s.xor (s * 64)

/-- The second part of generating a new secret. -/
def s2 (s : Nat) := prune <| s.xor (s / 32)

/-- The third part of generating a new secret. -/
def s3 (s : Nat) := prune <| s.xor (s * 2048)

/-- `step s` generates the new secret from an old secret `s`. -/
def step (s : Nat) := s3 <| s2 <| s1 s

-- Testing the `step` function.
#eval do
  let t := test0
  let mut init := 123
  let dat := t.getNats
  for d in dat do
    init := step init
    if init != d then IO.println s!"Expected {d}, found {init}!"

/-- `iter s n` returns the `n`-th iterated secret, generated starting from `s`. -/
def iter (s : Nat) : Nat → Nat
  | 0 => s
  | n + 1 => iter (step s) n

-- Testing the `iter` function.
#eval show Elab.Term.TermElabM _ from do
  let t := test
  let dat := t.getNats
  let msg := dat.foldl (init := #[]) fun m d => m.push s!"{d}: {(iter d 2000)}"
  guard <| test1 == "\n".intercalate msg.toList

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let dat := dat.getNats
  let vals := dat.foldl (init := #[]) fun m d => m.push (iter d 2000)
  vals.sum

#assert part1 test == 37327623

set_option trace.profiler true in solve 1 14869099597 file

/-!
#  Question 2
-/

/-- Updates the window containing the 4 latest price differences. -/
def window (a : Array Int) (n : Int) : Array Int :=
  (if a.size < 4 then a else a.reverse.pop.reverse).push n

/--
`storeWindows` keeps a tally of how many bananas you would get with each possible `window`,
while finding the `it` secrets generated from the seed `s`.
-/
def storeWindows (h : Std.HashMap (Array Int) Int) (s : Nat) (it : Nat := 2000) :
    Std.HashMap (Array Int) Int := Id.run do
  let mut prevSecret := s
  let mut alreadyFound : Std.HashSet (Array Int) := ∅
  let mut tot := h
  let mut w : Array Int := #[]
  for _ in [0:it] do
    let nextSecret := step prevSecret
    let nextPrice : Int := nextSecret % 10
    let change : Int := nextPrice - prevSecret % 10
    w := window w change
    if w.size == 4 then
      if ! alreadyFound.contains w then
        tot := tot.alter w (some <| ·.getD 0 + nextPrice)
        alreadyFound := alreadyFound.insert w
    prevSecret := nextSecret
  return tot

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := Id.run do
  let dat := dat.getNats.toArray
  let mut tallies : Std.HashMap (Array Int) Int := ∅
  for d in dat do
    tallies := storeWindows tallies d
  let mut (window, max) := (#[], 0)
  for r@(_, t) in tallies do
    if max < t then
      (window, max) := r
  return max.natAbs

#assert part2 test2 == 23

--set_option trace.profiler true in solve 2 1717 file  -- takes approximately 3 minutes

end Day22
