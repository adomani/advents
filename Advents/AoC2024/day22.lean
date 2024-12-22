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

def s1 (s : Nat) := prune <| mix (s * 64) s
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

def price (s : Nat) := step s % 10

def window (a : Array Int) (n : Int) : Array Int :=
  if a.size < 4 then a.push n else a.reverse.pop.reverse.push n

def changes (s : Nat) (it : Nat := 2000) (seq : Array Int := #[-2,1,-1,3]) : Array Int := Id.run do
  let mut prevSecret := s
  let mut tot : Array Int := #[]
  let mut w : Array Int := #[]
  for i in [1:it] do
    let nextSecret := step prevSecret
    let nextPrice : Int := nextSecret % 10
    let change : Int := nextPrice - prevSecret % 10
    w := window w change
    --dbg_trace w
    tot := tot.push <| change
    prevSecret := nextSecret

    if w == seq then
      dbg_trace s!"Found at {i} with price {nextPrice}"
      return #[nextPrice]
  return tot


#eval do
  let t ← IO.FS.readFile input
  let t := test0
  let t := "1,2,3,2024"
  let dat := t.getNats
  let mut tot := 0
  for d in dat do
    let ch := changes d
    match ch with
      | #[p] => tot := tot + p
      | _ => IO.println "Not found"
  IO.println tot

  --IO.println <| changes 123

#eval Nat.factors 16777216
#assert 2 ^ 24 == 16777216

#eval do
  let t ← IO.FS.readFile input
  let t := test0
  let dat := t.getNats
  IO.println <| s!"{123}: {Nat.toDigits 2 (iter 123 2000)}"

  for d in dat do
    IO.println <| s!"{d}: {Nat.toDigits 2 d}"
  let vals := dat.foldl (init := #[]) fun (m : Array Nat) d => m.push (iter d 2000)
  IO.println vals.sum

---def storeWindows (s : Array Int) : Std.HashSet (Array Int) :=
---  s.foldl (init := ∅) fun h

def storeWindows (h : Std.HashMap (Array Int) Int) (s : Nat) (it : Nat := 2000) :
    Std.HashMap (Array Int) Int := Id.run do
  let mut prevSecret := s
  let mut alreadyFound : Std.HashSet (Array Int) := ∅
  let mut tot := h
  let mut w : Array Int := #[]
  for i in [1:it+1] do
    let nextSecret := step prevSecret
    let nextPrice : Int := nextSecret % 10
    let change : Int := nextPrice - prevSecret % 10
    w := window w change
    if w.size == 4 then
      if ! alreadyFound.contains w then
        tot := tot.alter w (some <| ·.getD 0 + nextPrice)
        alreadyFound := alreadyFound.insert w
    --dbg_trace w
    prevSecret := nextSecret
  return tot

-- store the windows
set_option trace.profiler true in
#eval do
  let dat := #[1, 2, 3, 2024]
  let dat := (← IO.FS.readFile input).getNats.toArray
  let mut tallies : Std.HashMap (Array Int) Int := ∅
  for d in dat do
    tallies := storeWindows tallies d
  let mut (window, max) := (#[], 0)
  for r@(w, t) in tallies do
    if max < t then
      IO.println s!"{w}: {t}"
      (window, max) := r

/-!
-/
#eval do
  let mut n := 1
  for i in [0:20] do
    IO.println <| Nat.toDigits 2 <| 2 ^ 24 + n
    n := step <| step <| step <| step n

-- 1712 -- too low
-- 1717 -- 0:it -- correct!
-- 1717 -- 1:it+1 -- correct!
/-
#[-3, -1, -3, 6]: 755
#[-3, 3, 0, 0]: 1315
#[2, 0, 0, 3]: 1396
#[3, 1, -1, 1]: 1474
#[2, -2, 3, 1]: 1595
#[0, -1, -3, 4]: 1605
#[-1, 1, -1, 1]: 1676
#[-1, 0, 0, 2]: 1712
-/
#eval show Elab.Term.TermElabM _ from do
  let t ← IO.FS.readFile input
  let t := test0
  let dat := t.getNats
  for d in dat do
    IO.println <| s!"{d}: {Nat.toDigits 2 d}"
  let vals := dat.foldl (init := #[]) fun m d => m.push (iter d 2000)
  IO.println vals.sum

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day22
