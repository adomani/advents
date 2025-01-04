import Advents.Utils
open Std

namespace Day25

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day25.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure SC where
  grid : HashSet pos
  cc : HashMap pos pos
  w : Nat
  h : Nat

def inputToSC (dat : Array String) : SC where
  grid := sparseGrid dat fun _ => true
  cc := sparseMap dat fun c => match c with | '>' => some (0, 1) | 'v' => some (1, 0) | _ => none
  w := dat[0]!.length
  h := dat.size

instance : ToString pos where toString | (0, 1) => ">"  | (1, 0) => "v" | _ => "."

def drawSC (sc : SC) : IO Unit := draw <| drawHash sc.cc sc.h sc.w

#eval do
  let dat := atest
  let sc := inputToSC dat
  drawSC sc

def add (sc : SC) (p q : pos) : pos :=
  let cand := p + q
  if sc.cc.contains cand then cand else
  (cand.1 % sc.h, cand.2 % sc.w)

def move (sc : SC) (d : pos) : SC := Id.run do
  let mut (toMove, newCC) := sc.cc.partition fun _ p => p == d
  for (p, m) in toMove do
    let mv := add sc p m
    if sc.cc.contains mv then
      newCC := newCC.insert p m
      --toMove := toMove.erase p
    else
      newCC := newCC.insert mv m
  return {sc with cc := newCC}

def flip (p : pos) : pos := (p.2, p.1)

def moveN (sc : SC) (n : Nat) (d : pos := (0, 1)) : SC :=
  match n with
    | 0 => sc
    | n + 1 => moveN (move sc d) n (flip d)

def step (sc : SC) (n : Nat) : SC :=
  match n with
    | 0 => sc
    | n + 1 => step (moveN sc 2) n

#eval do
  let dat := atest
  let sc := inputToSC dat
  drawSC sc
  drawSC (move sc (0, 1))
  drawSC (move (move sc (0, 1)) (1, 0))
  --let sc := moveN sc 158 --(0, 1)
  let sc := step sc 58
  drawSC sc

set_option trace.profiler true in
#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut sc := inputToSC dat
  let mut con := 0
  while true do
    let scn := step sc 1
    con := con + 1
    if scn.cc.toArray.qsort (·.1 < ·.1) == sc.cc.toArray.qsort (·.1 < ·.1) then
      IO.println con
      break
    sc := scn
  drawSC sc


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

end Day25
