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

def drawSC (sc : SC) : IO Unit :=
  let _ : ToString pos := ⟨(match · with | (0, 1) => ">" | (1, 0) => "v" | _ => ".")⟩
  draw <| drawHash sc.cc sc.h sc.w

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
    else
      newCC := newCC.insert mv m
  return {sc with cc := newCC}

def step (sc : SC) : SC :=
  move (move sc (0, 1)) (1, 0)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let mut sc := inputToSC dat
  let mut con := 0
  let mut old := ∅
  let mut new := sc.cc.toArray.qsort (·.1 < ·.1)
  while old != new do
    sc := step sc
    con := con + 1
    old := new
    new := sc.cc.toArray.qsort (·.1 < ·.1)
  return con

#assert part1 atest == 58

--set_option trace.profiler true in solve 1 384  -- takes approx 160s

#eval do
  let dat := atest
  let sc := inputToSC dat
  drawSC sc
  drawSC (move sc (0, 1))
  drawSC (move (move sc (0, 1)) (1, 0))
  let sc := (Array.range 58).foldl (init := sc) (fun _ => step ·)
  drawSC sc

set_option trace.profiler true in
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let mut sc := inputToSC dat
  let mut old := ∅
  let mut con := 0
  while true do
    sc := step sc
    con := con + 1
    let sort := sc.cc.toArray.qsort (·.1 < ·.1)
    if old == sort then
      IO.println con
      break
    old := sort
  drawSC sc

end Day25
