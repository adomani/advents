import Advents.Utils
open Std

namespace Day25

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2021"/"day25" : FilePath).withExtension "input"

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

/--
`SeaCucumbers` encodes the sea cucumbers.
* `cc` is the `HashMap` assigning to each position the direction of the corresponding sea cucumber:
  the direction is either `(0, 1)` for the right moving (`>`) or `(1, 0)` for the down moving (`v`)
  herds.
* `w` is the width of the sea floor.
* `h` is the height of the sea floor.
-/
structure SeaCucumbers where
  cc : HashMap pos pos
  w : Nat
  h : Nat

/-- Converts the input data into `SeaCucumbers`. -/
def inputToSC (dat : Array String) : SeaCucumbers where
  cc := sparseMap dat fun c => match c with | '>' => some (0, 1) | 'v' => some (1, 0) | _ => none
  w := dat[0]!.length
  h := dat.size

/-- A utility function to draw `SeaCucumbers`. -/
def drawSC (sc : SeaCucumbers) : IO Unit :=
  let _ : ToString pos := ⟨(match · with | (0, 1) => ">" | (1, 0) => "v" | _ => ".")⟩
  draw <| drawHash sc.cc sc.h sc.w

/--
info: --0123456789-
0|v   >> vv>|
1| vv>> vv  |
2|>> >v>   v|
3|>>v>> > v |
4|v>v vv v  |
5|> >>  v   |
6| vv  > >v |
7|v v  >>v v|
8|    v  v >|
--0123456789-
-/
#guard_msgs in
#eval drawSC (inputToSC atest)

/-- `addMod` adds the positions `p q` taking care of the periodic nature of the sea floow. -/
def addMod (sc : SeaCucumbers) (p q : pos) : pos :=
  let cand := p + q
  if sc.cc.contains cand then cand else
  (cand.1 % sc.h, cand.2 % sc.w)

/-- Moves all the sea cucumbers facing the direction `d`. -/
def move (sc : SeaCucumbers) (d : pos) : SeaCucumbers := Id.run do
  let mut (toMove, newCC) := sc.cc.partition fun _ p => p == d
  for (p, m) in toMove do
    let mv := addMod sc p m
    if sc.cc.contains mv then
      newCC := newCC.insert p m
    else
      newCC := newCC.insert mv m
  return {sc with cc := newCC}

/-- Performs a `move` of the sea cucumbers facing right and then of the ones facing down. -/
def step (sc : SeaCucumbers) : SeaCucumbers :=
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

end Day25
