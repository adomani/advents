import Advents.Utils
open Lean

namespace Day17

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day17.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "target area: x=20..30, y=-10..-5"

def inputToPos (s : String) : pos × pos :=
  match s.getInts with
    | [x1, x2, y1, y2] => ((x1, x2), (y1, y2))
    | _ => panic! "Improperly parsed input!"

def step (p v : pos) : pos × pos :=
  let p := (p.1 + v.1, p.2)
  let p := (p.1, p.2 + v.2)
  let v := (v.1 - v.1.sign, v.2)
  let v := (v.1, v.2 - 1)
  (p, v)

def within (a : pos × pos) (p : pos) : Bool :=
  a.1.1 ≤ p.1 && p.1 ≤ a.1.2 &&
  a.2.1 ≤ p.2 && p.2 ≤ a.2.2

def mkRange (a : pos × pos) : pos × pos :=
  let ((mx, Mx), my, My) := a
  ((mx.natAbs.sqrt - 1, Mx), (my, - (My + 2 * My/3)))

def reaches (dat : pos × pos) (a : pos) : Bool := Id.run do
  let mut (s, v) : pos × pos := ((0, 0), a)
  let mut con := 0
  while dat.2.2 ≤ s.2 do
    con := con + 1
    (s, v) := step s v
    if within dat s then
      return true
  return false

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let rg := inputToPos dat
  let ((mx, Mx), (my, My)) := mkRange rg
  for y1 in [0:(My - my).natAbs] do
    let y := My - y1
    for x1 in [0:(Mx - mx).natAbs] do
      let x := x1 + mx
      let t := (x, y)
      if reaches rg t then
        return (y.natAbs + 1).binom 2
      else continue
  return 0

#assert part1 test == 45

solve 1 11781 file

/-!
#  Question 2
-/

def possible (rg : pos) : Std.HashSet (Int × Int) := Id.run do
  let mut h := {}
  for i in [0:rg.2.natAbs] do
    let mut sum := 0
    for k in [0:i] do
      sum := sum + (i - k)
      if rg.1 ≤ sum && sum ≤ rg.2 then h := h.insert (i, k)
  return h

#eval do
  let dat := inputToPos test
  IO.println s!"data: {dat}\n"
  IO.println s!"xs: {(possible (dat.1.1, dat.1.2)).toArray.qsort (·.1 < ·.1)}"
  IO.println s!"ys: {(possible (dat.2.1, dat.2.2)).toArray.qsort (·.1 < ·.1)}"

def visits (p : pos) : Array pos :=
  default

#eval do
  let dat := test
  let rg@(rx, ry) := inputToPos dat
  let (s, v) : pos × pos := ((0, 0), (7, 2))
  let tsts := #[(7, 2), (6, 3), (9, 0), (17, -4)]
  for t in tsts do
    if reaches rg t then
      IO.println <| t

#eval do
  let dat := test
  let dat ← IO.FS.readFile input
  let rg@(rx, ry) := inputToPos dat
  let ((mx, Mx), (my, My)) := mkRange rg
  --let mut may := 0
  for y1 in [0:(My - my).natAbs] do
    let y := My - y1
    for x1 in [0:(Mx - mx).natAbs] do
      let x := x1 + mx
      let t := (x, y)
      if reaches rg t then
        dbg_trace (y.natAbs + 1).binom 2
        return
        --IO.println <| t
  --dbg_trace (may.natAbs + 1).binom 2




/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day17
