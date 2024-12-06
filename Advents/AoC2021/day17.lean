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
  let p := p + v
  let v := (v.1 - v.1.sign, v.2 - 1)
  (p, v)

def within (a : pos × pos) (p : pos) : Bool :=
  a.1.1 ≤ p.1 && p.1 ≤ a.1.2 &&
  a.2.1 ≤ p.2 && p.2 ≤ a.2.2

def reaches (dat : pos × pos) (a : pos) : Bool := Id.run do
  let mut (s, v) : pos × pos := ((0, 0), a)
  --let mut con := 0
  while dat.2.1 ≤ s.2 do
    --con := con + 1
    (s, v) := step s v
    --dbg_trace "{(s, v)}"
    if within dat s then
      return true
  return false

def mkRange (a : pos × pos) : pos × pos :=
  let ((mx, Mx), my, _My) := a
  ((mx.natAbs.sqrt - 1, Mx), (my, - my))

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

--solve 1 11781 file

/-!
#  Question 2
-/

def possibleX (rg : pos) : Std.HashSet (Int × Int) := Id.run do
  let mut h := {}
  for i in [0:rg.2.natAbs+1] do
    let mut sum := 0
    for k in [0:i+1] do
      sum := sum + (i - k)
      if rg.1 ≤ sum && sum ≤ rg.2 then h := h.insert (i, k)
  return h

def possibleY (rg : pos) : Std.HashSet (Int) :=
  Std.HashSet.ofArray ((Array.range (rg.1.natAbs + 1)).map Nat.cast)
    |>.union <| .ofArray ((Array.range (rg.1.natAbs + 1)).map (- Nat.cast ·))


#eval do IO.println <| ← IO.FS.readFile input

def lex (a b : pos) : Bool := a.1 < b.1 || (a.1 == b.1 && a.2 < b.2)

#eval do
  let inp := test
  let inp ← IO.FS.readFile input
  let dat := inputToPos inp
  --IO.println s!"\ndata: {dat}\n\n{reaches dat (6, -1)}"
--#exit
  IO.println s!"data: {dat}\n"
  let posX := possibleX (dat.1.1, dat.1.2)
  let posY := possibleY (dat.2.1, dat.2.2)
  --IO.println s!"xs: {posX.toArray.qsort (·.1 < ·.1)}\n"
  --IO.println s!"ys: {posY.toArray.qsort (· < ·)}\n"
  let mut (incl, disc) : Std.HashSet pos × Std.HashSet pos := default
  for (x, n) in posX do
    for y in posY do
      --let val := if 0 ≤ y then 2 * y * n - (((n - 2 * y) + 1) * (n - 2 * y)) / 2 else y - ((n + 1) * n) / 2
      if reaches dat (x, y) then --dat.2.1 ≤ val && val ≤ dat.2.2 then
        incl := incl.insert (x, y)
      else
        disc := disc.insert (x, y)
  IO.println s!"Include: {incl.size}\n"
  IO.println s!"Discard: {disc.size}\n"
  --IO.println s!"Include: {incl.size}\n{incl.toArray.qsort lex}\n"
  --IO.println s!"Discard: {disc.size}\n{disc.toArray.qsort lex}"


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
