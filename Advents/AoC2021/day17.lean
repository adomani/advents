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

/--
Convert the input string into the `pos`, each representing an interval:
* the first for the `x` coordinate,
* the second for the `y` coordinate.
-/
def inputToPos (s : String) : pos × pos :=
  match s.getInts with
    | [x1, x2, y1, y2] => ((x1, x2), (y1, y2))
    | _ => panic! "Improperly parsed input!"

/-- Converts a position and velocity to what they become after each step. -/
def step (p v : pos) : pos × pos :=
  (p + v, (v.1 - v.1.sign, v.2 - 1))

/-- Checks that the input position `p` is inside the rectangular range specified by `a`. -/
def within (a : pos × pos) (p : pos) : Bool :=
  a.1.1 ≤ p.1 && p.1 ≤ a.1.2 &&
  a.2.1 ≤ p.2 && p.2 ≤ a.2.2

/--
`reaches dat a` returns `true` if and only if starting from `(0, 0)` and aiming in
direction `a`, the probe reaches the rectangular range `dat`.
-/
def reaches (dat : pos × pos) (a : pos) : Bool := Id.run do
  let mut (s, v) : pos × pos := ((0, 0), a)
  while dat.2.1 ≤ s.2 do
    (s, v) := step s v
    if within dat s then
      return true
  return false

/-- Constructs the range of the possible velocities for part 1. -/
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

solve 1 11781 file

/-!
#  Question 2
-/

/-- The possible first components of a velocity that reaches the `x`-range `rg`. -/
def possibleX (rg : pos) : Std.HashSet Int := Id.run do
  let mut h := {}
  for i in [0:rg.2.natAbs+1] do
    let mut sum := 0
    for k in [0:i+1] do
      sum := sum + (i - k)
      if rg.1 ≤ sum && sum ≤ rg.2 then h := h.insert i
  return h

/-- A crude bound on the second component of a velocity that reaches the `y`-range `rg`. -/
def possibleY (rg : pos) : Std.HashSet (Int) :=
  Std.HashSet.ofArray ((Array.range (rg.1.natAbs + 1)).map Nat.cast)
    |>.union <| .ofArray ((Array.range (rg.1.natAbs + 1)).map (- Nat.cast ·))

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := Id.run do
  let dat := inputToPos dat
  let posX := possibleX (dat.1.1, dat.1.2)
  let posY := possibleY (dat.2.1, dat.2.2)
  let mut (incl, disc) : Std.HashSet pos × Std.HashSet pos := default
  for x in posX do
    for y in posY do
      if reaches dat (x, y) then
        incl := incl.insert (x, y)
      else
        disc := disc.insert (x, y)
  incl.size

#assert part2 test == 112

solve 2 4531 file

end Day17
