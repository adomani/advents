import Advents.Utils
open Std

namespace AoC2025_Day09

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day09" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"

/-
........3←2
........↓.↑
.5←←←←←←4.↑
.↓........↑
.6→→→→7...↑
......↓...↑
......0→→→1

......#←←←#
......↓...↑
.#←←←←#...↑
.↓........↑
.#→→→→→→@.↑
........↓.↑
........#→#
-/


/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def inputToPos (dat : Array String) : HashSet pos :=
  dat.foldl (init := ∅) fun tot s => match s.getNats with | [x, y] => tot.insert (x, y) | _ => tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let gr := inputToPos dat
  let mut maxArea := 0
  let mut left := gr
  for A@(a, b) in gr do
    left := left.erase A
    for (c, d) in left do
      let newArea := ((a - c).natAbs + 1) * ((b - d).natAbs + 1)
      maxArea := max maxArea newArea
  return maxArea


#assert part1 atest == 50

set_option trace.profiler true in solve 1 4767418746

/-!
#  Question 2
-/

def inputToArray (dat : Array String) : Array pos :=
  dat.foldl (init := #[]) fun tot s => match s.getNats with | [x, y] => tot.push (x, y) | _ => tot

-- Check that the input is a snake
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let gr := inputToArray dat
  let switch := if dat.size ≤ 100 then 0 else 1
  for i in [:gr.size] do
    let (a, b) := gr[i]!
    let (c, d) := gr[i + 1]?.getD gr[0]!
    if i % 2 == switch && b != d then dbg_trace "Consecutive second coordinates do not match!"
    if i % 2 != switch && a != c then dbg_trace "Consecutive first coordinates do not match!"
  let (xs, ys) := gr.unzip
  if (HashSet.ofArray xs).size * 2 != dat.size then dbg_trace "Coincidence among first coordinates!"
  if (HashSet.ofArray ys).size * 2 != dat.size then dbg_trace "Coincidence among second coordinates!"

/-- Counterclockwise rotation by `π / 4`. -/
def rot (v : pos) : pos := (- v.2, v.1)
/-- Clockwise rotation by `π / 4`. -/
def rot' (v : pos) : pos := - rot v

#guard rot (1, 0) == (0, 1)
#guard rot (0, 1) == (- 1, 0)

def sign (v : pos) : pos :=
  (Int.sign v.1, Int.sign v.2)

/-
........3←2
........↓.↑
.5←←←←←←4.↑
.↓........↑
.6→→→→7...↑
......↓...↑
......0→→→1
-/

/--
Assumes that the edge `v0→v1` is either horizontal or vertical.
Checks that the coordinate of `v` is inside of the range determined
by the non-equal coordinate of `v0` and `v1`.
-/
def inRange (a b v : pos) : Bool :=
  dbg_trace "inRange {a} {b} {v}"
  if a.1 == b.1
  then
    dbg_trace "{v} fst coord eq {v.2} ∈ [{min a.2 b.2}, {max a.2 b.2}]"
    let res := min a.2 b.2 < v.2 && v.2 < max a.2 b.2
    dbg_trace "{res}\n"
    res
  else
    dbg_trace "{v} snd coord eq {v.1} ∈ [{min a.1 b.1}, {max a.1 b.1}]"
    let res := min a.1 b.1 < v.1 && v.1 < max a.1 b.1
    dbg_trace "{res}\n"
    res

/--
Assumes that the edge `v0→v1` is part of a counterclockwise oriented loop that is either horizontal or vertical.
Checks whether the vertex `v` is on the positive side of the edge, that is, it is in
the half-space that starts on the line `v0→v1` and contains all the points
obtained from a point on the line by adding a positive multiple of a clock-wise
rotation of `v0→v1`.
-/
def isPos (v0 v1 v : pos) : Bool :=
  if v0.1 == v1.1 then
    let sgn := (rot' (v1 - v0)).1
    0 ≤ sgn * (v.1 - v0.1)
  else
    let sgn := (rot' (v1 - v0)).2
    0 ≤ sgn * (v.2 - v0.2)

#guard isPos (0, 0) (0, 1) (1, 1)
#guard isPos (0, 0) (0, 1) (2, 1)
#guard !isPos (0, 0) (0, 1) (-1, 1)
#guard !isPos (0, 0) (0, 1) (-2, 1)
#guard !isPos (0, 0) (1, 0) (1, 1)

#guard !isPos (0, 0) (0, - 1) (1, 1)
#guard isPos (0, 0) (- 1, 0) (1, 1)


def cond (v0 v1 v : pos) : Bool :=
  let l := ! inRange v0 v1 v
  let r := isPos v0 v1 v
  dbg_trace "(rg, pos) = {(l, r)}"
  l || r

def condSquare (vs : Array pos) (v w : pos) : Bool :=
  (Array.range vs.size).all fun i =>
  let v0 := vs[i]!
  let v1 := vs[i + 1]!
  cond v0 v1 v &&
    cond v0 v1 w &&
    cond v0 v1 (v.1, w.2) &&
    cond v0 v1 (w.1, v.2)

/--
Assuming that `ab` is vertical, `vw` is horizontal,
returns `true` if the segment  `ab` crosses the segment `vw` internally.
-/
def vertHor (a b v w : pos) : Bool :=
    -- the common `y`-coordinate of `vw` is strictly between the `y`-coordinates of `ab`.
    min a.2 b.2 < v.2 && v.2 < max a.2 b.2 &&
    -- and similarly with the roles reversed.
    min v.1 w.1 < a.1 && a.1 < max v.1 v.1

def crosses (a b v w : pos) : Bool :=
  -- `ab` is vertical, `vw` is horizontal
  if a.1 = b.1 && v.2 == w.2 then
    vertHor a b v w
  else
  -- `ab` is horizontal, `vw` is vertical
  if a.2 = b.2 && v.1 == w.1 then
    vertHor v w a b
  else
    false

#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let gr := inputToArray dat
  let mut gr' := gr
  for v in gr do
    gr' := gr'.drop 1
    for w in gr' do
      dbg_trace "{v} {w} {condSquare gr v w}\n"
  let (xs, ys) := gr.unzip
  let mx := xs.foldl min (xs[0]!)
  let withMin := gr.filter (Prod.fst · == mx)
  let ccw : Bool := withMin[0]!.2 > withMin[1]!.2
  let inrot := if ccw then rot else rot'
  let mut possibles := #[]
  let mut past := gr.back!
  for i in [:gr.size] do
    let curr := gr[i]!
    let pastArrow := curr - past
    dbg_trace "{i}\nold {sign pastArrow}\nnew {inrot (sign pastArrow)}\n"
    possibles := possibles.push curr
    past := curr
  --gr.filter
  dbg_trace (withMin, ccw)

#exit
  --let (mx, Mx) : Option Int × Option Int := gr.fold (init := (none, none)) fun (mx, Mx) ((a, _) : pos) =>
  --  match mx, Mx with
  --  | some m, some M => (min a m, max a M)
  --  | none, some M => (a, max a M)
  --  | some m, none => (min a m, a)
  --  | none, none => (a, a)

  --let (my, My) : Option Int × Option Int := gr.fold (init := (none, none)) fun (my, My) ((_, b) : pos) =>
  --  match my, My with
  --  | some m, some M => (min b m, max b M)
  --  | none, some M => (b, max b M)
  --  | some m, none => (min b m, b)
  --  | none, none => (b, b)

  --let (mx, MY) : Option Int × Option Int := gr.fold (init := (none, none)) fun (mx, MY) ((a, b) : pos) =>
  --  match mx, MY with
  --  | some m, some M => (min a m, max b M)
  --  | none, some M => (a, max b M)
  --  | some m, none => (min a m, b)
  --  | none, none => (a, b)
  --let (mx, Mx) := (mx.getD 0, Mx.getD 0)
  --let (my, My) := (my.getD 0, My.getD 0)
  --let shifted : HashSet pos := gr.fold (init := ∅) fun tot (nx, ny) => tot.insert ((ny, nx) - (my, mx) + (1, 1))
  --dbg_trace (shifted.size, [mx, Mx, my, My])
  --dbg_trace shifted.toArray
  let mut areas : HashSet Nat := ∅
  let mut maxArea := 0
  let mut left := gr
  let switch := if dat.size ≤ 100 then 0 else 1
  for i in [:gr.size] do
    let (a, b) := gr[i]!
    let (c, d) := gr[i + 1]?.getD gr[0]!
    if i % 2 == switch && b != d then dbg_trace "Fail!"
    if i % 2 != switch && a != c then dbg_trace "Fail!"
    --if i % 2 != switch && a != c then dbg_trace "Fail!"
    for (c, d) in left do
      let newArea := ((a - c).natAbs + 1) * ((b - d).natAbs + 1)
      maxArea := max maxArea newArea
      areas := areas.insert newArea

  for A@(a, b) in gr do
    left := left.erase A
    for (c, d) in left do
      let newArea := ((a - c).natAbs + 1) * ((b - d).natAbs + 1)
      maxArea := max maxArea newArea
      areas := areas.insert newArea
  dbg_trace maxArea
  --dbg_trace (maxArea, areas.toArray.qsort)
  --draw <| drawSparse shifted ((My - my + 1).toNat + 2) ((Mx - mx + 1).toNat + 2)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end AoC2025_Day09
