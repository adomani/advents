import Advents.Utils
--open Lean

/-- `String.getInts l` takes as input a string `l`, removes everything that is neither a digit,
not a minus sign (`-`) and interprets the rest as a list of integers. -/
partial
def String.getInts (l : String) : List Int :=
  let cond : Char → Bool := fun c => (Char.isDigit c) || (c == '-')
  let l1 := l.dropWhile (!cond ·)
  if l1.length == 0 then [] else
    let d1 := String.toInt! (l1.takeWhile cond)
    let fin := getInts (l1.dropWhile cond)
  d1 :: fin

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day24.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `vol` is an abbreviation for a triple of integers, representing a point in
3-dimensional space. -/
abbrev vol := Int × Int × Int

/-- converts an input line into a pair of `(x, y, z)`-coordinates
representing the initial position and velocity. -/
def getpvOne (s : String) : vol × vol :=
  match s.getInts with
    | [a, b, c, d, e, f] => ((a, b, c), (d, e, f))
    | _ => dbg_trace "oh no"; default

/-- returns the `(x, y, z)`-coordinates of the input data `dat`. -/
def getpv (dat : Array String) : Array (vol × vol) := dat.map getpvOne

/-- returns the `(x, y)`-coordinates of the input data `dat`. -/
def getpvXY (dat : Array String) : Array (pos × pos) :=
  (getpv dat).map fun ((a, b, _), (d, e, _)) => ((a, b), (d, e))

/-- A simple implementation of the determinant of a matrix, using Laplace expansion
on the last row. -/
partial
def det {α} [BEq α] [Inhabited α] [Add α] [Mul α] [HMul Int α α] (m : Array (Array α)) : α :=
  let ms := m.size
  if ! (m.map Array.size).all (· == ms) then dbg_trace "wrong sizes"; default else
  if ms = 1 then m[0]![0]! else
  Id.run do
  let mut deti : α := default
  for a in [:ms] do
    if ! m.back[a]! == default then
      deti := deti + ((-1) ^ a : Int) * m.back[a]! * det (m.pop.map (Array.eraseIdx · a))
  ((-1) ^ (ms + 1) : Int) * deti
set_option profiler true
#assert det #[#[1, 0], #[0, 1]] == 1
#assert det #[#[1, 0, 0], #[0, 1, 0], #[0, 0, 1]] == 1

/-- finds the coefficients of the equation defining the line through `p` with
velocity `v`. -/
def getLine (p v : pos) : Array Int :=
  let ((px, py), (vx, vy)) := (p, v)
  #[vy, - vx, det #[#[px, py], #[vx, vy]]]

/-- given the coefficients of the equations of two lines, find the intersection point
in the form `#[x, y, z]` such that
* either `z ≠ 0` and `(x / z, y / z)` lies in the intersection;
* or `z = 0` and the lines are parallel.
-/
def solve2 {α} [BEq α] [Inhabited α] [Add α] [Mul α] [HMul Int α α] (x y : Array α) : Array α :=
  Id.run do
  let mut ans := #[]
  for i in [:x.size] do
    ans := ans.push <| ((- 1) ^ i.succ : Int) *  det #[Array.eraseIdx x i, Array.eraseIdx y i]
  ans

/-- `inter x y` takes as input two arrays `x, y` of integers, assuming that they are
the coefficients `#[a, b, c]` and `#[d, e, f]` of linear equations

`a * x + b * y = c`
`d * x + e * y = f`

It returns the array of coordinates of the intersection point of the two lines,
when they are not parallel, and `#[0, 0, 0]` otherwise. -/
def inter (x y : Array Int) : Array Rat :=
  match solve2 x y with
    | #[a, b, p] => #[a / -p, b / -p]
    | _ => dbg_trace "inter oh no"; default

/-- `findt pv x` takes as input
* a pair `pv = (p, v)` representing the initial position `p` and velocity `v`, and
* a rational number `x` representing an `x`-coordinate.

It returns the value of `t` for which a snowflake starting at `p` with velocity `v`
at time `t = 0` goes through a point with `x`-coordinate `x`.
-/
def findt (pv : pos × pos) (x : Rat) : Rat :=
  let ((px, _), (vx, _)) := pv
  (x - px) / vx

/-- `inrange l h p` takes as input
* a lower bound `l` in the rational numbers;
* an upper bound `h` in the rational numbers;
* an array `p` of rational numbers representing the coordinates of a point in the plane.

It returns `true` if the coordinates of point `p` both belong to the interval `[l, h]`. -/
def inrange (l h : Rat) (p : Array Rat) : Bool :=
  (p.map fun x => (l ≤ x ∧ x ≤ h : Bool)).all (·)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) (l : Nat := 200000000000000) (h : Nat := 400000000000000) :
    Nat :=
  let coos := getpvXY dat
  Id.run do
  let mut ints := 0
  for ci in [:coos.size] do
    let c := coos[ci]!
    for di in [ci.succ:coos.size] do
      let d := coos[di]!
      let icd := inter (getLine c.1 c.2) (getLine d.1 d.2)
      let tc := findt c icd[0]!
      let td := findt d icd[0]!
      --`projSol` should represent the situation in which the two lines are parallel
      --probably this check is not needed, since in this case `#[0, 0]` is the "solution"
--      let projSol := solve2 (getLine c.1 c.2) (getLine d.1 d.2)
--      if projSol[2]! = 0 then dbg_trace icd
      if (inrange l h icd) ∧ (0 ≤ tc) ∧ (0 ≤ td) /-∧ (projSol[2]! ≠ 0)-/ then
        ints := ints + 1
  return ints

#assert part1 atest 7 27 == 2

solve 1 23760
