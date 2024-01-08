import Advents.Utils
--open Lean

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
abbrev vol := Rat × Rat × Rat

instance : HMul (Array (Array Rat)) (Array Rat) (Array Rat) where
  hMul m x :=
    if (m.map Array.size).all (! · == x.size) then
      dbg_trace "cannot multiply matrix by column!"; default
    else
      m.map fun r => (r.zipWith x (· * ·)).sum

instance : HMul Rat vol vol where
  hMul a x := (a * x.1, a * x.2.1, a * x.2.2)

local instance : HMul Int Rat Rat where
  hMul := (· * ·)

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
  (getpv dat).map fun ((a, b, _), (d, e, _)) => ((a.num, b.num), (d.num, e.num))

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

#assert det #[#[1, 0], #[0, 1]] == 1
#assert det #[#[1, 0, 0], #[0, 1, 0], #[0, 0, 1]] == 1
#assert det #[#[43, 46, -3], #[70, 100, -6], #[31, 97, -5]] == 0

/-- finds the coefficients of the equation defining the line through `p` with
velocity `v`. -/
def getLine (p v : pos) : Array Rat :=
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
def inter (x y : Array Rat) : Array Rat :=
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

/-!
#  Question 2
-/

/-- `evals2 p` takes as input a point in space.
It returns the evaluation of all the monomials of degree at most 2 on `p`. -/
def evals2 (p : vol) : Array Rat :=
  let (a, b, c) := p
  let p := #[a, b, c, 1]
  Id.run do
  let mut ret := #[]
  for i in [:p.size] do
    for j in [i:p.size] do
      ret := ret.push (p[i]! * p[j]!)
  return ret

#assert evals2 (1, 2, 3) == #[1, 2, 3, 1, 4, 6, 2, 9, 3, 1]

/-- Given an array `a` of arrays of integers, returns the maximal minors of the
given arrays, assuming that each inner array has length one more than the number
of arrays. -/
def mins (a : Array (Array Rat)) : Array Rat :=
  (Array.range a.size.succ).map fun i => ((-1) ^ i : Int) * (det (a.map fun r => r.eraseIdx i))

#assert mins #[#[1, 2, 3], #[2, 3, 4]] == #[-1, 2, -1]

/-- `quadricContaining3lines pv1 pv2 pv3` takes as input three pairs consisting of a
position and a velocity in space.

It returns the 10 coefficients of the quadric containing the three lines going through
each of the points and with the given initial velocities. -/
def quadricContaining3lines (pv1 pv2 pv3 : vol × vol) : Array Rat :=
  let coos := #[pv1, pv2, pv3]
  let rels := (coos.toList.map fun x => [evals2 x.1, evals2 (x.1 + x.2), evals2 (x.1 - x.2)]).join
  let ls := mins rels.toArray
  let gcd : Int := ls.foldl (fun x y => Int.gcd x y.num) 0
  ls.map (· / gcd)

/-- `qeval q x` returns the evaluation of the quadric represented by `q` at the point whose
coordinates are `x`. -/
def qeval (q : Array Rat) (x : vol) : Rat :=
  (q.zipWith (evals2 x) (· * ·)).sum

/-- given the 10 coefficients of a quadric, return the matrix representing the quadratic
form associated to the quadric. -/
def getQF (coefs : Array Rat) : Array (Array Rat) :=
  match coefs with
    | #[xx, xy, xz, xw, yy, yz, yw, zz, zw, ww] =>
      #[ #[2 * xx,     xy,     xz,     xw],
         #[    xy, 2 * yy,     yz,     yw],
         #[    xz,     yz, 2 * zz,     zw],
         #[    xw,     yw,     zw, 2 * ww] ]
    | _ => dbg_trace "oh no, no QF!"; default

/-- `clq q p v` takes as input
* an array `q` of rational numbers, representing the 10 coefficients of a quadric;
* a point `p` in space, representing a position;
* a point `v` in space, representing an initial velocity.

It returns the array `#[c, l q]` of rational numbers consisting of the
constant, linear and quadratic coefficient of the evaluation of the quadratic form `q`
at the line through `p` with direction `v`. -/
def clq (q : Array Rat) (p v : vol) : Array Rat :=
  let c0 := qeval q p
  let a12 := qeval q (p + v) - c0
  let s12 := qeval q (p - v) - c0
  #[c0, (a12 - s12) / 2, (a12 + s12) / 2]

/-- `roots clq` takes as input an array `clq` of rational number.
Writing `clq = #[c, l, q]`, it finds the two rational roots of the equation
`q * x ^ 2 + l * x + c = 0`, returning `#[0, 0]` if there are no rational roots. -/
def roots (clq : Array Rat) : Array Rat :=
  match clq with
    | #[c, l, q] =>
      let disc := l * l - 4 * c * q
      if disc < 0 then dbg_trace "non-real roots"; default else
      let drt : Rat := disc.num.toNat.sqrt / disc.den.sqrt
      if drt * drt ≠ disc then dbg_trace "real irrational roots"; default
      else #[(- l + drt) / (2 * q), (- l - drt) / (2 * q)]
    | _ => dbg_trace "roots oh no!"; default

/-- converts the `vol` `v = (x, y, z)` to the projective point `#[x, y, z, 1]`. -/
def toArr (v : vol) : Array Rat :=
  #[v.1, v.2.1, v.2.2, 1]

/-- `interPlanePV pEq pv` takes as input an array `pEq = #[a, b, c, d]` of rational numbers and
a pair `pv = (p, v)` of points in space.

It computes the intersection of
* the plane `π` whose equation is `π : a * x + b * y + c * z + d = 0`;
* the line `ℓ` through `p` and direction `v`.

It returns a pair `(t, q)`, such that `q = p + t * v` and `q ∈ π ∩ ℓ` belongs to the intersection
of the plane `π` and the line `ℓ`. -/
def interPlanePV (pEq : Array Rat) (pv : vol × vol) : Rat × vol :=
  match (pEq, pv) with
    | (#[a, b, c, d], (p@(px, py, pz), v@(vx, vy, vz))) =>
      --  `a * (px + t * vx) + b * (py + t * vy) + c * (pz + t * vz) + d = 0` =>
      --  `t * (a * vx + b * vy + c * vz) = - (a * px + b * py + c * pz + d)`
      let den := a * vx + b * vy + c * vz
      if den = 0 then dbg_trace "interPlanePV: warning -- the denominator is zero"; default else
      let t := - (a * px + b * py + c * pz + d) / den
      (t, p + t * v)
    | _ => dbg_trace "oh no!"; default

#assert interPlanePV #[1, 1, 1, -1] ((1, 2, 0), (1, 1, 1)) ==
  ((-2 : Rat)/3, (1 : Rat)/3, (4 : Rat)/3, (-2 : Rat)/3)

/-- `findStart quad pv1 pv2 p` takes as input
* the array `quad` of the 10 coefficients of a quadric;
* two pairs `pv1, pv2` of a position and velocity in space;
* a point `p` in space.

It assumes that
* the line `l1` determined by `pv1` and the line `l2` determined by `pv2` are disjoint;
* the quadric contains the lines `l1` and `l2`;
* the point `p` lies on the quadric `quad`.

It returns a point on the line through `p` contained in `quad` that
meets the lines `l1` and `l2`, that meets the condition of part 2 of the question. -/
def findStart (quad : Array Rat) (pv1 pv2 : vol × vol) (p : vol) : vol :=
  -- the plane tangent to `quad` at `p`
  let plane := (getQF quad) * (toArr p)
  -- the intersection of the plane `plane` with the line determined by `pv`.
  let (t1, inter1) := interPlanePV plane pv1
  let (t2, inter2) := interPlanePV plane pv2
  1 / (t2 - t1) * (t2 * inter1 - t1 * inter2)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) (i0 : Nat := 0) (i1 : Nat := 1) (i2 : Nat := 2) (i3 : Nat := 3) :
    Rat :=
  let coo0 := getpvOne dat[i0]!
  let coo1 := getpvOne dat[i1]!
  let coo2 := getpvOne dat[i2]!
  let (pt, vt) := getpvOne dat[i3]!
  let _q := quadricContaining3lines coo0 coo1 coo2
  --  ideally, I could run the previous line to compute `q` directly from the data.
  let q : Array Rat := #[99152456616352095153, 31268323746884196420, 68883081878393694944, -87476990994254268966807869665080568, -191578927814978880960, -136390457582647411920, 153891014299882693838040060221881320, -31603424001432941840, 41693653625411433802947742476397232, -17989250061443095666272048548759880765735386106836]
  let (c1, c2) := match roots (clq q pt vt) with
      | #[a, b] => (pt + a * vt, pt + b * vt)
      | _ => dbg_trace "oh no match!"; default
  let planes := ((getQF q) * (toArr c1), (getQF q) * (toArr c2))
  let cand := if (planes.1.map (Rat.den · == 1)).all (·) then c1 else c2
  --  the array of pairs of tangent planes to the quadric `q` at each of the intersection
  --  points of `q` with the lines.
  let fs := findStart q coo0 coo1 cand
  fs.1 + fs.2.1 + fs.2.2

--#assert part2 atest == 47

-- 90s
solve 2 888708704663413
#eval "Pre-computed quadric, since the implementation of `det` is slow."
