import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day09.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `xbinom coeff degree n` equals `coeff * (n + degree` *choose* `degree)`. -/
def xbinom (coeff : Int) (degree n : Nat) : Int := coeff * Nat.binom (n + degree) degree

/-- The difference of two lists of integers is simply the componentwise difference.
The length of the result is the length of the shortest of the two lists. -/
instance : Sub (List Int) where
  sub x y := x.zipWith (· - ·) y

#assert ([0, 1] - [4] : List Int) = [- 4]
#assert ([0, 1] - [4, 1, 1] : List Int) = [- 4, 0]

/-- `degCoeff l` takes an input a list `l` of integers and returns
a degree `d` and a leading coefficient `c` such that `c * (x choose d)` is a polynomial
whose values on `{ 0, ..., l.length - 1 }` "approximate" `l`.
In practice, it is found by computing iterated first differences and recording the last non-zero list. -/
def degCoeff (l : List Int) : Nat × Int :=
  if l.all (· == l.getD 0 0) then (0, 0) else
  Id.run do
  let mut vals := l
  let mut lcs := #[l.getD 0 0]
  while vals.any (· != 0) do
    vals := vals.drop 1 - vals  -- compute the first difference of the list `vals`
    lcs := lcs.push vals[0]!
  return (lcs.size - 2, lcs.pop.back)

/-- `cs l` takes an input a list `l` of integers and iteratively returns coefficients and degrees of
appearing in the "binomial" expansion re-creating the list `l` when evaluated on
`{ 0, ..., l.length - 1 }`. -/
partial
def cs (l : List Int) : List (Nat × Int) :=
  let first := degCoeff l
  if first = default then default else
  let new := cs (l - (List.range l.length).map (xbinom first.2 first.1 ·))
  first :: new

/-- `toPol a n` takes as input an array of pairs `(degree, coeff)` and an integer `n`.
It returns the value `∑ (degree, coeff) in a, coeff * (n choose degree)`. -/
def toPol (a : Array (Nat × Int)) (n : Nat) : Int :=
  (a.map fun (d, c) => xbinom c d n).sum

/-- `part1 dat` takes as input an array `dat` of lists of integers.
For each entry `di` of `dat`, it computes the polynomial `f_di` of smallest degree such that
`di = #[f_di 0, f_di 1, ..., f_di (di.length - 1)]`.
It returns `∑ di in dat, f_di di.length`, the sum of the next value of each interpolating polynomial. -/
def parts (dat : Array (List Int)) : Int :=
  Id.run do
  let mut tot : Int := 0
  for idx in [:dat.size] do
    let di := dat[idx]!
    let dis := di.length
    let csi := (cs di).toArray
    let corr := di[dis - 1]! - (toPol csi (dis - 1))
    let part := toPol csi dis
    tot := tot + part + corr
  return tot

/-- `part1 oasis` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (oasis : Array String) : Int :=
  parts <| oasis.map String.getInts

#assert part1 atest == 114

solve 1 1884768153

/-!
#  Question 2
-/

/-- `part2 oasis` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (oasis : Array String) : Int :=
  parts <| (oasis.map String.getInts).map .reverse

#assert part2 atest == 2

solve 2 1031
