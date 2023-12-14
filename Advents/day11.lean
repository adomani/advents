import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day11.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- A `pos`ition is a pair of integers. -/
abbrev pos := Int × Int

/-- Auxilliary function to `getGal`.
Extract the positions of the galaxies (`#`)
for a string.
The input `rn` represents the row where this parsing happens. -/
def getGalOne (dat : String) (rn : Nat) : Array pos :=
  let cs := dat.toList
  Id.run do
  let mut fin : Array pos := #[]
  for i in [:dat.length] do
    if cs[i]! = '#' then fin := fin.push (rn, i)
  return fin

/-- Extract the positions of the galaxies (`#`)
for a string. -/
def getGal (dat : Array String) : Array pos :=
  Id.run do
  let mut fin : Array pos := #[]
  let mut con := 0
  for i in dat do
    fin := fin ++ getGalOne i con
    con := con + 1
  return fin

/-- Auxilliary function to `missingRowsCols`.
`missingOne xs` takes as input an array of integers `xs`.
Assuming that the integers in `xs` are non-negative, `missingOne`
returns the missing integers in `xs` in the range `[0,...,max xs]`. -/
def missingOne (xs : Array Int) : Array Int :=
  let missing := xs.sortAndDeduplicate
  let last := missing.back
  Id.run do
    let mut fin : Array Int := #[]
    for i in [:last.toNat] do
      if ! (↑i) ∈ missing then fin := fin.push i
    return fin

/-- `missingRowsCols dat` takes as input an array of `pos`itions `dat`.
It returns the arrays of the missing entries in the rows and columns. -/
def missingRowsCols (dat : Array pos) : Array Int × Array Int :=
  (missingOne (dat.map Prod.fst), missingOne (dat.map Prod.snd))

/-- `expand dat mis fac` takes as input
* an array of `pos`itions `dat`;
* a pair of arrays `mis` of integers;
* an optional scaling factor `fac`, set to `1` by default.
It "expands" the positions in `dat` by inserting `fac` more gaps
in each missing row and column. -/
def expand (dat : Array pos) (mis : Array Int × Array Int) (fac : Int := 1): Array pos :=
  let (mx, my) := mis
  dat.map fun (x, y) =>
    let misx := (mx.filter (· < x)).size
    let misy := (my.filter (· < y)).size
    (x + fac * misx, y + fac * misy)

/-- `distances map fac` takes as input an array of strings `map` and
an optional scaling factor `fac`.
It returns the sum of all (Manhattan) distances between all pairs
of entries in `map` containing a hash (`#`) character.
The distances are computed after expanding by the factor `fac`.
-/
def distances (map : Array String) (fac : Int := 1) : Int :=
  let dat := getGal map
  let misx := missingRowsCols <| dat
  let exp := expand dat misx fac
  Id.run do
  let mut tot := 0
  for i in exp do
    for j in exp do
      let (x, y) := i - j
      tot := tot + (x.natAbs + y.natAbs)
  return tot / 2

#assert distances atest == 374

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Int := distances dat

#assert part1 atest == 374

solve 1 10490062

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Int := distances dat (1000000 - 1)

#assert distances atest 9 == 1030
#assert distances atest 99 == 8410

solve 2 382979724122
