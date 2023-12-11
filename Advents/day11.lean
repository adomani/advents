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

def atest := (test.splitOn "\n").toArray

/-- a `pos`ition is a pair of integers. -/
abbrev pos := Int × Int

/-- the component-wise addition of pairs of integers. -/
instance : Add pos where
  add x y := (x.1 + y.1, x.2 + y.2)

/-- the component-wise subtraction of two pairs of integers. -/
instance : Sub pos where
 sub x y := (x.1 - y.1, x.2 - y.2)

def getGalOne (dat : String) (rn : Nat) : Array pos :=
  let cs := dat.toList
  Id.run do
  let mut fin : Array pos := #[]
  for i in [:dat.length] do
    if cs[i]! = '#' then fin := fin.push (rn, i)
  return fin

#check String.find
def getGal (dat : Array String) : Array pos :=
  Id.run do
  let mut fin : Array pos := #[]
  let mut con := 0
  for i in dat do
    fin := fin ++ getGalOne i con
    con := con + 1
  return fin

--#eval do
--  IO.println <| getGal atest
--  IO.println <| getGal (← IO.FS.lines input)
#check Array.sortAndDeduplicate
def missingOne (xs : Array Int) : Array Int :=
  let missing := xs.sortAndDeduplicate
  let last := missing.back
  Id.run do
    let mut fin : Array Int := #[]
    for i in [:last.toNat] do
      if ! (↑i) ∈ missing then fin := fin.push i
    return fin

def missingRowsCols (dat : Array pos) : Array Int × Array Int :=
  (missingOne (dat.map Prod.fst), missingOne (dat.map Prod.snd))

def expand (dat : Array pos) (mis : Array Int × Array Int) (fac : Int := 1): Array pos :=
  let (mx, my) := mis
  dat.map fun (x, y) =>
    let misx := (mx.filter (· < x)).size
    let misy := (my.filter (· < y)).size
    (x + fac * misx, y + fac * misy)

def parts (map : Array String) (fac : Int := 1) : Int :=
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



#assert parts atest == 374

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Int := parts dat

#assert part1 atest == 374

solve 1 10490062

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Int := parts dat (1000000 - 1)

#assert parts atest 9 == 1030
#assert parts atest 99 == 8410

solve 2 382979724122


#eval do
  let atest := ← IO.FS.lines input
  let dat := getGal atest
  let misx := missingRowsCols <| getGal atest
  let exp := expand dat misx
  let exp := expand dat misx (1000000-1)
  let mut tot := 0
  for i in exp do
    for j in exp do
      let (x, y) := i - j
      tot := tot + (x.natAbs + y.natAbs)
  IO.println <| exp
  IO.println <| tot / 2

--  82000210
