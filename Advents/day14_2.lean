import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day14.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `getInfo s` takes as input a string `s` and returns
the list os pairs `(wt, pos)`, where
* `wt` is the number of `O`s contained between consecutive `#`s
  in `s`;
* `pos` is the position of the `#` that immediately precedes the
  current block of `O`s.
-/
def getInfo (s : String) : List (Nat × Nat) :=
  let pc := s.toList
  let lth := pc.length
  let breaks := lth :: (pc.findIdxs (· == '#')).map (lth - 1 - ·)
  let moving := ((pc.filter (! · == '.')).splitOn '#').map List.length
  moving.zip breaks

/-- `getLoad dat` takes as input a list of pairs of natural numbers
and returns the sum of a "binomial-like" count for each entry
of `s`.

This is the "total load" for one row in part 1.
-/
def getLoad (dat : List (Nat × Nat)) : Nat :=
  Id.run do
  let mut tot := 0
  for (wt, pos) in dat do
    tot := tot + ((List.range wt).map (pos - ·)).sum
  return tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let infos := dat.transpose.map getInfo
  (infos.map getLoad).sum

#assert part1 atest == 136

--solve 1 110821

/-!
#  Question 2
-/

def eqa (l m : Array (Int × Int)) : Bool :=
  l.all (· ∈ m)
--  Id.run do
--    let mut cond := true
--    for i in l do
--      if ! i ∈ m then cond := false; break
--    return cond

--#eval do
--  let inp ← IO.FS.readFile input
--  for c in "#O.".toList do
--    IO.println <| s!"{c}: {inp.toList.filter (· == c) |>.length}"

def tiltOne (f mov : Array Int) : Array Int :=
  Id.run do
    let mut fin := #[]
    for p in [:f.size - 1] do
      let pl := f[p]! + 1
      let pr := f[p + 1]!
      let mut con := pl
      for x in mov do
        if (pl ≤ x && x < pr) then
--      let nr := (mov.filter fun x => (pl ≤ x && x < pr)).size
--      for j in [:nr] do
        fin := fin.push (con)
        con := con + 1
    return fin

--#eval
--  let grd := #[-1, 3, 5, 12, 50]
--  let mov := #[1, 0, 2, 8, 7, 11, 48, 33, 41, 13]
--  tiltOne grd mov

--#eval do
--  for ind in [:atest.transpose.size] do
--    let val := atest.transpose[ind]!.toList
--    let x : List Int := -1 :: (val.findIdxs (· == '#')).map (· : Nat → Int) ++ [(val.length : Int)]
--    let y : List Int := (val.findIdxs (· == 'O')).map (· : Nat → Int)
----    dbg_trace (x.toArray, y.toArray)
--    IO.println <| tiltOne x.toArray y.toArray

def tiltN (ff : Array (Array Int)) (mov : Array (Int × Int)) (M : Nat) : Array (Int × Int) :=
--  let ff := (Array.range M).map fun i : Nat =>
--    (#[-1] ++ (((f.filter (Prod.fst · == (i : Int))).map Prod.snd).qsort (· < ·)) ++ #[(M : Int)])
  Id.run do
    let mut new := #[]
    for i in [:M] do
      let mi := (mov.filter (Prod.fst · == (i : Int))).map Prod.snd
      let tlt := tiltOne ff[i]! mi
      new := new ++ (tlt.map (Prod.mk (i : Int) ·))
    return new

def rotate (dat : Array (Int × Int)) (Ny : Nat) : Array (Int × Int) :=
  dat.map (fun (x, y) => (Ny - 1 - y, x))

--#eval do
--  let dat := #[(1,0)]
--  IO.println <| dat
--  IO.println <| rotate dat 2
--  IO.println <| rotate (rotate dat 2) 2
--  IO.println <| rotate (rotate (rotate dat 2) 2) 2
--  IO.println <| rotate (rotate (rotate (rotate dat 2) 2) 2) 2

--#eval do
--  let mut f : Array (Int × Int) := #[]
--  let mut mov : Array (Int × Int) := #[]
--  for ind1 in [:atest.transpose.size] do
--    let ind := ind1 --atest.transpose.size - ind1 - 1
--    let val := atest.transpose[ind]!.toList
--    let x : List Int := -1 :: (val.findIdxs (· == '#')).map (· : Nat → Int) ++ [(val.length : Int)]
--    let y : List Int := (val.findIdxs (· == 'O')).map (· : Nat → Int)
--    f := f ++ x.toArray.map (Prod.mk (ind : Int) ·)
--    mov := mov ++ y.toArray.map (Prod.mk (ind : Int) ·)
--  IO.println <| f
--  IO.println <| rotate f 10
--  IO.println <| f == rotate (rotate (rotate (rotate f 10) 10) 10) 10

def cycle (N W S E : Array (Array Int)) (mov : Array (Int × Int)) (sz : Nat) : Array (Int × Int) :=
  let t1 := mov
  let t1 :=  tiltN N t1 sz
  let t1 := rotate t1 sz
  let t1 :=  tiltN W t1 sz
  let t1 := rotate t1 sz
  let t1 :=  tiltN S t1 sz
  let t1 := rotate t1 sz
  let t1 :=  tiltN E t1 sz
  rotate t1 sz

variable (sz : Nat) (N W S E : Array (Array Int)) in
def cycles : Array (Int × Int) → Nat →  Array (Int × Int)
  | m, 0     => m
  | m, n + 1 => cycles (cycle N W S E m sz) n

set_option profiler true

/-
compilation new took 1.97s
compilation of _eval took 1.08s
interpretation of _eval took 1.05s
elaboration took 472ms

-/
#eval show MetaM _ from do
--  let dat := atest.transpose
  let dat := ← IO.FS.lines input
--  IO.println s!"{(dat.size, dat[0]!.length)}"
  let sz := dat.size
  let mut f : Array (Int × Int) := #[]
  let mut mov : Array (Int × Int) := #[]
  for ind in [:dat.size] do
    let val := dat[ind]!.toList
    let x : List Int := (val.findIdxs (· == '#')).map (· : Nat → Int)
    let y : List Int := (val.findIdxs (· == 'O')).map (· : Nat → Int)
    f := f ++ x.toArray.map (Prod.mk (ind : Int) ·)
    mov := mov ++ y.toArray.map (Prod.mk (ind : Int) ·)
  let N := f
  let W := rotate N sz
  let S := rotate W sz
  let E := rotate S sz
  let N := (Array.range sz).map fun i : Nat =>
    (#[-1] ++ (((N.filter (Prod.fst · == (i : Int))).map Prod.snd).qsort (· < ·)) ++ #[(sz : Int)])
  let W := (Array.range sz).map fun i : Nat =>
    (#[-1] ++ (((W.filter (Prod.fst · == (i : Int))).map Prod.snd).qsort (· < ·)) ++ #[(sz : Int)])
  let S := (Array.range sz).map fun i : Nat =>
    (#[-1] ++ (((S.filter (Prod.fst · == (i : Int))).map Prod.snd).qsort (· < ·)) ++ #[(sz : Int)])
  let E := (Array.range sz).map fun i : Nat =>
    (#[-1] ++ (((E.filter (Prod.fst · == (i : Int))).map Prod.snd).qsort (· < ·)) ++ #[(sz : Int)])
  let cyc0 := mov
  let fi := 100
  let c0 := cycles sz N W S E cyc0 fi
  let mut per := 0
  for i in [:fi] do
    if eqa c0 (cycles sz N W S E c0 (i + 2)) then per := i + 2; break
  IO.println s!"period: {per}"
--  guard ( per == 7 )
  if ! per == 0 then
    let equi := fi + ((1000000000 - fi) % per)
    IO.println equi
    let mut tot := 0
    for i in [:sz] do
  --    IO.print s!"{sz-i} * "
      let curr := (((cycles sz N W S E cyc0 equi).map fun ((x, y) : Int × Int) => (y, x)).filter (Prod.fst · == (i : Int))).qsort (Prod.snd · < Prod.snd ·)
      let cus := curr.size
  --    IO.println <| cus
      tot := tot + cus * (sz - i)
    IO.println s!"Answer: {tot}"
--    guard ( tot == 64 )

#exit

#eval 3 + ((1000000000 - 3) % 7)

#eval 9 + 6*3 + 5*2 + 4*2 + 3*5 + 2 + 5

#exit

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
