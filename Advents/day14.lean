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

solve 1 110821

/-!
#  Question 2
-/

/-- A `pos`ition is a pair of integers. -/
abbrev pos := Int × Int

#eval do
  let inp ← IO.FS.readFile input
  for c in "#O.".toList do
    IO.println <| s!"{c}: {inp.toList.filter (· == c) |>.length}"

abbrev fix := Array (Int × Array Int)

def tiltOne (f mov : Array Int) : Array Int :=
  Id.run do
    let mut fin := #[]
    for p in [:f.size - 1] do
      let pl := f[p]! + 1
      let pr := f[p + 1]!
      let nr := (mov.filter fun x => (pl ≤ x && x < pr)).size
      for j in [:nr] do
        fin := fin.push (pl + j)
    return fin

#eval
  let grd := #[-1, 3, 5, 12, 50]
  let mov := #[1, 0, 2, 8, 7, 11, 48, 33, 41, 13]
  tiltOne grd mov

#eval do
  for ind in [:atest.transpose.size] do
    let val := atest.transpose[ind]!.toList
    let x : List Int := -1 :: (val.findIdxs (· == '#')).map (· : Nat → Int) ++ [(val.length : Int)]
    let y : List Int := (val.findIdxs (· == 'O')).map (· : Nat → Int)
--    dbg_trace (x.toArray, y.toArray)
    IO.println <| tiltOne x.toArray y.toArray

def tiltN (f mov : Array (Int × Int)) (M : Nat) : Array (Int × Int) :=
  Id.run do
    let mut new := #[]
    for i in [:M] do
      let fi := #[-1] ++ (((f.filter (Prod.fst · == (i : Int))).map Prod.snd).qsort (· < ·)) ++ #[(M : Int)]
      let mi := ((mov.filter (Prod.fst · == (i : Int))).map Prod.snd).qsort (· < ·)
--      dbg_trace f!"fi {fi}"
      let tlt := tiltOne fi mi
      new := new ++ (tlt.map (Prod.mk (i : Int) ·))
    return new

def tilt (frm mov : Array (Int × Int)) (M : Nat) : Array (Int × Int) :=
  let ix := Prod.fst
  let jx := Prod.snd
  let kx := fun x y => Prod.mk x y
  Id.run do
    let mut new := #[]
    for i in [:M] do
      let mi := ((mov.filter (ix · == (i : Int))).map jx).qsort (· < ·)
      let fi := ((frm.filter (ix · == (i : Int))).map jx).qsort (· < ·)
      let tlt := tiltOne fi mi
      new := new ++ (tlt.map (kx (i : Int) ·))
    return new

def tiltW (frm mov : Array (Int × Int)) (M : Nat) : Array (Int × Int) :=
  let jx := Prod.fst
  let ix := Prod.snd
  let kx := fun x y : Int => Prod.mk y x
  Id.run do
    let mut new := #[]
    for i in [:M] do
      let mi := ((mov.filter (ix · == (i : Int))).map jx).qsort (· < ·)
      let fi := ((frm.filter (ix · == (i : Int))).map jx).qsort (· < ·)
      let tlt := tiltOne fi mi
      new := new ++ (tlt.map (kx (i : Int) ·))
    return new

def rotateR (dat : Array (Int × Int)) (Ny : Nat) : Array (Int × Int) :=
  dat.map (fun (x, y) => (Ny-1-y, x))

def rotate (dat : Array (Int × Int)) (Nx : Nat) : Array (Int × Int) :=
  dat.map (fun (x, y) => (y, Nx - 1 - x))

#eval do
  let dat := #[(1,0)]
  IO.println <| dat
  IO.println <| rotate dat 2
  IO.println <| rotate (rotate dat 2) 2
  IO.println <| rotate (rotate (rotate dat 2) 2) 2
  IO.println <| rotate (rotate (rotate (rotate dat 2) 2) 2) 2

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

def cycle (f mov : Array (Int × Int)) (N : Nat) : Array (Int × Int) :=
  let rf := f
  let t1 := mov
--  IO.println s!"t1 has {t1.size} elements"
  let t1 :=   tiltN rf t1 N
--  IO.println s!"t1 has {t1.size} elements"
  let rf := rotateR rf N
  let t1 := rotateR t1 N
--  IO.println s!"t1 has {t1.size} elements"
  let t1 :=   tiltN rf t1 N
--  IO.println s!"t1 has {t1.size} elements"
  let rf := rotateR rf N
  let t1 := rotateR t1 N
--  IO.println s!"t1 has {t1.size} elements"
  let t1 :=   tiltN rf t1 N
--  IO.println s!"t1 has {t1.size} elements"
  let rf := rotateR rf N
  let t1 := rotateR t1 N
--  IO.println s!"t1 has {t1.size} elements"
  let t1 :=   tiltN rf t1 N
--  IO.println s!"t1 has {t1.size} elements"
  let rf := rotateR rf N
  let t1 := rotateR t1 N
  t1
--  IO.println s!"t1 has {t1.size} elements"
--  for i in [:10] do
--    IO.println <| ((t1.map fun ((x, y) : Int × Int) => (y, x)).filter (Prod.fst · == (i : Int))).qsort (Prod.snd · < Prod.snd ·)

/-- `lex` is the lexicographic order that is used sorting the "type" of game hands. -/
def lex [BEq α] [LT α] [DecidableRel (α := α) LT.lt] : List α → List α → Bool
  | [], _        => true
  | _, []        => false
  | a::as, b::bs => (b < a) || (a == b) && lex as bs

def eqa (l m : Array (Int × Int)) : Bool :=
  Id.run do
    let mut cond := true
    for i in l do
      if ! i ∈ m then cond := false; break
    return cond

variable (N : Nat) (f : Array (Int × Int)) in
def cycles : Array (Int × Int) → Nat →  Array (Int × Int)
  | m, 0     => m
  | m, n + 1 => cycles (cycle f m N) n

#eval do
  let dat := atest.transpose
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
  let cyc0 := mov
  let fi := 20
  let c0 := cycles sz f cyc0 fi
  for i in [:20] do
    if eqa c0 (cycles sz f c0 (i + 2)) then IO.println (i + 2); break
#exit
  let mut tot := 0
  for i in [:sz] do
    IO.print s!"{sz-i} * "
    let curr := (((cycles sz f cyc0 6).map fun ((x, y) : Int × Int) => (y, x)).filter (Prod.fst · == (i : Int))).qsort (Prod.snd · < Prod.snd ·)
    let cus := curr.size
    IO.println <| cus
    tot := tot + cus * (sz - i)
  IO.println s!"Answer: {tot}"

#eval 3 + ((1000000000 - 3) % 7)

#eval 9 + 6*3 + 5*2 + 4*2 + 3*5 + 2 + 5

#exit

--  IO.println s!"t1 has {t1.size} elements"
  for i in [:10] do
    IO.println <| ((cyc.map fun ((x, y) : Int × Int) => (y, x)).filter (Prod.fst · == (i : Int))).qsort (Prod.snd · < Prod.snd ·)
#exit
  for i in [:10] do
    IO.println <| ((mov.map fun ((x, y) : Int × Int) => (y, x)).filter (Prod.fst · == (i : Int))).qsort (Prod.snd · < Prod.snd ·)
  let rf := f
  let t1 := mov
  IO.println s!"t1 has {t1.size} elements"
  let t1 :=   tiltN rf t1 10
  IO.println s!"t1 has {t1.size} elements"
  let rf := rotateR rf 10
  let t1 := rotateR t1 10
  IO.println s!"t1 has {t1.size} elements"
  let t1 :=   tiltN rf t1 10
  IO.println s!"t1 has {t1.size} elements"
  let rf := rotateR rf 10
  let t1 := rotateR t1 10
  IO.println s!"t1 has {t1.size} elements"
  let t1 :=   tiltN rf t1 10
  IO.println s!"t1 has {t1.size} elements"
  let rf := rotateR rf 10
  let t1 := rotateR t1 10
  IO.println s!"t1 has {t1.size} elements"
  let t1 :=   tiltN rf t1 10
  IO.println s!"t1 has {t1.size} elements"
  let rf := rotateR rf 10
  let t1 := rotateR t1 10
  IO.println s!"t1 has {t1.size} elements"
  for i in [:10] do
    IO.println <| ((t1.map fun ((x, y) : Int × Int) => (y, x)).filter (Prod.fst · == (i : Int))).qsort (Prod.snd · < Prod.snd ·)
#exit
  let t1 := tiltN (rotate f 10) (rotate t1 10) 10
--  IO.println t2
  let t1 := tiltN (rotate (rotate f 10) 10) (rotate t1 10) 10
  let t1 := tiltN (rotate f 10) (rotate t1 10) 10
--  IO.println (tiltN f mov 10 == tilt f mov 10)
--  IO.println s!"f\n{f}\n"
--  IO.println s!"mov\n{mov}\n"
--  IO.println s!"tiltW\n{tiltW f mov 10}\n"
--  IO.println s!"tiltN\n{tiltN f mov 10}\n"

#exit
    let up := mov.qsort (Prod.fst · < Prod.fst ·)
    for i in [:up.back.1.toNat] do
      let f := 4
    default


def cycle (n w s e : Array Int) (mov : Array (Int × Int)) : Array (Int × Int) :=

  sorry



#exit

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
