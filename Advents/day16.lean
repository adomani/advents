import Advents.Utils
open Lean

namespace Day16

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day16.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|...."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- A `ray` is a pair consisting of a `pos`ition and a `dir`ection. -/
abbrev ray := pos × dir

/-- Adding a direction to a position moves one step in the corresponding direction. -/
instance : HAdd dir pos pos where
  hAdd x p := x.toPos + p

/-- `dir.split c d` takes as input a character `c` and a direction `d`.
It returns the effect that `c` has on a ray moving in the direction `d`. -/
def _root_.dir.split : Char → dir → Array dir
  | '-',  .R => #[.R]
  | '-',  .L => #[.L]
  | '-',   _ => #[.R, .L]

  | '|',  .U => #[.U]
  | '|',  .D => #[.D]
  | '|',   _ => #[.U, .D]

  | '\\', .U => #[.L]
  | '\\', .D => #[.R]
  | '\\', .R => #[.D]
  | '\\', .L => #[.U]

  | '/',  .U => #[.R]
  | '/',  .D => #[.L]
  | '/',  .R => #[.U]
  | '/',  .L => #[.D]

  | _, _ => default

/-- An auxilliary function to process mirrors encoded in the string `s`.
It returns data that `getHSteps` and `getVSteps` use. -/
def getHVSteps (s : String) (left_or_up : dir) (f : Int → pos) : Std.HashMap ray (Array ray) :=
  let sc := s.toList
  let idxs := sc.findIdxs (· != '.')
  if idxs.isEmpty then .empty else
  Id.run do
  let mut new : Std.HashMap ray (Array ray) := .empty

  -- insert the entry `(last+1, .X)` pointing `left_or_up`
  let lastMirrorIdx := idxs[idxs.length-1]!
  let lastSplit := left_or_up.split sc[lastMirrorIdx]!
  let lastInsertable := lastSplit.map (f lastMirrorIdx, ·)
  new := new.insert (f s.length, .X) lastInsertable

  -- insert the entry `(-1, .X)` pointing `right_or_down = left_or_up.rev`
  let firstMirrorIdx := idxs[0]!
  let firstSplit := left_or_up.rev.split sc[firstMirrorIdx]!
  let firstInsertable := firstSplit.map (f firstMirrorIdx, ·)
  new := new.insert (f (- 1), .X) firstInsertable

  -- insert the entries in the middle (could probably include the two special cases above)
  for i in [:idxs.length] do
    let prev := if i = 0 then 0 else idxs[i - 1]!
    let cand := match left_or_up.split sc[prev]! with
      | #[] => #[(f (prev - 1), .X)]
      | x   => x.map (f prev, ·)
    new := new.insert (f idxs[i]!, left_or_up) cand
    let next := if i = idxs.length - 1 then s.length - 1 else idxs[i + 1]!
    let cand := match left_or_up.rev.split sc[next]! with
      | #[] => #[(f next, .X)]
      | x   => x.map (f next, ·)
    new := new.insert (f idxs[i]!, left_or_up.rev) cand
  return new

/-- `getHSteps s row` takes as input a string `s` and an integer `row`.
It interprets the string `s` as the data of the mirrors on the `row`th row.
It returns the `HashMap` sending each `ray` on the `row` column traveling
left (`.L`) or right (`.R`) to where it would be deflected by the
mirrors in `s`.

The function only produces this information for the
* `-1`st position;
* `last+1`th position;
* positions where there is a mirror.
-/
def getHSteps (s : String) (row : Int) : Std.HashMap ray (Array ray) :=
  getHVSteps s .L (row, ·)

/-- `getVSteps s col` takes as input a string `s` and an integer `col`.
It interprets the string `s` as the data of the mirrors on the `col`th column.
It returns the `HashMap` sending each `ray` on the `col` column traveling
upwards (`.U`) or downwards (`.D`) to where it would be deflected by the
mirrors in `s`.

The function only produces this information for the
* `-1`st position;
* `last+1`th position;
* positions where there is a mirror.
-/
def getVSteps (s : String) (col : Int) : Std.HashMap ray (Array ray) :=
  getHVSteps s .U (·, col)

/-- `lth p` is the `ℓ¹`-length of the `pos`ition `p`:
the sum of the absolute values of the coordinates of `p`. -/
def lth (p : pos) : Nat := p.1.natAbs + p.2.natAbs

/-- returns the unsigned distance between `a` and `b` and the `dir`ection
of `a → b`. -/
def getEnum (a b : pos) : Nat × dir :=
  (lth (b - a), match compare a.1 b.1, compare a.2 b.2 with
    | .lt, .eq => .D
    | .gt, .eq => .U
    | .eq, .lt => .R
    | .eq, .gt => .L
    | _, _ => default)

run_cmd Lean.Elab.Command.liftTermElabM do
  let ans := [ (( 0,  1), ( 0, -1)),
               (( 0, -1), ( 0,  1)),
               (( 1,  0), (-1,  0)),
               ((-1,  0), ( 1,  0))].map fun v => getEnum v.1 v.2
  let res : List (Nat × dir) := [(2, .L), (2, .R), (2, .U), (2, .D)]
  guard <| res == ans

/-- returns the `HashMap` with the locations of the mirrors -/
def init (dat : Array String) : Std.HashMap ray (Array ray) :=
  let datt := dat.transpose
  Id.run do
  let mut new := .empty
  for r in [:dat.size] do
    for x in getHSteps dat[r]! r do
      new := new.insert x.1 x.2
  for r in [:datt.size] do
    for x in getVSteps datt[r]! r do
      new := new.insert x.1 x.2
  return new

/-- `mkPath mirs r` takes as input a HashMap `mirs` of deflections and a
starting `ray` `r`.
It returns the `HashMap` of the horizontal or vertical pairs of `ray`s
encoding the path of the ray of light through the maze. -/
def mkPath (mirs : Std.HashMap ray (Array ray)) (r : ray) : Std.HashMap ray ray :=
  Id.run do
  let mut path := .empty
  let mut curr : Array ray := #[r]
  let mut con := 0
  while (! curr.isEmpty) do --∧ con ≤ 70 do
    con := con + 1
    let news := curr.map mirs.get?
    let toAdd := (curr.zipWith news fun x y => (y.getD #[]).map (x, ·)).foldl (· ++ ·) #[]
    curr := (news.reduceOption.foldl (· ++ ·) #[]).filter (! path.contains ·)
    for x in toAdd do
      path := path.insert x.1 x.2
  return path

/-- `reduceToPos path` takes as input a `HashMap` `path` representing
the deflection points of a path in the maze.
It strips off the information about the `dir`ection encoded in the `ray`s
and remembers only the `pos`itions. -/
def reduceToPos (path : Std.HashMap ray ray) : Std.HashSet pos :=
  Id.run do
  let mut fin := .empty
  for (a, b) in path do
    let (ct, d) := getEnum a.1 b.1
    let mut cur := a.1
    for _ in [:ct.succ] do
      fin := fin.insert cur
      cur := d + cur
  return fin

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let new := init dat
  let first : ray := ((0, -1), .X)
  let path := mkPath new first
  let fin := reduceToPos path
  let res := fin.toList.filter fun x :pos =>
    (x.1 > -1 &&
     x.2 > -1 &&
     x.1 < dat.size &&
     x.2 < dat.size )
  res.length

#assert part1 atest == 46

solve 1 7728

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let new := init dat
  Id.run do
  let mut mm := 0
  let mut out := ((List.range dat.size).map fun x : Nat =>
    ([
      ((       x, dat.size), .X),
      ((       x,       -1), .X),
      ((dat.size,        x), .X),
      ((      -1,        x), .X)] : List ray)).flatten.toArray
  while ! out.isEmpty do
    let first := out.back?.getD default
    let path := mkPath new first
    let fin := reduceToPos path
    let (res, toRemove) := fin.toList.partition fun x :pos =>
      (x.1 > -1 &&
       x.2 > -1 &&
       x.1 < dat.size &&
       x.2 < dat.size )
    out := out.filter fun x => (! ((first.1::toRemove).contains x.1))
    out := out.erase first
    mm := max mm res.length
  return mm

#assert part2 atest == 51

solve 2 8061
