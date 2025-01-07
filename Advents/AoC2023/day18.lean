import Advents.Utils

namespace Day18

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day18.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `mv` is a pair of a character and a natural number.
For instance, `(U, 3)`. -/
abbrev mv := Char × Nat

/-- Component-wise multiplication by a natural number scalar of a `pos`ition. -/
instance : HMul Nat pos pos where
  hMul n x := (n * x.1, n * x.2)

/-- `signedAreaLeft a l` takes as input a `pos`ition `a` and
a horizontal integer offset `l`.
It interprets `a` as a horizontal of vertical oriented segment,
assuming that at least one coordinate of `a` is zero.
It returns a pair consisting of
* the signed area to the left of `l`, assuming that the edge is part
  of clockwise loop;
* the new offset value at the end of the segment represented by `a`.

The optional `ccw?` `Bool`ean input computes the contribution
assuming that the loop is traversed counter-clockwise.
-/
def signedAreaLeft (a : pos) (l : Int) (ccw? : Bool := false) : Int × Int :=
  let a := if ccw? then (-a.1, a.2) else a
  let negZero (n : Int) : Int := if n < 0 then 0 else 1
  -- moving vertically `↕`:
  if a.2 = 0 then (a.1 * (l + negZero a.1), l) else
  -- moving horizontally `↔`:
  if a.1 = 0 then (a.2.toNat, l + a.2) else
  dbg_trace "signedAreaLeft: should be unreachable!"; default

/-- `content moves` takes as input a list of `pos`itions `moves.
It interprets each entry `a` of `moves` as a horizontal of vertical
oriented segment, assuming that
* at least one coordinate `a` of `moves` is zero,
* the segments in `moves` match tail-to-source and close up
  in a simple, clockwise loop.

`convent moves` returns the number of integral points inside or
on the boundary of the region determined by `moves`.

The optional `ccw?` `Bool`ean input allows you select that the loop
is traversed counter-clockwise direction.
-/
def content (moves : List pos) (ccw? : Bool := false) : Int :=
  Id.run do
  let mut tot : Int := 0
  let mut l   : Int := 0
  for pi in [:moves.length] do
    let p := moves[pi]!
    let (val, nl) := signedAreaLeft p l ccw?
    tot := tot + val
    l := nl
  return tot + 1

/-- `getMvs dat` parses an array of strings `dat` according to the rules for part 1. -/
def getMvs (dat : Array String) : List mv :=
  (dat.map getMvsOne).toList where
  /-- `getMvsOne s` parses a string `s` according to the rules for part 1. -/
  getMvsOne (s : String) : mv :=
    match s.splitOn " " with
      | [c, n, _] => (c.back, n.toNat!)
      | _ => dbg_trace "oh no!"; default

/-- `Char.toPos c` takes as input a character `c` and returns the position
corresponding to `c`.
* The letters `U, D, L, R` get interpreted as `U`p, `D`own, `L`eft, `R`ight
  (used in part 1).
* The numbers `0, 1, 2, 3` get interpreted as `R`ight, `D`own, `L`eft, `U`p
  (used in part 2).
-/
def _root_.Char.toPos : Char → pos
  | 'U' => ((- 1, 0) : pos)
  | '3' => ((- 1, 0) : pos)

  | 'D' => ((  1, 0) : pos)
  | '1' => ((  1, 0) : pos)

  | 'L' => ((0, - 1) : pos)
  | '2' => ((0, - 1) : pos)

  | 'R' => ((0,   1) : pos)
  | '0' => ((0,   1) : pos)

  | _ => dbg_trace "oh no!"; default

#assert "UDLR".toList.map (Char.toPos) == [(-1, 0), (1, 0), (0, -1), (0, 1)]

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Int :=
  content <| (getMvs dat).map fun ((c, n) : Char × Nat) => n * c.toPos

#assert part1 atest == 62

solve 1 40745

/-!
#  Question 2
-/

/-- `Char.toDec c` takes as input a character `c`, assumes that it represents
a hexadecimal digit and returns the corresponding natural number. -/
def _root_.Char.toDec (c : Char) : Nat :=
  match c.toString.toNat? with
    | some n => n
    | none => c.val.val + 10 - 'a'.val.val

#assert "0123456789abcdef".toList.map Char.toDec == List.range 16

/-- `String.toDec s` takes as input a string `s`, assumes that it represents
the digits of a hexadecimal number and returns the corresponding natural number. -/
def _root_.String.toDec (s : String) : Nat :=
  go s.toList.reverse where
  /-- `go l` takes as input a list of characters, assumes that the entries represent
  the digits of a hexadecimal number and converts the list to the corresponding
  natural number.
  This is an auxilliary function to `String.toDec`. -/
  go : List Char → Nat
    | [] => 0
    | c::cs => c.toDec + 16 * go cs

#assert "70c71".toDec == 461937

/-- `getHexMvsOne s` takes as input a string `s` and returns
the move encoded by the puzzle input. -/
def getHexMvsOne (s : String) : mv :=
  match s.splitOn " " with
    | [_, _, h] => ((h.dropRight 1).back, ((h.dropRight 2).drop 1).toDec)
    | _ => dbg_trace "oh no!"; default

/-- `getHexMvs dat` takes as input an array of strings `dat` and returns
the list of moves encoded by the puzzle input. -/
def getHexMvs (dat : Array String) : List mv :=
  (dat.map getHexMvsOne).toList

#assert (getHexMvs atest).map Prod.snd ==
  [461937, 56407, 356671, 863240, 367720, 266681, 577262, 829975, 112010, 829975, 491645,
   686074, 5411, 500254]

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Int :=
  let moves := (getHexMvs dat).map fun (c, n) => n * c.toPos
  content moves

#assert part2 atest == 952408144115

solve 2 90111113594927
