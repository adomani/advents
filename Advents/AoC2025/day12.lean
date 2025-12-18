import Advents.Utils
open Std

/-!
Arranging weirdly shaped presents in rectangles
-/

namespace AoC2025_Day12

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day12" : FilePath).withExtension "input"

/-!
#  Question 1

Determine how many of the rectangles can be filled in by the given numbers of each kind of presents.

There is no part 2!
-/

/-- `test` is the test string for the problem. -/
def test := "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- A `present` is the `HashSet` of `pos`itions that it occupies in a `3 × 3` grid. -/
abbrev present := HashSet pos

/-- A convenience function to print information about a `present`. -/
instance : ToString present where
  toString p := "\n".intercalate (drawSparse p 3 3).toList

/--
A `state` contains
* the `h`eight and `w`idth of the rectangle,
* the assignment `pres` of each index to a `present`,
* the assignment `grs` of each index to the multiplicity of the corresponding `present`.
-/
structure state where
  /-- The height of the `region` -/
  h : Nat
  /-- The width of the `region` -/
  w : Nat
  /-- `pres` assigns to an index, the corresponding `present` -/
  pres : HashMap Nat present
  /-- `grs` assigns to each index the number of presents of that shape that still need placing -/
  grs : HashMap Nat Nat

/-- A convenience function to print information about a `state`. -/
instance : ToString state where
  toString := fun
    | {h, w, pres, grs} => s!"H&W: {(h, w)}, remaining: {grs.toArray.qsort}"

/-- Converts the input string into an array of `state`s. -/
def inputToState (dat : String) : Array state :=
  let parts := dat.trimRight.splitOn "\n\n" |>.toArray
  let (prs, sts) := (parts.pop, parts.back!)
  let pres := prs.foldl (init := ∅) fun tot s =>
    let split := s.splitOn "\n"
    tot.insert split.head!.getNats[0]! (sparseGrid split.tail.toArray (· == '#'))
  (sts.splitOn "\n").foldl (init := ∅) fun tot n =>
    let (dims, grsString) := match n.splitOn ": " with
      | [dims, w] => (dims, w)
      | _ => panic s!"'{n}' does not contain a single ': '!"
    let (h, w) := match dims.getNats with
      | [h, w] => (h, w)
      | _ => panic s!"'{dims}' does not contain two nats!"
    let nums := grsString.getNats
    tot.push <|
    { h := h
      w := w
      pres := pres
      grs := HashMap.ofList <| (List.range nums.length).zipWith (·, ·) nums
      }

/-- Computes the total area that the presents that should be included in the rectangle occupy. -/
def totArea (s : state) : Nat :=
  s.grs.fold (init := 0) fun tot i n => tot + n * ((s.pres.get? i).getD default).size

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let tot := inputToState dat
  -- `ok` counts the rectangles where the shapes fit in disjoint `3 × 3` blocks
  -- `no` counts the rectangles where the total area of the shapes exceeds the area of the rectangle
  -- `maybe` counts the remaining rectangles
  let mut (ok, maybe, no) := (0, 0, 0)
  for s in tot do
    let obv := (s.h / 3) * (s.w / 3)
    let want := s.grs.fold (fun n _ v => n + v) 0
    if want ≤ obv
    then
      ok := ok + 1
    else
    if s.h * s.w < totArea s then
      no := no + 1
    else
      maybe := maybe + 1
  -- `maybe = 0` means that every rectangle can either be trivially filled or is too small
  if maybe == 0 then
    return ok
  else
    panic "I don't know!"

-- This is commented, since the example is *not* trivially decided!
--#assert part1 atest == ???

solve 1 410 file

end AoC2025_Day12
