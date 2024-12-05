import Advents.Utils
open Lean

namespace Day05

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day05.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
Returns the component-wise sign of `b - a` and the absolute value of the difference `b.1 - a.1`, if non-zero,
and the absolute value of the difference `b.2 - a.2` otherwise.

This is geared towards pairs of vectors that are either parallel to a coordinate axis, or at a 45⁰ degree angle with
the axes.
-/
def toDirDist (a b : Int × Int) : (Int × Int) × Nat :=
  (((b.1 - a.1).sign, (b.2 - a.2).sign), if a.1 != b.1 then (a.1 - b.1).natAbs else (a.2 - b.2).natAbs)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let mut pos : Std.HashMap (Int × Int) Nat := {}
  for s in dat do
    if let [p1, p2, q1, q2] := s.getNats then
      let (dir, dist) := toDirDist (p1, p2) (q1, q2)
      if dir.1 * dir.2 != 0 then continue
      for j in [0:dist+1] do
        let curr := (p1 + j * dir.1, p2 + j * dir.2)
        let val := (pos.get? curr).getD 0
        pos := pos.insert curr (val + 1)
  return pos.fold (init := 0) (fun tot _ tots => if 2 ≤ tots then tot + 1 else tot)

#assert part1 atest == 5

solve 1 6005

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let mut pos : Std.HashMap (Int × Int) Nat := {}
  for s in dat do
    if let [p1, p2, q1, q2] := s.getNats then
      let (dir, dist) := toDirDist (p1, p2) (q1, q2)
      for j in [0:dist+1] do
        let curr := (p1 + j * dir.1, p2 + j * dir.2)
        let val := (pos.get? curr).getD 0
        pos := pos.insert curr (val + 1)
  return pos.fold (init := 0) (fun tot _ tots => if 2 ≤ tots then tot + 1 else tot)

#assert part2 atest == 12
set_option trace.profiler true in
solve 2 23864

end Day05
