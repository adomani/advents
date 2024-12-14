import Advents.Utils
open Lean

namespace Day14

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day14.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- The size of the input area. -/
def size : Nat × Nat := (101, 103)

/-- The size of the test area. -/
def tsize : Nat × Nat := (11, 7)

/--
The main structure to encode the puzzle.
* `ps` is the `HashSet` containing the position and velocity of each robot.
* `sz` is the size of the grid.
-/
structure Robs where
  /-- `ps` is the `HashSet` containing the position and velocity of each robot. -/
  ps : Std.HashSet (pos × pos)
  /-- `sz` is the size of the grid. -/
  sz : Nat × Nat

/-- Convert the input strings and a pair of sizes into a `Robs` structure. -/
def inputToGrid (dat : Array String) (sz : Nat × Nat) : Robs where
  ps := dat.foldl (init := {}) fun h s => match s.getInts with
          | [p1, p2, v1, v2] => h.insert ((p2, p1), (v2, v1))
          | l => panic s!"There should be 4 integers in {l}"
  sz := (sz.2, sz.1)

/-- A convenience function to multiply coordinatewise a position by an integer scalar. -/
instance : HMul Int pos pos where hMul a p := (a * p.1, a * p.2)

/-- A convenience function to add two vectors modulo the grid sizes. -/
def add (r : Robs) (p v : pos) : pos :=
  let a := p + v
  (a.1 % r.sz.1, a.2 % r.sz.2)

/-- Converts the input configuration `r` of robots into the configuration after `n` steps. -/
def move (r : Robs) (n : Int) : Robs :=
  {r with ps := r.ps.fold (init := {}) fun h (p, v) => h.insert (add r p (n * v), v)}

/--
Returns the quadruple of numbers of robots in each one of the four quadrants.
The order of the quadrants is
```
(1 2)
(3 4).
```
-/
def counts (rs : Robs) : Nat × Nat × Nat × Nat :=
  let colSep := rs.sz.2 / 2
  let rowSep := rs.sz.1 / 2
  rs.ps.fold (init := (0, 0, 0, 0)) fun h (p, _v) =>
    h +
      if p.1 < rowSep && p.2 < colSep then (1, 0, 0, 0) else
      if p.1 < rowSep && p.2 > colSep then (0, 1, 0, 0) else
      if p.1 > rowSep && p.2 < colSep then (0, 0, 1, 0) else
      if p.1 > rowSep && p.2 > colSep then (0, 0, 0, 1) else
      (0, 0, 0, 0)

/-- The safety measure is simply the product of the entries of the counts. -/
def safety (rs : Robs) : Nat :=
  let (q1, q2, q3, q4) := counts rs
  q1 * q2 * q3 * q4

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let sz := if dat.size == atest.size then tsize else size
  let rs := inputToGrid dat sz
  let r100 := move rs 100
  safety r100

#assert part1 atest == 12

solve 1 220971520

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let sz := if dat.size == atest.size then tsize else size
  let rs := inputToGrid dat sz
  let bd := max sz.1 sz.2
  let numRobs := rs.ps.size
  -- The expectation is that each quadrant contains approximately `numRobs / 4` robots.
  -- We track the configurations that contain roughly half of this number in two adjacent quadrants.
  let exp := (numRobs / 4) ^ 2 / 4
  let results := (List.range bd).foldl (init := #[]) fun h (i : Nat) =>
    let ri := move rs i
    let (c1, c2, c3, c4) := counts ri
    if c1 * c2 ≤ exp then h.push ((i, sz.2), 3, 4) else
    if c1 * c3 ≤ exp then h.push ((i, sz.1), 2, 4) else
    if c2 * c4 ≤ exp then h.push ((i, sz.1), 1, 3) else
    if c3 * c4 ≤ exp then h.push ((i, sz.2), 1, 2) else
    h
  match results with
    | #[((val1, mod1), r1, r2), ((val2, mod2), s1, _s2)] =>
      let corner := if r1 == s1 then r1 else r2
      let ind := (Array.range mod2).filterMap fun i =>
        if (val1 + i * mod1) % mod2 == val2 then some (val1 + i * mod1) else none
      match ind with
        | #[ind] =>
          let xmas := drawSparse
            ((move rs ind).ps.fold (fun h ((p, _) : pos × pos) => h.insert p) {}) sz.2 sz.1
          dbg_trace String.intercalate "\n" <| s!"Look in quadrant number {corner}" :: xmas.toList
          ind
        | e => dbg_trace "{e} should contain exactly one element!"; 0
    | _ => dbg_trace "Expecting 2 results, but found {results.size}!"; 0

-- The test does not have a picture.
solve 2 6355

end Day14
