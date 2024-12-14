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

structure Robs where
  ps : Std.HashSet (pos × pos)
  sz : Nat × Nat

def inputToGrid (dat : Array String) (sz : Nat × Nat) : Robs where
  ps := dat.foldl (init := {}) fun h s => match s.getInts with
          | [p1, p2, v1, v2] => h.insert ((p2, p1), (v2, v1))
          | l => panic s!"There should be 4 integers in {l}"
  sz := (sz.2, sz.1)

instance : HMul Int pos pos where hMul a p := (a * p.1, a * p.2)

def add (r : Robs) (p v : pos) : pos :=
  let a := p + v
  (a.1 % r.sz.1, a.2 % r.sz.2)

def move (r : Robs) (n : Int) : Robs :=
  {r with ps := r.ps.fold (init := {}) fun h (p, v) => h.insert (add r p (n * v), v)}

def tal (a b : Nat × Nat × Nat × Nat) : Nat × Nat × Nat × Nat :=
  (a.1 + b.1, a.2.1 + b.2.1, a.2.2.1 + b.2.2.1, a.2.2.2 + b.2.2.2)

def counts (rs : Robs) : Nat × Nat × Nat × Nat :=
  let colSep := rs.sz.2 / 2
  let rowSep := rs.sz.1 / 2
  rs.ps.fold (init := (0, 0, 0, 0)) fun h (p, _v) =>
    h +
      if p.1 < rowSep && p.2 < colSep then (1, 0, 0, 0) else
      if p.1 > rowSep && p.2 < colSep then (0, 1, 0, 0) else
      if p.1 < rowSep && p.2 > colSep then (0, 0, 1, 0) else
      if p.1 > rowSep && p.2 > colSep then (0, 0, 0, 1) else
      (0, 0, 0, 0)

def safety (rs : Robs) : Nat :=
  let (q1, q2, q3, q4) := counts rs
  q1 * q2 * q3 * q4

#eval do
  let (dat, sz) := (atest, tsize)
  let (dat, sz) := (← IO.FS.lines input, size)
  let rs := inputToGrid dat sz
  --draw <| drawSparse (rs.ps.fold (fun h ((p, v) : pos × pos) => h.insert p) {}) (sz.2) (sz.1)
  let r100 := move rs 100
  --draw <| drawSparse (r100.ps.fold (fun h ((p, v) : pos × pos) => h.insert p) {}) (sz.2) (sz.1)
  IO.println <| safety r100


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

#eval do
  let (dat, sz) := (atest, tsize)
  let (dat, sz) := (← IO.FS.lines input, size)
  let rs := inputToGrid dat sz
  --draw <| drawSparse (rs.ps.fold (fun h ((p, v) : pos × pos) => h.insert p) {}) (sz.2) (sz.1)
  let mut ccs : Std.HashMap _ Nat := {}
  for i in [10: 20] do
    let ri := move rs (- i)
    let cs@(c1, c2, c3, c4) := counts ri
    ccs := ccs.alter cs fun v => some <| v.getD 0 + 1
    --if (c1 == c3 && c2 == c4) || (c1 == c2 && c3 == c4) then
      --IO.println (i, cs)
    draw <| drawSparse (ri.ps.fold (fun h ((p, v) : pos × pos) => h.insert p) {}) (sz.2) (sz.1)
  let reps := ccs.filter fun _ c => (c != 1)
  IO.println reps.size
  IO.println reps.toArray

#eval 101 * 103


/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day14
