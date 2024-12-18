import Advents.Utils
open Lean

namespace Day18

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day18.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def exit := (70, 70)
def one := 1024
def texit := (6, 6)
def tone := 12

structure MS where
  f : Std.HashSet pos
  S : pos := (0, 0)
  tot : Nat := 0
  sz : Nat

def MS.available (ms : MS) (p : pos) : Bool :=
  (! ms.f.contains p) && max p.1 p.2 ≤ ms.sz

def inputToMS (s : Array String) (init sz : Nat) : MS :=
  { f := (s.take init).foldl (fun h p =>
          if let [x, y] := p.getNats then h.insert (y, x) else panic "wrong input") ∅
    sz := sz }

#eval do
  let (dat, sz, ex) := (atest, tone, texit)
  let ms := inputToMS dat sz ex.1
  draw <| drawSparse ms.f (ex.1 + 1) (ex.2 + 1)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--set_option trace.profiler true in solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day18
