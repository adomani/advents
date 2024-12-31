import Advents.Utils
open Lean

namespace Day23

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day23.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

inductive AP where | A | B | C | D
  deriving BEq, Hashable

instance : ToString AP where toString
  | .A => "A" | .B => "B" | .C => "C" | .D => "D"

def CharToAP : Char → Option AP
  | 'A' => some .A
  | 'B' => some .B
  | 'C' => some .C
  | 'D' => some .D
  | _ => none

structure Burrow where
  all : Std.HashMap pos Char
  grid : Std.HashSet pos
  ap : Std.HashMap pos AP
  energy : Nat

def inputToBurrow (dat : Array String) : Burrow where
  all := loadGrid dat id
  grid := sparseGrid dat ("ABCD.".toList.contains ·)
  ap := sparseMap dat CharToAP
  energy := 0

def drawBurrow (br : Burrow) : IO Unit := do
  let all := br.all.fold (init := br.all) fun (h : Std.HashMap _ _) p c =>
    if "ABCD".toList.contains c then h.insert p '.' else h
  let bur := br.ap.fold (init := all) fun (h : Std.HashMap _ _) p c => h.insert p <| s!"{c}".get 0
  let (mx, my) := br.all.fold (fun (mx, my) (x, y) _ => (max mx x.natAbs, max my y.natAbs)) (0, 0)
  draw <| drawHash bur (mx + 1) (my + 1)
  IO.println s!"Energy: {br.energy}"

/-- info:
--0123456789012-
0|#############|
1|#...........#|
2|###B#C#B#D###|
3|  #A#D#C#A#  |
4|  #########  |
--0123456789012-

Energy: 0
-/
#guard_msgs in
#eval drawBurrow (inputToBurrow atest)

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

end Day23
