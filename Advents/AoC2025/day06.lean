import Advents.Utils
open Std

namespace AoC2025_Day06

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2025"/"day06" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "\
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  \
"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def inputToArrays (dat : Array String) : Array (Array Nat) × List (Char) :=
  let ops := dat.back!
  ((dat.pop.map (List.toArray ·.getNats)), (String.toList (ops.replace " " "")))

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let (nums, ops) := inputToArrays dat
  let adds := nums.pop.foldl (·.zipWith (· + ·) ·) nums.back!
  let muls := nums.pop.foldl (·.zipWith (· * ·) ·) nums.back!
  let tots := (Array.range ops.length).map fun i => if ops[i]! == '+' then adds[i]! else muls[i]!
  tots.sum

#assert part1 atest == 4277556

solve 1 6503327062445

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let maxLth : Nat := dat.foldl (init := 0) (max · ·.length)
  let dat := dat.map fun d => d ++ List.toString (List.replicate (maxLth - d.length) ' ')
  let trs := Array.transposeString dat
  let nums := trs.map String.getNats
  let nums := nums.toList.splitBy fun l r => l != [] && r != []
  let nums := nums.filterMap fun ls => if ls == [[]] then none else some (ls.map (·[0]!))
  let ops := trs.filterMap fun s =>
    let f1 : String := s.dropWhile fun c => c != '*' && c != '+'
    if f1.isEmpty then none else some (String.Pos.Raw.get f1 0)
  let mut tots := 0
  for i in [0:ops.size] do
    let opi := ops[i]!
    let ni := nums[i]!
    tots := tots + if opi == '*' then ni.toArray.prod else ni.sum
  return tots

#assert part2 atest == 3263827

solve 2 9640641878593

end AoC2025_Day06
