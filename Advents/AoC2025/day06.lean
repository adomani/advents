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

def charToOp : Char → Option (Nat → Nat → Nat)
  | '*' => some (· * ·)
  | '+' => some (· + ·)
  | ' ' => none
  | c => some (panic s!"'{c}' is not an operation!")

def stringToOp (s : String) : List (Nat → Nat → Nat) :=
  go s.toList
  where go : List Char → List (Nat → Nat → Nat)
    | '*'::rs => (· * ·) :: go rs
    | '+'::rs => (· + ·) :: go rs
    | ' '::rs => go rs
    | c::rs => panic s!"'{c}' is not an operation!" :: go rs
    | [] => []

def inputToArrays (dat : Array String) : Array (Array Nat) × Array (Nat → Nat → Nat) :=
  let ops := dat.back!
  ((dat.pop.map (List.toArray ·.getNats)), (stringToOp ops).toArray)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let (nums, ops) := inputToArrays dat
  let adds := nums.pop.foldl (·.zipWith (· + ·) ·) nums.back!
  let muls := nums.pop.foldl (·.zipWith (· * ·) ·) nums.back!
  let tots := ((Array.range ops.size).filterMap fun i =>
    if ops[i]! 1 1 == 2 then some adds[i]! else some muls[i]!)
  tots.sum

#assert part1 atest == 4277556

solve 1 6503327062445

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let maxLth : Nat := dat.foldl (init := 0) fun tot (n : String) => (max tot n.length)
  let dat := dat.map fun d : String => d ++ List.toString (List.replicate (maxLth - d.length) ' ')
  let trs := Array.transposeString dat
  let nums := trs.map String.getNats
  let nums := nums.toList.splitBy fun l r => l != [] && r != []
  let nums := nums.filterMap fun ls => if ls == [[]] then none else some (ls.map (·[0]!))
  let ops? := trs.filterMap fun s =>
    let f1 : String := String.dropWhile s fun c => c != '*' && c != '+'
    if f1.isEmpty then none else some (charToOp (String.Pos.Raw.get f1 0))
  let ops := ops?.reduceOption
  let mut tots := 0
  for i in [0:ops.size] do
    let opi := ops[i]!
    let ni := nums[i]!
    tots := tots + if opi 1 1 == 1 then ni.toArray.prod else ni.sum
  return tots

#assert part2 atest == 3263827

solve 2 9640641878593

end AoC2025_Day06
