import Advents.Utils

namespace Day07

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day07.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "16,1,2,0,4,2,7,1,2,14"

/--
Assume that moving entry `d` of `dat` by `i` "costs" `wt d i` abd that we only move to a position that is in the
range `[0..max dat]`.
Then `weightedMove` finds the minimum cost for moving all entries of `dat` to the same position.
-/
def weightedMove (dat : List Nat) (wt : Nat → Nat → Nat) : Nat :=
  (List.range <| dat.max?.getD 0).foldl (fun tots i => tots.push (dat.map (wt · i)).sum) #[]
    |>.min?.getD 0

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  weightedMove dat.getNats fun d i => (d - i) + (i - d)

#assert part1 test == 37

solve 1 335330 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  weightedMove dat.getNats fun d i => (i - d + 1) * (i - d) / 2 + (d - i + 1) * (d - i) / 2

#assert part2 test == 168

solve 2 92439766 file

end Day07
