import Advents.Utils

namespace Day13

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2024"/"day13" : FilePath).withExtension "input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"

/--
A `Claw` encodes the data of each machine.
* `A` is the vector by which pushing the left button moves the claw.
* `B` is the vector by which pushing the right button moves the claw.
* `P` is the position of the prize.
-/
structure Claw where
  /-- `A` is the vector by which pushing the left button moves the claw. -/
  A : pos
  /-- `B` is the vector by which pushing the right button moves the claw. -/
  B : pos
  /-- `P` is the position of the prize. -/
  P : pos

/-- Converts the input string to an array of `Claw`s.  The `offset` is useful for part 2. -/
def inputToClaws (s : String) (offset : pos := (0, 0)) : Array Claw :=
  let cl := s.splitOn "\n\n"
  cl.foldl (init := {}) fun h s =>
    match s.getInts with
      | [a1, a2, b1, b2, p1, p2] => h.push {A := (a1, a2), B := (b1, b2), P := offset + (p1, p2)}
      | l => panic s!"'{l}' should have 6 entries!"

/-- Checks whether a position has non-negative coordinates. -/
def isPos (p : pos) : Bool := 0 ≤ p.1 && 0 ≤ p.2

/--
Divides the entries of `p` by `d`, if they are exactly divisible, returning `(-1, -1)` otherwise.
-/
def rescaleIfDivisible (p : pos) (d : Int) : pos :=
  if (p.1 % d, p.2 % d) == (0, 0) then (p.1 / d, p.2 / d) else (- 1, - 1)
/--
The common function for the two parts:
Finds the two lines corresponding to the solutions of the two coordinates,
finds the intersection point, making sure that its entries are integers and non-negative,
tallies the outcome, weighting the first coordinate by `3` and the second one by `1`.
-/
def parts (dat : String) (offset : pos := (0, 0)) : Nat :=
  let claws := inputToClaws dat offset
  claws.foldl (init := 0) fun tot c' =>
    let c := c'
    let (a1, b1) := (c.A.1, c.B.1)
    let (a2, b2) := (c.A.2, c.B.2)
    -- in matrix form, the lines are given by
    -- `(a1 b1) (x) = (c.P.1)`
    -- `(a2 b2) (y) = (c.P.2)`
    -- we compute the determinant of the matrix
    let det := a1 * b2 - b1 * a2
    -- and the rescaled solution, using the adjoint matrix
    let scaledCandidate : pos := (b2 * c.P.1 - b1 * c.P.2, - a2 * c.P.1 + a1 * c.P.2)
    let candidate := rescaleIfDivisible scaledCandidate det
    if isPos candidate then
      tot + (candidate.1 * 3 + candidate.2).natAbs
    else tot

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := parts dat

#assert part1 test == 480

solve 1 28138 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat := parts dat (let μ : Int := 10000000000000; (μ, μ))

#assert part2 test == 875318608908  -- not provided with the puzzle

solve 2 108394825772874 file

end Day13
