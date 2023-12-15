import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day15.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn ",").toArray

def Hash := "HASH"

#eval
  let dat := Hash.toList.map Char.val
  dat.map UInt32.val

def convOne (curr : Nat) (c : Char) : Nat :=
  (curr + c.val.val) * 17 % 256

#assert convOne 0 'H' == 200

def convC : Nat → List Char → Nat
  | n, c::cs => convC (convOne n c) cs
  | n, []    => n

def convert (s : String) : Nat :=
  convC 0 s.toList

#eval 'z'.val.val

#assert convert Hash == 52

#eval do
--  IO.println dat.getLast!
  let dat := atest
  let dat := ((← IO.FS.readFile input).dropRight 1).splitOn "," |>.toArray
  IO.println (← IO.FS.readFile input).back

  IO.println (dat.map convert).sum
  IO.println dat.size
--  for t in dat do IO.println (t, convert t)

--  wrong: 509879

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 (test.splitOn "\n").toArray == ???

--solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
