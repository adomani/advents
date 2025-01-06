import Advents.Utils
open Std

namespace Day02

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day02.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "forward 5
down 5
forward 8
up 3
down 8
forward 2"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- What it means to go `down` for part 1. -/
def dn (s : Int × Int) (d : Int) : Int × Int := (s.1, s.2 + d)

/-- What it means to go `up` for part 1. -/
def up (s : Int × Int) (d : Int) : Int × Int := (s.1, s.2 - d)

/-- What it means to go `forward` for part 1. -/
def fw (s : Int × Int) (d : Int) : Int × Int := (s.1 + d, s.2)

/-- The infix notation for `down`. -/
infixl:25 " down "    => dn

/-- The infix notation for `up`. -/
infixl:25 " up "      => up

/-- The infix notation for `forward`. -/
infixl:25 " forward " => fw

/-- The function that accumulates the the change to horizontal position and depth. -/
def inputToTots (i : Array String) : Int × Int :=
  let fws := i.filterMap fun p => if p.startsWith "forward " then some p.getInts.toArray else none
  let ups := i.filterMap fun p => if p.startsWith "up "      then some p.getInts.toArray else none
  let dns := i.filterMap fun p => if p.startsWith "down "    then some p.getInts.toArray else none
  (0, 0) forward fws.flatten.sum up ups.flatten.sum down dns.flatten.sum

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Int :=
  let (x, y) := inputToTots dat
  x * y

#assert part1 atest == 150

solve 1 1580000

/-!
#  Question 2
-/

/-- What it means to go `down` for part 2, when `aim` is present. -/
def adn (s : Int × Int × Int) (d : Int) : Int × Int × Int := (s.1, s.2.1, s.2.2 + d)

/-- What it means to go `up` for part 2, when `aim` is present. -/
def aup (s : Int × Int × Int) (d : Int) : Int × Int × Int := (s.1, s.2.1, s.2.2 - d)

/-- What it means to go `forward` for part 2, when `aim` is present. -/
def afw (s : Int × Int × Int) (d : Int) : Int × Int × Int := (s.1 + d, s.2.1 + s.2.2 * d, s.2.2)

/-- The infix notation for `down` when `aim` is present. -/
infixl:25 " down "    => adn

/-- The infix notation for `up` when `aim` is present. -/
infixl:25 " up "      => aup

/-- The infix notation for `forward` when `aim` is present. -/
infixl:25 " forward " => afw

/-- The function that accumulates the change to horizontal position, depth and aim. -/
def inputToTotsAim (i : Array String) : Int × Int × Int :=
  i.foldl (init := (0, 0, 0)) fun tot p =>
    if p.startsWith "forward " then tot forward p.getInts.sum
    else if p.startsWith "up " then tot up      p.getInts.sum
    else                            tot down    p.getInts.sum

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Int :=
  let (x, y, _) := inputToTotsAim dat
  x * y

#assert part2 atest == 900

solve 2 1251263225

end Day02
