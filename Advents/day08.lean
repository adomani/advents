import Advents.Utils
open Lean

namespace Day08

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day08.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test1` is the first test string for the problem. -/
def test1 := "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

/-- `test2` is the second test string for the problem. -/
def test2 := "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"

/-- `getInstrMvs s` takes the input string `s` and returns the list of instructions together with
the list of pairs `(location, (goLeft, goRight))`. -/
def getInstrMvs (s : String) : (List Char × (List (String × (String × String)))) :=
  match s.splitOn "\n\n" with
    | [i, r] =>
      let rows := (r.splitOn "\n").map fun x => x.splitOn " = "
      let part := rows.map fun a => match a with
        | [init, pair] =>
          let ps := pair.splitOn ", "
          (init, ps[0]!.drop 1, ps[1]!.dropRight 1)
        | _ => dbg_trace "oh no!"; default
      (i.toList, part)
    | _ => dbg_trace "oh no!"; default

/-- `mv c opt` takes a character `c` and a pair of strings `opt` and returns the
`L`eft/`R`ight option depending on whether `c` is `L/R`. -/
def mv (c : Char) (opt : String × String) : String :=
  match c with
    | 'L' => opt.1
    | 'R' => opt.2
    | _ => dbg_trace "mv: oh no!"; default

/-- `runCond first mvs lkup f` takes as input
* the starting position `first` -- a string;
* the list of `L/R` moves -- a list of characters;
* the map `lkup` of the desert -- a list of pairs of a location and the accessible locations;
* a predicate `f` that decides if the current location is an exit.

The function returns the number of steps that it takes to go from `first` to the first time that
we reach a location on which `f` is `true`.
It goes through the moves in `mvs` cyclically and uses `lkup` to figure out the result of each move.
-/
def runCond (first : String) (mvs : List Char)
    (lkup : List (String × (String × String))) (f : String → Bool) :
    Nat :=
  let lth := mvs.length
  Id.run do
    let mut init := first
    let mut steps := 0
    while ! f init do
      let new := (lkup.lookup init).get!
      init := mv (mvs[steps % lth]!) new
      steps := steps + 1
    return steps

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let (mvs, lkup) := getInstrMvs dat
  runCond "AAA" mvs lkup (· == "ZZZ")

#assert part1 test1 == 2
#assert part1 test2 == 6

solve 1 14257 file

/-!
#  Question 2
-/

/-- `test3` is the third test string for the problem. -/
def test3 := "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let (mvs, lkup) := getInstrMvs dat
  let endA := (lkup.filter fun x => String.endsWith (Prod.fst x) "A").map Prod.fst
  let periods := endA.map (runCond · mvs lkup (String.endsWith · "Z"))
  periods.foldl Nat.lcm 1

#assert part2 test3 = 6

solve 2 16187743689077 file
