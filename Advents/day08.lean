import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day08.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

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

def mv (c : Char) (opt : String × String) : String :=
  match c with
    | 'L' => opt.1
    | 'R' => opt.2
    | _ => dbg_trace "mv: oh no!"; default

def runCond (first : String) (mvs : List Char) (lkup : (List (String × (String × String)))) (f : String → Bool) :
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

#assert part1 test == 2

solve 1 14257 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let (mvs, lkup) := getInstrMvs dat
  let endA := (lkup.filter fun x => String.endsWith (Prod.fst x) "A").map Prod.fst
  let periods := endA.map (runCond · mvs lkup (String.endsWith · "Z"))
  periods.foldl Nat.lcm 1

#assert part2 test = 2

solve 2 16187743689077 file
