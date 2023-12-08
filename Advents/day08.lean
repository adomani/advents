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

#eval getInstrMvs test

def mv (c : Char) (opt : String × String) : String :=
  match c with
    | 'L' => opt.1
    | 'R' => opt.2
    | _ => dbg_trace "mv: oh no!"; default

def run (mvs : List Char) (lkup : (List (String × (String × String)))) : Nat :=
  let fin := "ZZZ"
  let lth := mvs.length
  Id.run do
    let mut init := "AAA"
    let mut steps := 0
    while init != fin do
      let new := (lkup.lookup init).get!
      init := mv (mvs[steps % lth]!) new
      steps := steps + 1
--      dbg_trace (mvs[i % lth]!, new)
    return steps

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let (mvs, lkup) := getInstrMvs dat
  run mvs lkup

--#assert part1 test == 2

solve 1 14257 file

#eval do
  let (mvs, lkup) := getInstrMvs test
  let (mvs, lkup) := getInstrMvs (← IO.FS.readFile input)
  return run mvs lkup

/-!
#  Question 2
-/

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

#eval do
  let (mvs, lkup) := getInstrMvs test
  let (mvs, lkup) := getInstrMvs (← IO.FS.readFile input)
  let endA := (lkup.filter fun x => String.endsWith (Prod.fst x) "A").map Prod.fst
  let periods := endA.map (runCond · mvs lkup (String.endsWith · "Z"))
  return periods.foldl Nat.lcm 1



#check String.endsWith
def run2 (mvs : List Char) (lkup : (List (String × (String × String)))) : Nat :=
  let fin := "ZZZ"
  let lth := mvs.length
  let endA := (lkup.filter fun x => x.1.endsWith "A").map Prod.fst
  dbg_trace endA
  default
--  Id.run do
--    let mut init := endA.map Prod.fst
--    let mut steps := 0
--    while init != fin do
--      let new := (lkup.lookup init).get!
--      init := mv (mvs[steps % lth]!) new
--      steps := steps + 1
----      dbg_trace (mvs[i % lth]!, new)
--    return steps

#eval do
  let (mvs, lkup) := getInstrMvs test
  let (mvs, lkup) := getInstrMvs (← IO.FS.readFile input)
  return run2 mvs lkup



/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
