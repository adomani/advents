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
    for i in [:100 * lkup.length] do
      let new := (lkup.lookup init).get!
      init := mv (mvs[i % lth]!) new
--      dbg_trace (mvs[i % lth]!, new)
      if init == fin then steps := i + 1; break
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

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
