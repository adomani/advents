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

/-- `Hash` is the second test string. -/
def Hash := "HASH"

#assert (Hash.toList.map Char.val).map UInt32.val == [72, 65, 83, 72]

/-- `convOne curr c` takes as input the current value `curr` and
a character `c`.
It performs the conversion step on the character `c`,
starting with the current value `curr`. -/
def convOne (curr : Nat) (c : Char) : Nat :=
  (curr + c.val.val) * 17 % 256

#assert convOne 0 'H' == 200

/-- `convert s` takes as input a string and
returns the outcome of the `HASH` algorithm on `s`. -/
def convert (s : String) : Nat :=
  convC 0 s.toList where
  /-- The internal function of `convert` for computing the `HASH` algorithm. -/
  convC : Nat → List Char → Nat
    | n, c::cs => convC (convOne n c) cs
    | n, []    => n

#assert convert Hash == 52

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let dat := (if dat.back == '\n' then dat.dropRight 1 else dat).splitOn ","
  (dat.map convert).sum

#assert part1 test == 1320

solve 1 510013 file

/-!
#  Question 2
-/


#eval do  -- 510013
--  IO.println dat.getLast!
  let dat := atest
  let dat := ((← IO.FS.readFile input).dropRight 1).splitOn "," |>.toArray
  IO.println (← IO.FS.readFile input).back

  IO.println (dat.map convert).sum
  IO.println dat.size
--  for t in dat do IO.println (t, convert t)

#eval Array.mkArray 256 (#[] : Array (Array String))

#eval convert "qp"

/-- returns the box number for the input string. -/
def findBox (s : String) : Nat :=
  let cv := if s.back == '-' then
    convert (s.dropRight 1)
  else
    convert (s.splitOn "=")[0]!
  if 255 < cv then dbg_trace cv; cv else
  cv

#eval convert "rn"

variable (dat : Array (Array String)) in
/-- assumes that `s` ends with `-`, removes
the first lens with label `s.dropRight 1`. -/
def actOneSub (s : String) : Array (Array String) :=
  let lab := findBox s
--  dbg_trace s!"{lab}"
  let pos : Fin dat.size := ⟨lab % dat.size, Nat.mod_lt _ sorry⟩
--  dbg_trace s!"{dat[pos]}"
  let repl := (dat[pos].find? (String.isPrefixOf ((s.dropRight 1).push '='))).get!
--  dbg_trace s!"{repl}"
  let dm := dat.set pos (dat[lab]!.erase repl)
  dm

#eval do
  let init := #[ #[], #["pc"], #[], #["ciao", "pc=1", "d2", "pc=54"] ]
  IO.println init
  IO.println <| actOneSub init "pc-"

variable (dat : Array (Array String)) in
/-- assumes that `s` contains `=`, replaces or adds
the lens with the appropriate label in its relevan box. -/
def actOneEq (s : String) : Array (Array String) :=
  let lab := findBox s
--  dbg_trace s!"{lab}"
  let pos : Fin dat.size := ⟨lab % dat.size, Nat.mod_lt _ sorry⟩
--  dbg_trace s!"{dat[pos]}"
  match (dat[pos].find? (String.isPrefixOf ((s.splitOn "=")[0]!.push '='))) with
    | none => dat.set pos (dat[lab]!.push s)
    | some repl =>
--      dbg_trace "found"
      dat.set pos (dat[lab]!.map fun x => if x == repl then s else x)
--      dat
--  dbg_trace s!"{repl}"
--  let dm := dat.set pos (dat[lab]!.erase repl)
--  dm
#check Array.modify
#eval do
  let init := #[ #[], #["pc"], #[], #["ciao", "pc=1", "d2"] ]
  IO.println init
  let de := actOneSub init "pc-"
  IO.println <| de
  let de := actOneEq de "pc=12"
  IO.println <| de

def combineAux : Array (Array String) → List String → Array (Array String)
  | dat, s::ss =>
    let actOne := if s.back == '-' then actOneSub dat s else actOneEq dat s
    combineAux (actOne) ss
  | dat, [] => dat

def combine (dat : List String) : Array (Array String) :=
  combineAux (Array.mkArray 256 (#[] : Array String)) dat

def focPowOne (a : Array String) : Nat :=
  Id.run do
  let mut tot := 0
  for s in [:a.size] do
    tot := tot + (s + 1) * (a[s]!.toList).getNumbers[0]!
  return tot


#eval do  -- 268497
  let dat := (((← IO.FS.readFile input).dropRight 1).splitOn ",") --.take 100
  let dat := atest.toList
  let alls := (Array.range 256).zipWith ((combine dat).map focPowOne) (Nat.succ · * ·)
  IO.println <| alls
  IO.println <| alls.sum

--  too low:  267407
#exit
#eval do
--  IO.println dat.getLast!
--  let dat := ((← IO.FS.readFile input).dropRight 1).splitOn "," |>.toArray
  let dat := atest
  IO.println <| dat.map findBox



--  wrong: 509879
/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
