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

/-- returns the box number for the input string. -/
def findBox (s : String) : Nat :=
  if s.back == '-' then
    convert (s.dropRight 1)
  else
    convert (s.splitOn "=")[0]!

#assert convert "rn" == 0

variable (dat : Array (Array String)) in
/-- assumes that `s` ends with `-`, removes
the first lens with label `s.dropRight 1`. -/
def actOneSub (s : String) : Array (Array String) :=
  if h : dat.size = 0 then
    #[]
  else
    let lab := findBox s
    let pos : Fin dat.size := ⟨lab % dat.size, Nat.mod_lt _ (Nat.pos_of_ne_zero h)⟩
    let repl := (dat[pos].find? (String.isPrefixOf ((s.dropRight 1).push '='))).getD ""
    dat.set pos (dat[lab]!.erase repl)

variable (dat : Array (Array String)) in
/-- assumes that `s` contains `=`, replaces or adds
the lens with the appropriate label in its relevan box. -/
def actOneEq (s : String) : Array (Array String) :=
  if h : dat.size = 0 then
    #[]
  else
    let lab := findBox s
    let pos : Fin dat.size := ⟨lab % dat.size, Nat.mod_lt _ (Nat.pos_of_ne_zero h)⟩
    match (dat[pos].find? (String.isPrefixOf ((s.splitOn "=")[0]!.push '='))) with
      | none => dat.set pos (dat[lab]!.push s)
      | some repl =>
        dat.set pos (dat[lab]!.map fun x => if x == repl then s else x)

/-- `combine dat` takes as input the list of instructions and
returns the final configuration of lenses in each box. -/
def combine (dat : List String) : Array (Array String) :=
  combineAux (Array.mkArray 256 (#[] : Array String)) dat where
  /-- `combineAux` is an auxilliary function to `combine`. -/
  combineAux : Array (Array String) → List String → Array (Array String)
    | dat, s::ss =>
      let actOne :=
        if s.back == '-' then actOneSub dat s else actOneEq dat s
      combineAux (actOne) ss
    | dat, [] => dat

/-- `focPowOne a` takes as input an array of strings and returns the
focal power of that array.  The actual value will need to be multiplied
by the position of the box containing `a` and then summed all up. -/
def focPowOne (a : Array String) : Nat :=
  Id.run do
  let mut tot := 0
  for s in [:a.size] do
    tot := tot + (s + 1) * (a[s]!.toList).getNumbers[0]!
  return tot

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let dat := (if dat.back == '\n' then dat.dropRight 1 else dat).splitOn ","
  let alls := (Array.range 256).zipWith ((combine dat).map focPowOne) (Nat.succ · * ·)
  alls.sum

#assert part2 test == 145

solve 2 268497 file
