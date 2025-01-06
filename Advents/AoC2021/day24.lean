import Advents.Utils
open Std

namespace Day24

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day24.input"

/-!
#  Question 1
-/

/-- `test1` is the first test string for the problem. -/
def test1 := "inp x
mul x -1"

/-- `atest1` is the first test string for the problem, split into rows. -/
def atest1 := (test1.splitOn "\n").toArray

/-- `test2` is the second test string for the problem. -/
def test2 := "inp z
inp x
mul z 3
eql z x"

/-- `atest2` is the second test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/-- `test3` is the third test string for the problem. -/
def test3 := "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2"

/-- `atest3` is the third test string for the problem, split into rows. -/
def atest3 := (test3.splitOn "\n").toArray

structure ALU where
  input : List Nat
  var : List (String × Int)
  ops : Array String
  current : Nat
  deriving Inhabited

instance : ToString ALU where
  toString a :=
    --let pairs := "\n".intercalate <| a.var.foldl (init := ["Pairings:"]) fun h (var, val) => h ++ [s!"{var}: {val}"]
    s!"(w, x, y, z) = {a.var.map Prod.snd}\n\
      Current: {a.current}, \
      prevMove: {a.ops[a.current-1]!}, \
      nextMove: {a.ops.getD a.current "Done"}"

def inputToALU (dat : Array String) : ALU where
  input := []
  var := [("w", 0), ("x", 0), ("y", 0), ("z", 0)]
  ops := dat
  current := 0

def modifyEntry (a : ALU) (w : String) (result : Int) : ALU :=
  {a with var := a.var.modify (fun (x, _) => (x, result)) ((a.var.findIdx? (·.1 == w)).getD default)}

def ALU.enter (a : ALU) (l : List Nat) : ALU :=
  {a with input := l}

def valDiv (a b : Int) : Int :=
  let d := a / b
  if b == 0 then dbg_trace "div by zero!"; d else
  if b < 0 then dbg_trace "rounding div?"; d else
  d

def valMod (a b : Int) : Int :=
  let d := a % b
  if a < 0 || b ≤ 0 then dbg_trace "mod zero!!"; d else d

def oneOp (a : ALU) : ALU :=
  let d := a.ops[a.current]!
  let a := {a with current := a.current + 1}
  match d.splitOn with
    | [op, v1, v2] =>
      let w1 := (a.var.lookup v1).getD (v1.toInt?.getD 0)
      let w2 := (a.var.lookup v2).getD (v2.toInt?.getD 0)
      --dbg_trace d
      let result := match op with
        | "add" => w1 + w2
        | "mul" => w1 * w2
        | "div" => valDiv w1 w2
        | "mod" => valMod w1 w2
        | "eql" => if w1 == w2 then 1 else 0
        | _ => panic
          s!"The value of '{op}' is unexpected: it should be 'add', 'mul', 'div', 'mod', or 'eql'"
      modifyEntry a v1 result
    | ["inp", w] => {modifyEntry a w a.input[0]! with input := a.input.drop 1}
    | _ => panic s!"'{d}' Expected exactly 3 words from each line of the input"

def run (a : ALU) (n : Nat) : ALU := Id.run do
  let mut a := a.enter <| Nat.toDigits 10 n |>.map ("".push · |>.toNat!)
  while a.current < a.ops.size do
    a := oneOp a
  a

def finalZ (a : ALU) (n : Nat) : Int :=
  (run a n |>.var.find? (·.1 == "z") |>.getD default).2

--def run (a : ALU) (n : Nat) : Nat := Id.run do
--  let mut a := a.enter <| Nat.toDigits 10 n |>.map ("".push · |>.toNat!)
--  while a.current < a.ops.size do
--    a := oneOp a
--  a.input[3]!

#eval show Lean.Elab.Term.TermElabM _ from do
  let a := run (inputToALU atest1) 4
  guard <| a.var[1]! == ("x", -4)

#eval show Lean.Elab.Term.TermElabM _ from do
  let a := run (inputToALU atest2) 39
  guard <| a.var[3]! == ("z", 1)

#eval 10^13 ≤ 13579246899999
def validate (n : Nat) : Bool :=
  10^13 ≤ n && !(Nat.toDigits 10 n).contains '0'

partial
def firstFalse? (fin : Nat) (cond : Nat → Bool) (bd : Nat) (st : Nat := 0) : Option Nat :=
  if bd == 0 then some 123456789 else
  if st == fin then (if cond st then none else some st) else
  let mid := (st + fin) / 2
  if cond mid then firstFalse? fin cond (bd - 1) (mid + 1) else firstFalse? mid cond (bd - 1) st

#eval 'a'.toNat
#eval (.ofNat 97 : Char)

def toWd (n : Nat) : String :=
  if n = 0 then "" else
  let last : Char := .ofNat <| (n % 26) + 97
  toWd (n / 26) |>.push last

def toDigs (s : String) : Nat :=
  let f := s.toList.map (·.toNat - 96)
  ((List.range f.length).zip f.reverse).foldl (init := 0) fun t (i, c) => t + 10 ^ i * c

#eval toDigs "aaaaaaaaaaaaaa"

def oneDigit (div? : Bool) (i j w z : Int) : Int :=
  --let z := z % 10
  let z? := if div? then z / 26 else z
  let zbar := z % 26
  let z' := zbar + i
  let c := if z' != w then 1 else 0
  let c' := c * 25 + 1
  z? * c' + (w + j) * c

#eval do
  let dat ← IO.FS.lines input
  --let dat := #[dat[0]!] ++ (dat.erase dat[0]!).takeWhile fun s : String => (!s.startsWith "inp")
  IO.println dat
  --let vars : List (String × Int) := [("w", 4), ("z", 2), ("y", 1), ("x", 8)]
  --let a : ALU := {var := vars, ops := #[]}
  let mut a := inputToALU dat
  --let mut oldZ := 0
  -- digits that seem to do nothing: `****_*__[eh?]` penultimate
  --for newZ in [0:26] do
  let newZ := 1
  --a := modifyEntry a "z" newZ
  --for i in [0:100] do
    --let n1 := newZ % 26 + 4
    let n1 := 11111111111111 --+ i
    let anew := run a n1
    IO.println s!"{(anew.var[0]!, anew.var[3]!)}"
    --IO.println <| s!"{(n1)}: {finalZ a n1}, {oneDigit false 12 4 n1 0}"

def _root_.Array.drop (as : Array α) (n : Nat) : Array α :=
  as.reverse.take (as.size - n) |>.reverse

#assert #[1, 2, 3, 4].drop 1 == #[2, 3, 4]
#assert #[1, 2, 3, 4].drop 0 == #[1, 2, 3, 4]
#assert #[1, 2, 3, 4].drop 4 == #[]
#assert #[1, 2, 3, 4].drop 5 == #[]

def _root_.Array.takeRight (as : Array α) (n : Nat) : Array α :=
  as.reverse.take n |>.reverse

#assert #[1, 2, 3, 4].takeRight 1 == #[4]
#assert #[1, 2, 3, 4].takeRight 0 == #[]
#assert #[1, 2, 3, 4].takeRight 4 == #[1, 2, 3, 4]
#assert #[1, 2, 3, 4].takeRight 5 == #[1, 2, 3, 4]

def makeParts (dat : String) : Array (Array String) :=
  let parts := (dat.drop "inp w\n".length).splitOn "inp w\n" |>.foldl (·.push <| "inp w\n" ++ ·) #[]
  --dbg_trace parts
  parts.map (·.trim.splitOn "\n" |>.toArray)

def makeALUs (dat : String) : Array ALU :=
  let parts := (dat.drop "inp w\n".length).splitOn "inp w\n" |>.foldl (·.push <| "inp w\n" ++ ·) #[]
  parts.map (inputToALU <| ·.trim.splitOn "\n" |>.toArray)

def solveALU (a : ALU) (xs : HashMap Int (Array Int)) : HashMap Int (Array Int) := Id.run do
  let mut sols : HashMap Int (Array Int) := ∅
  for d in [1:9] do
    for (oldX, prevDigs) in xs do
      for newX' in [0:26] do
        let newX := 26 ^ prevDigs.size * newX' + oldX
        let a := modifyEntry a "z" newX
        let fiz := finalZ a d
        if oldX == fiz || (3 ≤ prevDigs.size && fiz == newX) then
          sols := sols.insert newX (#[d.cast] ++ prevDigs)
  return sols

partial
def toDs (n : Nat) : Array Nat :=
  if n == 0 then #[] else
  (toDs (n / 26)).push (n % 26)

def solveOneLayer (as : Array ALU) (xs : HashMap Int (Array Int)) :
    Array ALU × HashMap Int (Array Int) :=
  (as.pop, solveALU as.back! xs)

#eval do
  --let dat ← IO.FS.lines input
  let pts := makeParts (← IO.FS.readFile input)
  let a := inputToALU (pts.takeRight 5).flatten
  --let a := modifyEntry a "z" 7733
  --IO.println <| finalZ a 176
  --let a := modifyEntry a "z" 8436
  --IO.println <| finalZ a 287
  for nd in [0:26] do
    let a := modifyEntry a "z" (201069 + nd * 26^4)
    for w in [1:9] do --for w' in [0:9] do
      let res := finalZ a (w * 10000 + 6176)
      if res < 100 then
        IO.println <| s!"{w} & {nd} ↦ {res}"
  --for i in [2:9] do
  --  for j in [2:9] do
  --    IO.println <| s!"2{j}{i}: {finalZ a (200 + j * 10 + i)}"

#eval toDs 219348
#eval toDs 201069
#eval toDs 8436
#eval toDs 7733

--#exit
#eval do
  let dat ← IO.FS.readFile input
  let mut mps := makeALUs dat
  let mut xs : HashMap Int (Array Int) := {(0, #[])}
  let mut con := 1
  while !mps.isEmpty do
    dbg_trace xs.size
    let (mps', sols) := solveOneLayer mps xs --(con - 1) --(min (con-1) (14 - con))
    con := con + 1
    mps := mps'
    IO.println <| sols.toArray.qsort (·.1 < ·.1)
    xs := sols
    --xs := sols.fold (init := ∅) fun h p => h.insert (Prod.snd p)
  --IO.println <| solveALU mp.back! {0} |>.toArray
--#exit
#eval do
  let dat ← IO.FS.readFile input
  let mp := makeParts dat
  for i in [0:mp[0]!.size] do
    let mut row := #[]
    for j in [0:mp.size] do
      row := row.push mp[j]![i]!
    if true || row.sortDedup.size != 1 then
      IO.println s!"{i}"
      IO.println <| "  " ++ "\n  ".intercalate row.sortDedup.toList

#eval do
  --let dat := atest1
  --let dat := atest2
  let dat ← IO.FS.lines input
  let dat := #[dat[0]!] ++ (dat.erase dat[0]!).takeWhile fun s : String => (!s.startsWith "inp")
  IO.println dat
  --let vars : List (String × Int) := [("w", 4), ("z", 2), ("y", 1), ("x", 8)]
  --let a : ALU := {var := vars, ops := #[]}
  let mut a := inputToALU dat
  --let mut oldZ := 0
  -- digits that seem to do nothing: `****_*__[eh?]` penultimate
  for newZ in [0:27] do
--  let newZ := 1
    a := modifyEntry a "z" newZ
    let n1 := newZ % 26 + 4
    if n1 ≤ 9 then
--    for i in [0:10] do
      --let n1 := 0 * 11111111111111 + i
      IO.println <| s!"{(n1, newZ)}: {finalZ a n1}" --", {oneDigit false 12 4 n1 newZ}"

--#exit

  let bd := 34
  for i' in [bd * 26^3:0 * (bd + 10) * 26^3] do
    --let i := i' --26 ^ 8 * (i' + 1)
    let n1 := 11111111111111 + i'
    --let n2 := 11111111111111 + i
    --let cond := [n1].map fun n => n ≤ 10^14 && ! (Nat.toDigits 10 n).contains '0'
    let pair := (finalZ a n1).natAbs --(toWd (finalZ a n1).natAbs)
    if pair ≤ 10000 then --&& ! cond.contains false then
      IO.println <| (i', pair)
      --break
       --else  IO.println <| pair
    --let cond := [n1, n2].map fun n => n ≤ 10^14 && ! (Nat.toDigits 10 n).contains '0'
    --let pair := (toWd (finalZ a n1).natAbs, toWd (finalZ a n2).natAbs)
    --if cond.contains false then IO.println <| (pair, cond) else  IO.println <| pair
  --IO.println <| toWd (finalZ a (toDigs "ihihihhgbeddhc")).natAbs
  --IO.println <| firstFalse? 13579246899999 (fun n => if validate n then finalZ a n != 0 else false) 10
  --IO.println <| finalZ a 13579246899999
--  a := a.enter <| Nat.toDigits 10 13579246899999 |>.map ("".push · |>.toNat!) --[4, 12, 0, 0]
--  IO.println <| a
--  while a.current < a.ops.size do
--    a := oneOp a
--  IO.println s!"{a}\n"



def inputToFun (dat : Array String) (var : List (String × Int)) (n : Int) : Int :=
  dat.foldl (init := n) fun tot s => match s.splitOn with
    | [op, v1, v2] =>
      let w1 := match var.lookup v1 with | some d => d | none => v1.toInt!
      let w2 := match var.lookup v2 with | some d => d | none => v2.toInt!

      default
    | _ => panic "Expected exactly 3 words from each line of the input"


abbrev out : List (Nat × Nat):= [
  (88976, 3698), (89086, 3698), (89936, 3698), (91036, 3698), (92136, 3698), (93236, 3698),
  (94336, 3698), (95436, 3698), (96536, 3698), (97636, 3698), (98736, 3698), (99976, 3698),
  (100086, 3698), (101036, 3698), (102136, 3698), (103236, 3698), (104336, 3698), (105436, 3698),
  (106536, 3698), (107636, 3698), (108736, 3698), (109936, 3698), (110976, 3698), (111086, 3698),
  (112136, 3698), (113236, 3698), (114336, 3698), (115436, 3698), (116536, 3698), (117636, 3698),
  (118736, 3698), (119936, 3698), (121036, 3698), (121976, 3698), (122086, 3698), (123236, 3698),
  (124336, 3698), (125436, 3698), (126536, 3698), (127636, 3698), (128736, 3698), (129936, 3698),
  (131036, 3698), (132136, 3698), (132976, 3698), (133086, 3698), (134336, 3698), (135436, 3698),
  (136536, 3698), (137636, 3698), (138736, 3698), (139936, 3698), (141036, 3698), (142136, 3698),
  (143236, 3698), (143976, 3698), (144086, 3698), (145436, 3698), (146536, 3698), (147636, 3698),
  (148736, 3698), (149936, 3698), (151036, 3698), (152136, 3698), (153236, 3698), (154336, 3698),
  (154976, 3698), (155086, 3698), (156536, 3698), (157636, 3698), (158736, 3698), (159936, 3698),
  (161036, 3698), (162136, 3698), (163236, 3698), (164336, 3698), (165436, 3698), (165976, 3698),
  (166086, 3698), (167636, 3698), (168736, 3698), (169936, 3698), (171036, 3698), (172136, 3698),
  (173236, 3698), (174336, 3698), (175436, 3698), (176536, 3698), (176976, 3698), (177086, 3698),
  (178736, 3698), (179936, 3698), (181036, 3698), (182136, 3698), (183236, 3698), (184336, 3698),
  (185436, 3698), (186536, 3698), (187636, 3698), (187976, 3698), (188086, 3698), (288936, 3698),
  (299936, 3698), (310936, 3698), (321936, 3698), (332936, 3698), (343936, 3698), (354936, 3698),
  (365936, 3698), (376936, 3698), (387936, 3698), (389036, 3698), (400036, 3698), (411036, 3698),
  (422036, 3698), (433036, 3698), (444036, 3698), (455036, 3698), (466036, 3698), (477036, 3698),
  (488036, 3698), (489136, 3698), (500136, 3698), (511136, 3698), (522136, 3698), (533136, 3698),
  (544136, 3698), (555136, 3698), (566136, 3698), (577136, 3698), (588136, 3698), (589236, 3698),(600236, 3698), (611236, 3698), (622236, 3698), (633236, 3698), (644236, 3698), (655236, 3698),
  (666236, 3698), (677236, 3698), (688236, 3698), (689336, 3698), (700336, 3698), (711336, 3698),
  (722336, 3698), (733336, 3698), (744336, 3698), (755336, 3698), (766336, 3698)
]

#eval do
  let x := out.map Prod.fst
  IO.println <| (Array.range (x.length - 1)).map fun i => x[i+1]! - x[i]!
  --IO.println <| ((Array.range (x.length - 1)).map fun i => x[i+1]! - x[i]!).filter (· % 26 == 0)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--set_option trace.profiler true in solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day24
