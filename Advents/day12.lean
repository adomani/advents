import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day12.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

def atest := (test.splitOn "\n").toArray

def String.breakDot (s : String) : List (List Char) :=
  ((s.splitOn ".").filter (· != "")).map String.toList

#assert "..#?..#".breakDot == [['#', '?'], ['#']]

partial
def collapseDot (s : String) : List Char :=
  let col := s.dropWhile (· == '.')
  if col.length == 0 then [] else
  if s.get ⟨0⟩ == '.' then
    '.' :: collapseDot col
  else
    let tail := s.dropWhile (· != '.')
    (s.takeWhile (· != '.')).toList ++ collapseDot tail

#assert collapseDot "##?....??.?#..?.." == ['#', '#', '?', '.', '?', '?', '.', '?', '#', '.', '?']
#assert collapseDot "...##?....??.?#..?.." == ['.', '#', '#', '?', '.', '?', '?', '.', '?', '#', '.', '?']

def String.ep (s : String) : List Char × List Nat :=
  match s.splitOn " " with
    | [l, r] => (collapseDot l, r.toList.getNumbers)
    | _ => dbg_trace "misparsed"; default
--  ((s.splitOn ".").filter (· != "")).map String.toList

#eval atest.map String.ep

abbrev red := List Char × List Nat

def advanceOne : red → Option red
  | ('#'::_cs, 0::_ns) => none
  | ('#'::cs, n::ns) => (cs, (n - 1)::ns)
  | ('·'::cs, n::ns) => (cs, n::ns)
  | _ => default

--partial
def advance : red → Option red
  | ([], 0::cs) => some ([], cs)
  | ([], _) => none
--  | ([], [_]) => none
  | ('#'::_cs, 0::_ns) => none
  | ('#'::cs, n::ns) => advance (cs, (n - 1)::ns)
  | ('.'::cs, 0::ns) => advance (cs, ns)
  | ('.'::cs, n::ns) => advance (cs, n::ns)
  | x => x
  termination_by _ => by
                        rename_i x
                        exact x.1.length + x.2.length

partial
--def tot : red → Nat
def tot1 (r : red) : Nat :=
  dbg_trace r
  match r with
  | ([], 0::cs) => tot1 ([], cs)
  | ([], []) => 1
  | ([], _) => 0
  | ('#'::_cs, []) => 0
  | ('#'::_cs, 0::_ns) => 0
  | ('#'::cs, n::ns) => match advance (cs, (n - 1)::ns) with
    | none => 0
    | some x => tot1 x
  | ('.'::cs, 0::ns) => match advance (cs, ns) with
    | none => 0
    | some x => tot1 x
  | ('.'::cs, n::ns) => match advance (cs, n::ns) with
    | none => 0
    | some x => tot1 x
  | ('?'::cs, 0::ns) => tot1 (cs, ns)
  | ('?'::cs, ns) => tot1 ('#'::cs, ns) + tot1 (cs, ns)
  | x => dbg_trace s!"1000 {x}"; 1000

partial
--def tot : red → Nat
def tot (r : red) : Nat :=
--  dbg_trace r
  match r with
  | ([], 0::cs) => tot ([], cs)
  | ([], []) => /-dbg_trace "add one";-/ 1 --(1, 1)
  | ([], _) => default
  | ('#'::_cs, []) => default
  | ('#'::_cs, 0::_ns) => default
  | ('#'::'#'::cs, n::ns) => tot ('#'::cs, (n - 1)::ns)
  | ('#'::'.'::cs, 1::ns) => tot (cs, ns)
  | ('#'::'.'::_cs, _::_ns) => default
  | (['#'], 1::ns) => tot ([], ns)
  | (['#'], _) => 0
  | ('#'::'?'::cs, 1::ns) => tot (cs, ns)
  | ('#'::'?'::cs, n::ns) => tot ('#'::cs, (n-1)::ns)
  | ('.'::cs, ns) => tot (cs, ns)
--  | ('.'::cs, 0::ns) => tot (cs, ns)

  | ('?'::cs, 0::ns) => tot (cs, ns)
  | ('?'::cs, []) => tot (cs, [])
--  | ('.'::cs, n::ns) => tot (cs, n::ns)
  | ('?'::cs, ns) => tot ('#'::cs, ns) + tot (cs, ns)
  | x => dbg_trace s!"1000 {x}"; 1000 --(1000, 1000)

#assert ((atest.map String.ep).pop).map tot == #[1,4, 1, 1, 4]
#eval ((atest.map String.ep)).map tot --== #[1,4, 1, 1, 4]

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  (((dat.map String.ep)).map tot).sum

#assert part1 atest == 21

solve 1 6935

/-!
#  Question 2
-/



#exit

#eval do -- 6935
  let dat := (← IO.FS.lines input)
  IO.println <| (((dat.map String.ep)).map tot).sum --== #[1,4, 1, 1, 4]

#eval do
  let v : Array red := #[(['?', '.', '?'], [2])]
  IO.println <| v
  IO.println <| v.map tot

#eval do
  let v : Array red := #[(['?', '#', '?', '?', '?'], [2, 1])]
  IO.println <| v
  IO.println <| v.map tot

#eval show MetaM _ from do
  let v : Array red := #[("???".toList, [1])]
  let v := atest.map String.ep
  let v := #[(atest.map String.ep)[5]!]
  IO.println <| v
  IO.println <| v.map tot


def alr (x : red) : Option red :=
  match advance x with
    | none => none
    | some x => advance (x.1.reverse, x.2.reverse)

partial
def split : red → Array red
  | ('?'::cs, ns) =>
    let init := split (cs, ns)
    match alr ('#'::cs, ns) with
      | none => init
      | some x => init ++ split x
  | x => #[alr x].reduceOption

partial
def psplit : red → Array red
  | ('?'::cs, ns) =>
    let cr := alr (cs, ns)
    (if cr.isSome then #[cr.get!] else #[]).push ('#'::cs, ns)
  | x => #[alr x].reduceOption

partial
def step (xs : Array red) : Array red :=
--  if xs.all (· == xs[0]!) then xs else
  let xlr := (xs.map alr).reduceOption
  let xsp := xlr.map split
  let x2 := xsp.foldl (· ++ ·) #[]
  x2
  --step x2
#eval show MetaM _ from do
  let v : red := ("??".toList, [1])
  IO.println <| v
  let pv := psplit v
  IO.println <| pv
  IO.println <| pv.map alr
  IO.println <| (pv.map alr).reduceOption.map split

--  let sv := step #[v]
--  IO.println <| s!"step: {sv}\n"



#eval show MetaM _ from do
  let _xs : Array red := #[("?##.###".toList, [2,3])]
  let _xs := atest.map String.ep
  let _v := _xs[0]!
  let _v : red := ("??.??".toList, [1, 1])
  let v : red := ("??".toList, [1, 1])
  IO.println <| v
  let sv := step #[v]
  IO.println <| s!"step: {step sv}\n"
#exit
  let xlr := (xs.map alr).reduceOption
  let xsp := xlr.map split
  let x2 := xsp.foldl (· ++ ·) #[]
  IO.println <| x2.map alr
  for x in xs do
    --let x' := (x.1.reverse, x.2.reverse)
    IO.println s!"{split x}\n{((split x).map alr).reduceOption}\n"

abbrev record := List (List Char) × List Nat

def String.parseRow (d : String) : record :=
  match d.splitOn " " with
    | [l, r] => (l.breakDot, r.toList.getNumbers)
    | _ =>
      dbg_trace "parseRow: found more than one space"
      default

#assert "..#?..# 2,3,4".parseRow == ([['#', '?'], ['#']], [2, 3, 4])
#assert "..#?..# 2,3,4".parseRow == ([['#', '?'], ['#']], [2, 3, 4])

#assert atest.map String.parseRow == #[([['?', '?', '?'], ['#', '#', '#']], [1, 1, 3]), ([['?', '?'], ['?', '?'], ['?', '#', '#']], [1, 1, 3]),
  ([['?', '#', '?', '#', '?', '#', '?', '#', '?', '#', '?', '#', '?', '#', '?']], [1, 3, 1, 6]),
  ([['?', '?', '?', '?'], ['#'], ['#']], [4, 1, 1]),
  ([['?', '?', '?', '?'], ['#', '#', '#', '#', '#', '#'], ['#', '#', '#', '#', '#']], [1, 6, 5]),
  ([['?', '#', '#', '#', '?', '?', '?', '?', '?', '?', '?', '?']], [3, 2, 1])]


def eatLeftOne1 : List Char × List Nat → List Char × List Nat
  | ('#'::cs, 0::ns) =>
    dbg_trace "eatLeft -1!"; (cs, (0 - 1)::ns)
  | ('#'::cs, 1::ns) => (cs, ns)
  | ('#'::cs, n::ns) => (cs, (n - 1)::ns)
  | x => x

def eatLeftOne : List Char × List Nat → Option (List Char × List Nat)
  | ('#'::_cs, 0::_ns) => none
    --dbg_trace "eatLeft -1!"; (cs, (0 - 1)::ns)
  | (['#'], 1::ns) => some ([], 0::ns)
  | ('#'::'?'::cs, 1::ns) => (cs, ns)
  | ('#'::'#'::_cs, 1::_ns) => none
    --dbg_trace "eatLeftOne inconsistency"
    --(cs, ns)
  | ('#'::cs, 1::ns) => (cs, ns)
  | ('#'::cs, n::ns) => (cs, (n - 1)::ns)
  | x => x

def reduceOneLeft (l : List Char) (fs : List (List Char)) (r : List Nat) : record :=
  if l = [] && r.getD 0 1 = 0 then
    dbg_trace "here"
    (fs, r.drop 1)
  else (l::fs, r)

def eatLeft : record → Option record
  | l@([], _) => l
  | (f::fs, x) =>
    match eatLeftOne (f, x) with
      | none => none
      | some (l, r) => reduceOneLeft l fs r

#eval
  let dat := "# 1"
  let ps := dat.parseRow
  eatLeft ps

def reduceOneRight (l : List (List Char)) (fs : List Nat) : record :=
  let x := l.getLastD ['1']
  if x == [] && fs.getLast! == 0 then
    (l.dropLast, fs.dropLast)
  else (l, fs)

def eatRight (l : record) : Option record :=
  match (eatLeftOne (l.1.getLast!.reverse, l.2.reverse)) with
    | none => none
    | some (nl, nn) =>
      reduceOneRight (l.1.dropLast ++ [nl.reverse]) nn.reverse

variable (f : record → Option record) in
def eatAllF (l : record) : Option record :=
  Id.run do
    let mut prev := l
    let mut curr := f prev
    if curr.isNone then return none else
    while prev != curr.get! do
      if curr.isNone then return none else
      prev := curr.get!
      curr := f prev
    return curr


def eatAllLeft (l : record) : Option record :=
  eatAllF eatLeft l

def eatAllRight (l : record) : Option record :=
  eatAllF eatRight l
/-
def eatAllRight (l : record) : record :=
  Id.run do
    let mut prev := l
    let mut curr := eatRight prev
    while prev != curr do
      prev := curr
      curr := eatRight prev
    return curr
-/


def split (d : record) : Array record :=
  if (d.1 == [[]] && d.1 == []) && d.2 != [] || (d.1 != [] && d.1 != [[]]) && d.2 == []
  then #[] else
  #[(d.1[0]!.drop 1 :: d.1.drop 1, d.2), (('#'::d.1[0]!.drop 1) :: d.1.drop 1, d.2)]

partial
def iter (d : record) (n : Nat) : Array record :=
  if n = 0 then #[d] else
  match eatAllLeft d with
    | none => #[]
    | some dl =>
      match eatAllRight dl with
        | none => #[]
        | some dr =>
--          let new := split dr
          let splits := (split dr).map (iter · (n - 1))
          splits.foldl (· ++ ·) #[]

--def eatRightOne (l : List Char × List Nat) : List Char × List Nat :=
--  let (nl, nn) := eatLeftOne (l.1.reverse, l.2.reverse)
--  (nl.reverse, nn.reverse)

#eval eatAllLeft ([['#'], ['?', '?'], ['?']], [1, 1, 1])

#eval do
  let dat := atest.map String.parseRow
  let d := dat.back
  let d := dat[1]!
  IO.println <| iter d 5
#exit

#[--([[], [?, ?], [?]], [1, 1, 1])
  ([[#], [?, ?], [?]], [1, 1, 1])
  ([[?], [?]], [1, 1])
  ([[#, ?], [?]], [1, 1])
  --([[], [?]], [1, 1])
  ([[#], [?]], [1, 1])
  ([[], [?]], [1])
  ([[#], [?]], [1])
  --([[], [?]], [1, 1])
  ([[#], [?]], [1, 1])
  --([[]], [1])
  ([[#]], [1])
  ([[], [?]], [1])
  ([[#], [?]], [1])
  ([[], [?, ?], [?]], [1, 1])
  ([[#], [?, ?], [?]], [1, 1])
  ([[?], [?]], [1])
  ([[#, ?], [?]], [1])
  ([[], [?]], [1])
  ([[#], [?]], [1])
  ([[], [?]], [1])
  ([[#], [?]], [1])
]


  let ds := split d
  IO.println ds
  IO.println d
  let reds := (ds.map eatAllLeft).reduceOption

  IO.println <| (reds.map fun x =>
    let xs := split x
    (xs.map eatAllLeft).reduceOption)


#eval do
  let dat := atest.map String.parseRow
--  IO.println dat
  for t in [:dat.size] do
    let d := dat[t]!
    let dl := eatAllLeft d
    let dr := eatAllRight dl.get!
--    IO.println <| d
--    IO.println <| dl
    IO.println <| dr
--    if dr != dl then IO.println <| eatAllRight dr
    IO.println <| ""

#exit


#eval do
  let ind := 0
  let (lcs, nums) := atest[ind]!.parseRow
  let (lcs, nums) := (["?##".toList], [1])
  IO.println <| (lcs, nums)
  IO.println <| eatLeft (lcs, nums)
  IO.println <| eatRight (lcs, nums)
  IO.println <| eatRight <| eatRight (lcs, nums)
  IO.println <| eatRight <| eatRight <| eatRight (lcs, nums)

#eval do
  let dat := atest.map String.parseRow
--  IO.println dat
  for t in [:dat.size] do
    let d := dat[t]!
    let res := eatRight d
    IO.println <| d
    IO.println <| res
    IO.println <| ""
--  IO.println <| eatLeft (lcs, nums)
--  IO.println <| eatRight (lcs, nums)
--  IO.println <| eatRight <| eatRight (lcs, nums)
--  IO.println <| eatRight <| eatRight <| eatRight (lcs, nums)

#exit

def parseLeft : List Char → List Nat
  | '.'::cs => parseLeft cs
  | c::d::cs =>
    let new := parseLeft (d::cs)
    match c == d, new with
      | true, [] => dbg_trace "oh no!"; []
      | true, x::xs => (x + 1)::xs
      | false, [] => if c = '#' then [1] else [0]
      | false, x::xs => (x + 1)::xs
      | _, _ => []

  | _ => []

def parseOne : String →

def getPairs (dat : Array String) : Array (String × List Nat) :=



/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
