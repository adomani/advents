import Advents.Utils
open Lean

set_option profiler true

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

/-- `atest`is simply the splitting of `test` into its array of lines. -/
def atest := (test.splitOn "\n").toArray

/-- `String.ep s` takes as input a string `s` and "easy parses" it.
It breaks it at the (unique) space ` `, converts the LHS to a
list of characters and the RHS to a list of natural numbers. -/
def String.ep (s : String) : List Char × List Nat :=
  match s.splitOn " " with
    | [l, r] => (l.toList, r.getNats)
    | _ => dbg_trace "misparsed"; default

/-- `red` is the type representing the parsed input of the problem.
It consists of a pair `(cs, ns)`, where
* `cs` is a list of characters -- a succession of `#`, `?` and `.`;
* `ns` a list of natural numbers, representing consecutive groups of
  `#`s separated by `.`s.
-/
abbrev red := List Char × List Nat

namespace part1

/-- `tot r` takes as input an element of `red` and returns
the number of assignments of `?` in the first element of the
pair that are compatible con the control sequence given by
the second pair. -/
partial
def tot : red → Nat
  | ([], 0::cs) => tot ([], cs)
  | ([], []) => 1
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
  | ('?'::cs, 0::ns) => tot (cs, ns)
  | ('?'::cs, []) => tot (cs, [])
  | ('?'::cs, ns) => tot ('#'::cs, ns) + tot (cs, ns)
  | x => dbg_trace s!"1000 {x}"; 1000 --(1000, 1000)

#assert (atest.map String.ep).map tot == #[1, 4, 1, 1, 4, 10]

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let ls := ((dat.map String.ep)).map tot
  ls.sum

#assert part1 atest == 21

solve 1 6935

end part1

/-!
#  Question 2
-/
#eval "ciao".toList.getD 5 'u'
partial
def doOne (l : List Char) (n : Nat) : Option (List (List Char)) :=
  if (l.length ≤ n - 1) ∧ (l.contains '#') then
    --dbg_trace "α"
    none
  else
  if l.length ≤ n - 1 then
    --dbg_trace "β"
    some [['A']]
  else
  if l[0]! = '#' ∧ l.getD n 'B' = '#' then
    --dbg_trace "γ"
    none
  else
  if l.getD n 'A' = '#' then
    doOne (l.drop 1) n
  else
  if l[0]! = '#' then
    --dbg_trace "δ"
    some ([l.drop n.succ])
  else
    --dbg_trace "ε"
  some (l.drop n.succ :: (doOne (l.drop 1) n).getD default)
--  default

#eval
  let l := "?#?????".toList
  let n := 2
  doOne l n

partial
def doAll : List (List Char) → List Nat → Nat
  |     l,    [] => if l.all fun cs => cs.all (· == '?') then 1 else 0
  |    [],     _ => 0
  | l::ls, n::ns => match doOne l n with
    | none => 0
    | some nls =>
      let news := nls.map fun nl =>
        if nl = ['A'] then doAll ls (n::ns) else
        doAll (nl :: ls) ns
      --dbg_trace news
      news.sum
--  | _, _ => default

def String.reparseOne (s : String) : List String × List Nat :=
  match s.splitOn " " with
    | [l, r] => ((l.splitOn ".").filter (! · == ""), r.getNats)
    | _ => dbg_trace s!"reparseOne error: {s}"; default

#eval do
  let t := "#?# 2"
  let fir := t.ep
  let (l, r) := t.reparseOne
  let da := doAll (l.map String.toList) r
  if part1.tot fir ≠ da then
    IO.println s!"\n1st: {part1.tot fir}  {fir}\n2nd: {da}  {(l, r)}\n"
  IO.println <| da


#eval show MetaM _ from do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut total := 0
  let mut tots := #[]
  for t in dat do
    let (l, r) := t.reparseOne
    let da := doAll (l.map String.toList) r
    total := total + da
    tots := tots.push da
  IO.println <| total
--  guard (total == 21)
--  guard (tots == #[1, 4, 1, 1, 4, 10])

def extendRed (r : red) (n : Nat := 5) : red :=
  let (l, r) := r
  (['?'].intercalate (List.replicate n l), (List.replicate n r).join)

#exit

#eval
  let l := ["???????"].map String.toList
  let n := [2]
--  doAll l n
  (doOne l[0]! n[0]!, doAll l n)

#eval
  let l := ["#??????"].map String.toList
  let n := [2, 2]
  let l := ["???", "###"].map String.toList
  let n := [1,1,3]
--  doAll l n
  (doOne l[0]! n[0]!, doAll l n)

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
--  draw <| dat
  let mut total := 0
--  for t in dat do
--    let (l, r) := t.reparseOne
--    IO.println <| (l, r)
  for t in dat do
    let fir := t.ep
    let (l, r) := t.reparseOne
    let da := doAll (l.map String.toList) r
    if part1.tot fir ≠ da then IO.println s!"{t}\n1st: '{fir}' {part1.tot fir}\n2nd: '{(l, r)}' {da}\n"
    total := total + da
  IO.println <| total




partial
def noDouble : red → Nat × Option red
  | ([], 0::cs) => noDouble ([], cs)
  | ([], []) => (1, none)
  | ([], _) => default
  | ('#'::_cs, []) => default
  | ('#'::_cs, 0::_ns) => default
  | ('#'::'#'::cs, n::ns) => noDouble ('#'::cs, (n - 1)::ns)
  | ('#'::'.'::cs, 1::ns) => noDouble (cs, ns)
  | ('#'::'.'::_cs, _::_ns) => default
  | (['#'], 1::ns) => noDouble ([], ns)
  | (['#'], _) => (0, none)
  | ('#'::'?'::cs, 1::ns) => noDouble (cs, ns)
  | ('#'::'?'::cs, n::ns) => noDouble ('#'::cs, (n-1)::ns)
  | ('.'::cs, ns) => noDouble (cs, ns)
  | ('?'::cs, 0::ns) => noDouble (cs, ns)
  | ('?'::cs, []) => noDouble (cs, [])
  | x => (0, some x)

/-
partial
def tot (r : red) : Nat :=
  let (n, rl) := noDouble r
  match rl with
    | none => n
    | some rl =>
      let (m, rr) := noDouble (rl.1.reverse, rl.2.reverse)
      match rr with
        | some ('?'::cs, ns) => tot ('#'::cs, ns) + tot (cs, ns) + n + m
        | _ => 0
--  | x => dbg_trace s!"1000 {x}"; 1000 --(1000, 1000)
-/

partial
--def tot : red → Nat
def tot' (n : Nat) (r : red) : Nat × Nat :=
--  dbg_trace "step"
  let dif := (r.1.filter (! · == '.')).length
  if dif < r.2.sum then (n, 0) else
  if dif == r.2.sum && '?' ∈ r.1 then
    let rn := (r.1.map fun x => if x == '?' then '#' else x, r.2)
    tot' n rn else
--  dbg_trace s!"?: {(r.1.filter (· == '?')).length}, #: {(r.1.filter (· == '#')).length}, available {r.2.sum}"
  match r with
  | ([], 0::cs) => tot' n.succ ([], cs)
  | ([], []) => (n, 1)
  | ([], _) => (n, default)
  | ('#'::_cs, []) => (n, default)
  | ('#'::_cs, 0::_ns) => (n, default)
  | ('#'::'#'::cs, n::ns) => tot' n.succ ('#'::cs, (n - 1)::ns)
  | ('#'::'.'::cs, 1::ns) => tot' n.succ (cs, ns)
  | ('#'::'.'::_cs, _::_ns) => (n, default)
  | (['#'], 1::ns) => tot' n.succ ([], ns)
  | (['#'], _) => (n, 0)
  | ('#'::'?'::cs, 1::ns) => tot' n.succ (cs, ns)
  | ('#'::'?'::cs, n::ns) => tot' n.succ ('#'::cs, (n-1)::ns)
  | ('.'::cs, ns) => tot' n.succ (cs, ns)
  | ('?'::cs, 0::ns) => tot' n.succ (cs, ns)
  | ('?'::cs, []) => tot' n.succ (cs, [])
  | ('?'::cs, ns) => tot' n.succ ('#'::cs, ns) + tot' n.succ (cs, ns)
  | x => dbg_trace s!"1000 {x}"; (n, 1000) --(1000, 1000)

def tot1 (r : red) : Nat := (tot' 0 r).2

#assert (atest.map String.ep).map tot1 == #[1, 4, 1, 1, 4, 10]

/-
#[(12, 1), (87, 4), (6, 1), (11, 1), (35, 4), (115, 10)]

-/
#eval (atest.map String.ep).map (tot' 0)
#eval do draw atest

#eval
  dbg_trace ((atest.map String.ep).toList.take 1).map (tot' 0)
  dbg_trace ""
  dbg_trace ((atest.map String.ep).toList.take 1).map fun (x, y) => tot' 0 (x.reverse, y.reverse)
  0
  --IO.print "ciao"

#eval tot1 <| "??#.?#?#??????#.?#?#??????#.?#?#??? 1,3,1,1,3,1,1,3,1".ep

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let ls := ((dat.map String.ep)).map tot1
--  dbg_trace (ls.zip dat).qsort (Prod.fst · > Prod.fst  ·)
  ls.sum
--  (((dat.map String.ep)).map tot).sum

#assert part1 atest == 21

set_option profiler true
solve 1 6935
solve 1 6935
solve 1 6935

/-
#exit

??#.?#?#???  1,3,1
..#..###.#.  1,3,1
..#..###..#  1,3,1

??#.?#?#??? ? ??#.?#?#???
1,3,1         1,3,1


?????.??.???. 1,1,1
??????.??..? 2,1,2
.??#???.??? 3,1,1
??##?#?????.. 5,1
-/

def extendRed (r : red) (n : Nat := 5) : red :=
  let (l, r) := r
  (['?'].intercalate (List.replicate n l), (List.replicate n r).join)

#eval
  let x : red := (".#".toList, [1])
  extendRed x

def String.reparseOne (s : String) : List String × List Nat :=
  match s.splitOn " " with
    | [l, r] => ((l.splitOn ".").filter (! · == ""), r.getNats)
    | _ => dbg_trace s!"reparseOne error: {s}"; default

#eval do
--  let dat ← IO.FS.lines input
  let dat := atest
  draw <| dat
  for t in dat do IO.println <| t.reparseOne

abbrev rec := List String × List Nat
#eval "012".get! ⟨1⟩
#check String.all

partial
--def step : rec → Nat
def step (r: rec) : Nat :=
  let part := r.1.map String.length
  if part.sum < r.2.sum then 0 else
  if part.sum = r.2.sum ∧ part = r.2 then 1 else
  if part.sum = r.2.sum ∧ part = r.2 then 0 else
--  dbg_trace r
  match r with
  | ([], [])       => 1
  | ([], _)        => 0
  | (x, [])        =>
    if (x.map fun s => s.all (· == '?')).all (· == true) then 1 else 0
  | (r::rs, n::ns) =>
    if r.length < n ∧ r.contains '#' then 0 else
    if r.length < n then step (rs, n::ns) else
    if r.length = n ∧ r.contains '#' then step (rs, ns) else
    if r.length = n then step (rs, n::ns) + step (rs, ns) else
    if r.get! ⟨0⟩ == '#' ∧ r.get! ⟨n⟩ = '#' then 0 else
    if r.get! ⟨0⟩ == '#' then step (r.drop n.succ :: rs, ns) else
      step ((r.drop 1)::rs, n::ns) +
        if r.get! ⟨n⟩ ≠ '#' then step (r.drop n.succ :: rs, ns)
        else 0

def repl (s : String) (n : Nat := 5) : String :=
  match s.splitOn " " with
    | [l, r] =>
      "?".intercalate (List.replicate n l) ++ " " ++
      ",".intercalate (List.replicate n r)
    | _ => dbg_trace s!"oh no! {s}"; default

#eval repl "## 1" 2

#eval show MetaM _ from do
  let (nd, mul) := (4, 5)
  let mut fin := 0
  let dat := atest
  let dat ← IO.FS.lines input
  --for ind in [:dat.size] do
  --  let x := repl dat[ind]! mul
  --  fin := fin + step x.reparseOne
  --IO.println fin




/-  loop for testing equalities
#eval show MetaM _ from do
--  let ind := 43
  let dat := atest
  let dat ← IO.FS.lines input
  for ind in [:dat.size] do
--  let x := "?? 1"
  let x := dat[ind]!
--  let pr := dat.map reparseOne
--  IO.println <| s!"x is: {x}\n"
  let cond := (step (x.reparseOne), tot x.ep)
--  IO.println <| cond
  if ! cond.1 = cond.2 then IO.println (ind, x); break
--  for t in dat do IO.println <| reparseOne t

#eval show MetaM _ from do
  let mut fin := 0
  let dat := atest
  let dat ← IO.FS.lines input
  for ind in [:dat.size] do
    let x := dat[ind]!
    fin := fin + step x.reparseOne
  IO.println fin

#eval show MetaM _ from do
  let mut fin := 0
  let dat := atest
  let dat ← IO.FS.lines input
  for ind in [:dat.size] do
    let x := dat[ind]!
    fin := fin + step x.reparseOne
  IO.println fin

#eval show MetaM _ from do
  let mut fin := 0
  let dat := atest
  let dat ← IO.FS.lines input
  for ind in [:dat.size] do
    let x := dat[ind]!
    fin := fin + tot x.ep
  IO.println fin

#eval show MetaM _ from do
  let mut fin := 0
  let dat := atest
  let dat ← IO.FS.lines input
  for ind in [:dat.size] do
    let x := dat[ind]!
    fin := fin + tot x.ep
  IO.println fin
--/

#eval 0
#check String.intercalate

#exit
/-
(525152, #[1, 16384, 1, 16, 2500, 506250])
( 36308, #[1,  2048, 1,  8,  500,  33750])
(  2612, #[1,   256, 1,  4,  100,   2250])
(   206, #[1,    32, 1,  2,   20,    150])
(    21, #[1,     4, 1,  1,    4,     10])
(     6, #[1,     1, 1,  1,    1,      1])

  1,  1,    4,     10
, 1,  2,   20,    150

-/
#eval do
  let ot := [(4, 32), (1, 1), (4, 20), (10, 150)]
  for o in ot do
    let (init, fm) := o
    IO.println <| (List.range 5).map fun n => (init :: List.replicate n (fm/init)).prod
--  IO.println <| (List.range 5).map fun n => (4 :: List.replicate n 8).prod

--def d1 := #[2, 68, 6, 12, 7, 3, 1, 2, 3, 3, 4, 7, 2, 15, 2, 14, 2, 3, 1, 1, 2, 4, 9, 7, 4, 6, 15, 8, 3, 19, 3, 4, 1, 1, 6, 12, 7, 4, 4, 3, 3, 1, 3, 3, 2, 2, 4, 3, 6, 7, 4, 3, 1, 3, 1, 1, 4, 1, 6, 3, 3, 10, 4, 4, 1, 23, 1, 2, 34, 2, 2, 3, 57, 2, 2, 23, 3, 3, 40, 4, 5, 5, 4, 4, 2, 6, 14, 4, 2, 6, 2, 3, 10, 6, 5, 2, 3, 6, 9, 2, 2, 13, 4, 48, 46, 10, 1, 44, 3, 13, 2, 9, 2, 4, 10, 8, 2, 6, 16, 2, 3, 2, 2, 2, 8, 2, 9, 12, 3, 3, 2, 2, 1, 3, 3, 4, 2, 1, 1, 3, 2, 3, 3, 3, 6, 34, 1, 3, 4, 23, 3, 4, 4, 14, 17, 2, 6, 4, 6, 4, 2, 3, 1, 4, 2, 5, 1, 14, 4, 1, 14, 1, 3, 5, 2, 4, 7, 9, 3, 3, 2, 4, 4, 34, 24, 6, 1, 4, 2, 16, 1, 3, 3, 4, 1, 9, 12, 1, 3, 1, 34, 4, 3, 3, 16, 2, 3, 2, 7, 57, 2, 2, 1, 6, 2, 3, 4, 2, 2, 15, 2, 3, 4, 2, 1, 2, 7, 4, 4, 2, 11, 2, 10, 2, 2, 4, 1, 4, 1, 2, 3, 32, 4, 2, 1, 2, 15, 4, 11, 4, 2, 3, 5, 8, 6, 40, 8, 11, 4, 3, 3, 4, 1, 6, 2, 1, 12, 2, 3, 8, 2, 4, 6, 8, 4, 2, 6, 5, 5, 12, 5, 6, 5, 8, 1, 3, 36, 52, 2, 4, 4, 21, 12, 2, 16, 3, 35, 10, 1, 2, 16, 9, 1, 3, 3, 3, 4, 3, 3, 14, 6, 2, 1, 2, 4, 13, 8, 2, 5, 4, 33, 3, 18, 9, 8, 1, 4, 9, 1, 2, 7, 17, 5, 7, 11, 2, 2, 1, 1, 11, 1, 2, 7, 2, 1, 3, 3, 1, 6, 6, 3, 2, 2, 2, 6, 6, 4, 1, 2, 3, 4, 3, 16, 4, 4, 3, 5, 6, 1, 1, 3, 7, 1, 1, 1, 1, 1, 1, 2, 15, 6, 3, 3, 4, 3, 4, 8, 1, 35, 14, 4, 2, 8, 10, 13, 41, 28, 2, 4, 9, 3, 3, 6, 10, 1, 5, 6, 4, 126, 2, 3, 18, 3, 5, 2, 7, 1, 1, 5, 5, 9, 24, 6, 2, 2, 4, 2, 14, 12, 6, 1, 7, 2, 2, 2, 3, 3, 2, 2, 3, 9, 16, 2, 2, 9, 6, 3, 9, 5, 11, 4, 6, 8, 3, 5, 2, 8, 24, 8, 3, 2, 2, 7, 7, 1, 2, 2, 4, 2, 28, 3, 22, 2, 3, 14, 4, 2, 2, 13, 2, 1, 11, 5, 6, 2, 1, 1, 1, 1, 2, 1, 4, 11, 5, 1, 34, 4, 1, 10, 2, 2, 4, 4, 4, 2, 2, 1, 1, 12, 6, 6, 14, 20, 8, 8, 5, 4, 4, 3, 2, 4, 2, 2, 3, 6, 3, 10, 2, 13, 2, 5, 6, 8, 3, 3, 2, 4, 3, 3, 1, 4, 33, 12, 25, 4, 16, 2, 13, 7, 4, 6, 6, 4, 8, 8, 6, 4, 3, 15, 8, 2, 1, 4, 2, 9, 3, 2, 3, 3, 4, 5, 70, 11, 11, 2, 3, 12, 2, 10, 8, 6, 2, 10, 3, 8, 6, 12, 3, 2, 5, 2, 1, 2, 2, 3, 1, 8, 4, 6, 4, 7, 4, 42, 3, 6, 2, 4, 7, 18, 5, 3, 15, 2, 13, 15, 2, 3, 4, 14, 2, 51, 3, 4, 3, 3, 4, 6, 6, 9, 8, 6, 4, 2, 9, 7, 3, 4, 2, 4, 16, 3, 2, 1, 2, 4, 1, 10, 22, 4, 15, 1, 1, 2, 13, 3, 9, 9, 5, 6, 6, 10, 22, 12, 5, 10, 5, 21, 1, 1, 10, 3, 6, 1, 10, 2, 3, 5, 10, 3, 2, 4, 3, 9, 4, 1, 3, 1, 13, 1, 5, 1, 1, 10, 4, 5, 4, 26, 14, 14, 1, 8, 10, 2, 1, 2, 2, 7, 23, 10, 18, 6, 4, 11, 1, 1, 3, 4, 2, 9, 6, 10, 45, 41, 7, 3, 6, 7, 5, 4, 3, 8, 2, 14, 5, 2, 1, 3, 4, 10, 3, 4, 1, 4, 2, 1, 3, 6, 8, 1, 4, 7, 1, 2, 3, 5, 3, 4, 1, 3, 7, 2, 8, 2, 3, 9, 20, 6, 2, 12, 3, 6, 2, 4, 20, 5, 41, 4, 1, 25, 13, 3, 20, 3, 1, 2, 10, 9, 10, 2, 3, 1, 12, 18, 1, 4, 5, 10, 7, 4, 2, 8, 2, 2, 2, 3, 2, 2, 7, 1, 12, 40, 4, 2, 3, 2, 4, 1, 4, 41, 1, 18, 1, 21, 8, 3, 3, 4, 3, 6, 4, 12, 1, 2, 21, 4, 8, 1, 9, 7, 2, 2, 4, 3, 9, 2, 15, 4, 31, 4, 1, 9, 2, 6, 9, 1, 8, 3, 2, 2, 13, 126, 2, 2, 18, 4, 4, 8, 3, 3, 12, 5, 2, 1, 6, 18, 15, 3, 3, 3, 16, 2, 8, 2, 1, 4, 8, 2, 4, 6, 5, 3, 10, 3, 2, 56, 2, 3, 3, 6, 7, 7, 4, 2, 3, 1, 2, 23, 2, 12, 2, 13, 12, 6, 4, 6, 8, 4, 2, 25, 1, 1, 1, 3, 3, 1, 3, 6, 2, 1, 4, 3, 78, 4, 3, 2, 2, 4, 2, 6, 9, 2, 8, 2, 1, 1, 21, 1, 1, 2, 3, 7, 1, 4, 5, 1, 4, 9, 36, 14, 4, 1, 3, 5, 4, 6, 4, 3, 8, 17, 2, 3, 4, 4, 5, 2, 4, 19, 16, 5, 1, 3, 4, 3, 2, 13, 4, 7, 28, 19, 6, 20, 36, 4, 1, 6, 6, 4, 16, 1, 4]
--def d2 := #[8, 13918, 178, 267, 76, 14, 1, 8, 13, 11, 16, 118, 4, 431, 6, 414, 4, 9, 1, 1, 12, 16, 243, 92, 24, 58, 416, 72, 9, 961, 14, 16, 4, 1, 86, 144, 114, 32, 16, 15, 9, 1, 9, 30, 4, 4, 16, 18, 36, 100, 41, 15, 2, 9, 1, 5, 24, 1, 69, 13, 18, 210, 23, 32, 2, 651, 2, 12, 2071, 4, 6, 9, 8966, 8, 8, 704, 31, 14, 2880, 20, 35, 33, 16, 16, 8, 72, 270, 20, 6, 56, 9, 9, 122, 48, 29, 4, 12, 60, 108, 8, 10, 257, 24, 4518, 5333, 120, 2, 3492, 12, 191, 4, 106, 6, 16, 250, 144, 6, 72, 341, 6, 12, 4, 4, 10, 104, 4, 187, 144, 11, 18, 4, 6, 3, 15, 9, 24, 6, 2, 2, 9, 6, 9, 12, 9, 68, 3149, 2, 9, 24, 1209, 9, 31, 43, 434, 740, 12, 150, 16, 36, 20, 5, 11, 2, 16, 4, 37, 1, 256, 32, 1, 348, 1, 15, 39, 6, 36, 115, 133, 9, 15, 4, 16, 20, 2637, 648, 63, 1, 16, 4, 380, 3, 13, 15, 16, 3, 117, 240, 1, 18, 1, 2806, 24, 12, 9, 288, 18, 9, 6, 49, 8318, 4, 6, 1, 36, 4, 21, 44, 6, 4, 300, 4, 9, 97, 4, 2, 4, 104, 36, 19, 4, 144, 4, 221, 4, 4, 28, 1, 16, 1, 4, 27, 1272, 26, 4, 1, 6, 483, 26, 252, 36, 4, 12, 65, 184, 56, 2570, 166, 213, 30, 9, 18, 16, 2, 54, 7, 3, 276, 4, 15, 64, 6, 45, 36, 80, 48, 4, 60, 61, 26, 299, 44, 90, 39, 64, 1, 18, 3060, 7124, 6, 40, 64, 1001, 381, 4, 688, 9, 3544, 268, 1, 6, 443, 162, 2, 28, 18, 15, 36, 9, 18, 364, 63, 4, 2, 4, 20, 291, 192, 4, 39, 28, 2112, 18, 1350, 183, 88, 3, 16, 168, 4, 4, 91, 759, 25, 230, 256, 6, 8, 1, 2, 165, 2, 8, 112, 4, 1, 9, 15, 3, 36, 54, 9, 6, 4, 9, 54, 96, 20, 1, 8, 10, 20, 9, 348, 16, 24, 12, 25, 289, 1, 1, 18, 53, 8, 3, 1, 1, 2, 3, 8, 403, 48, 11, 9, 16, 12, 24, 272, 1, 1821, 338, 36, 4, 146, 176, 269, 3671, 1820, 4, 19, 199, 9, 9, 36, 100, 1, 25, 64, 24, 43758, 12, 12, 750, 9, 30, 6, 97, 3, 2, 45, 33, 103, 1056, 78, 10, 4, 24, 8, 196, 464, 52, 1, 83, 4, 6, 6, 12, 18, 4, 4, 15, 108, 704, 8, 14, 180, 48, 9, 99, 25, 314, 28, 60, 128, 21, 51, 4, 116, 642, 79, 18, 4, 8, 63, 119, 1, 4, 8, 24, 6, 1650, 10, 655, 4, 12, 260, 16, 6, 16, 488, 6, 1, 162, 37, 36, 6, 1, 1, 1, 6, 6, 1, 48, 222, 35, 2, 2976, 20, 3, 317, 4, 4, 29, 40, 36, 6, 4, 1, 5, 216, 72, 36, 251, 598, 174, 112, 52, 24, 16, 18, 6, 21, 4, 4, 18, 36, 9, 310, 4, 223, 4, 33, 77, 96, 15, 18, 4, 20, 18, 48, 1, 24, 1476, 300, 1806, 32, 608, 4, 233, 118, 16, 60, 62, 33, 156, 162, 42, 28, 9, 315, 168, 6, 1, 32, 4, 198, 18, 6, 18, 9, 16, 48, 8820, 241, 246, 4, 12, 264, 4, 282, 98, 84, 4, 100, 11, 104, 52, 244, 34, 4, 59, 8, 2, 4, 6, 12, 3, 120, 36, 60, 24, 93, 16, 1764, 26, 39, 6, 24, 237, 654, 38, 15, 450, 6, 239, 495, 8, 9, 48, 408, 6, 6806, 9, 16, 9, 18, 36, 42, 132, 90, 64, 72, 24, 4, 196, 49, 12, 32, 4, 16, 256, 9, 26, 1, 4, 56, 2, 140, 1548, 37, 330, 2, 1, 4, 379, 12, 191, 156, 31, 54, 72, 210, 728, 664, 35, 180, 49, 914, 1, 1, 201, 25, 65, 2, 200, 4, 13, 45, 145, 30, 4, 37, 19, 180, 16, 9, 13, 1, 240, 1, 84, 4, 1, 272, 16, 28, 19, 1636, 376, 232, 2, 78, 199, 4, 2, 4, 12, 95, 1313, 275, 750, 96, 24, 161, 2, 1, 15, 56, 8, 102, 63, 100, 4895, 3910, 80, 9, 84, 49, 30, 16, 28, 112, 6, 196, 50, 8, 3, 39, 19, 100, 18, 28, 1, 24, 4, 1, 13, 72, 98, 1, 36, 91, 1, 8, 15, 50, 15, 34, 3, 15, 90, 4, 80, 4, 30, 221, 400, 60, 4, 316, 29, 88, 4, 18, 924, 67, 6878, 48, 4, 1755, 521, 14, 400, 12, 2, 4, 100, 81, 230, 4, 15, 1, 250, 450, 4, 64, 30, 200, 98, 20, 4, 80, 4, 20, 4, 9, 8, 6, 49, 1, 228, 5320, 50, 9, 9, 4, 32, 4, 20, 3349, 1, 432, 1, 1206, 80, 9, 20, 24, 9, 81, 16, 378, 2, 4, 1001, 43, 133, 3, 147, 133, 8, 6, 41, 18, 135, 6, 476, 16, 1284, 48, 1, 144, 78, 48, 81, 2, 120, 18, 6, 4, 225, 15876, 10, 8, 468, 16, 25, 96, 18, 9, 144, 43, 8, 1, 42, 624, 408, 21, 90, 38, 466, 8, 102, 4, 1, 24, 142, 4, 20, 36, 50, 15, 156, 9, 6, 5264, 6, 9, 9, 36, 94, 98, 20, 4, 12, 1, 4, 1340, 4, 216, 6, 350, 309, 72, 137, 54, 126, 26, 4, 857, 1, 1, 1, 9, 18, 3, 9, 75, 4, 2, 16, 30, 15004, 16, 20, 15, 9, 16, 8, 71, 315, 8, 80, 4, 2, 1, 713, 2, 1, 4, 9, 89, 1, 32, 47, 4, 28, 142, 1296, 244, 16, 3, 9, 37, 20, 36, 20, 9, 104, 460, 6, 9, 36, 24, 47, 8, 24, 1135, 418, 39, 1, 9, 24, 12, 8, 341, 20, 78, 1910, 525, 36, 1324, 2024, 25, 1, 36, 36, 60, 506, 7, 38]

/-
[1, 68, 13918]
117768619008
-/
#eval 68 * (13918 / 68)^2
/-
[1, 6, 178, 6358]
4243686
-/
#eval 6 * (178 / 6)^2
#eval 6358 / (6) * 6

/-
[1, 2, 8, 36]
[1, 68, 13918, 3280527]
[1, 6, 178, 6358]
[1, 12, 267, 6207]
117775674278

-/
/-
[1, 7, 76, 808]
[1, 3, 14, 78]
[1, 1, 1, 1]
[1, 2, 8, 32]
71281

-/
/-
[1, 2, 8, 36]
[1, 68, 13918, 3280527]
[1, 6, 178, 6358]
[1, 12, 267, 6207]
[1, 7, 76, 808]
[1, 3, 14, 78]
[1, 1, 1, 1]
[1, 2, 8, 32]
0

-/
/-
[1, 3, 13, 61]
[1, 3, 11, 49]
[1, 4, 16, 64]
[1, 7, 118, 2396]
[1, 2, 4, 8]
[1, 15, 431, 14367]
[1, 2, 6, 18]
[1, 14, 414, 13010]
0

-/

/-- `Nat.factorial n` -- the factorial of `n`. -/
def Nat.factorial : Nat → Nat
  | 0 => 1
  | n + 1 => (n + 1) * n.factorial

/-- `Nat.binom n k` -- the binomial coefficient `n choose k`. `n` is allowed to be an integer. -/
def Nat.binom (n : Int) (k : Nat) : Int :=
  ((List.range k).map fun i : Nat => n - i).prod / k.factorial


#eval do
--  let x := atest.map fun f => extendRed (String.ep f) 1
  let x := atest
  let x ← IO.FS.lines input
  let pd := x.map String.ep
  let pd := pd.map fun r : red =>
    let n? := (r.1.filter (· == '?')).length
    let n! := (r.1.filter (· == '#')).length
    (Nat.binom n? (r.2.sum - n!), n? , (r.2.sum - n!), r.2.length)
  for x in pd.qsort (Prod.fst · > Prod.fst ·) do
    IO.println <| x

namespace sec
partial
def tot' (n : Nat) (r : red) : Nat × Nat :=
--  dbg_trace "step"
  let dif := (r.1.filter (! · == '.')).length
  if dif < r.2.sum then (n, 0) else
  if dif == r.2.sum && '?' ∈ r.1 then
    let rn := (r.1.map fun x => if x == '?' then '#' else x, r.2)
    tot' n rn else
--  dbg_trace s!"?: {(r.1.filter (· == '?')).length}, #: {(r.1.filter (· == '#')).length}, available {r.2.sum}"
  match r with
  | ([], 0::cs) => tot' n.succ ([], cs)
  | ([], []) => (n, 1)
  | ([], _) => (n, default)
  | ('#'::_cs, []) => (n, default)
  | ('#'::_cs, 0::_ns) => (n, default)
  | ('#'::'#'::cs, n::ns) => tot' n.succ ('#'::cs, (n - 1)::ns)
  | ('#'::'.'::cs, 1::ns) => tot' n.succ (cs, ns)
  | ('#'::'.'::_cs, _::_ns) => (n, default)
  | (['#'], 1::ns) => tot' n.succ ([], ns)
  | (['#'], _) => (n, 0)
  | ('#'::'?'::cs, 1::ns) => tot' n.succ (cs, ns)
  | ('#'::'?'::cs, n::ns) => tot' n.succ ('#'::cs, (n-1)::ns)
  | ('.'::cs, ns) => tot' n.succ (cs, ns)

  | (c::'#'::d::e::'.'::cs, 5::ns) =>
    (n.succ, 0)

  | ('?'::'.'::cs, 0::ns) => (n.succ, 0)
  | ('?'::'.'::cs, 2::ns) => tot' n.succ (cs, 2::ns)
  | ('?'::'?'::'#'::'.'::cs, 3::ns) => tot' n.succ (cs, ns)

  | ('?'::'#'::'?'::cs, 0::ns) => (n.succ, 0)
  | ('?'::'#'::'?'::cs, 1::ns) => tot' n.succ (cs, ns)
  | ('?'::'#'::'?'::cs, 2::ns) =>
    tot' n.succ (cs, ns) + tot' n.succ ('#'::cs, 1::ns)
  | ('?'::'#'::'?'::cs, m::ns) => tot' n.succ ('?'::'#'::cs, (m-1)::ns)
  | (_::cs, 0::ns) => (n.succ, 0)
  | ('?'::'.'::cs, 1::ns) =>
    tot' n.succ (cs, ns) + tot' n.succ (cs, 1::ns)
  | ('?'::'.'::cs, m::ns) => tot' n.succ (cs, m::ns)

  | ('?'::'#'::'#'::cs, n::ns) =>
    if n ≤ 1 then (n.succ, 0) else
    tot' n.succ ('?'::'#'::cs, (n-1)::ns)
--    tot' n.succ ('#'::cs, 1::ns)
  | ('?'::'#'::cs, 1::ns) => tot' n.succ ('#'::cs, 1::ns)
--  | ('?'::cs, 0::ns) => tot' n.succ (cs, ns)
  | ('?'::cs, []) => tot' n.succ (cs, [])
  | ('?'::cs, ns) =>
    if n ≤ 2 then
      dbg_trace s!"split {r}"
      tot' n.succ ('#'::cs, ns) + tot' n.succ (cs, ns)
    else
      tot' n.succ ('#'::cs, ns) + tot' n.succ (cs, ns)
  | x => dbg_trace s!"1000 {x}"; (n, 1000) --(1000, 1000)

def tot2 (r : red) : Nat := (tot' 0 r).2


#eval show MetaM _ from do
--  let x := atest.map fun f => extendRed (String.ep f) 1
  let x := atest
  let x ← IO.FS.lines input
  let mut d5 := 0
  for f1 in [:x.size] do
--    if f1 ≠ 0 then
      let f := x[f1]!
--    let d1 := tot <| extendRed (String.ep f) 1
--    let d2 := tot <| extendRed (String.ep f) 2
--    if tot (extendRed (String.ep f) 1) < 4 then
--      IO.println <| tot2 <| extendRed (String.ep f) 1
      d5 := d5 + (tot2 <| extendRed (String.ep f) 1)
--      d5 := d5 + 1
--    dbg_trace (List.range 1).map fun n => tot <| extendRed (String.ep f) 1
--    d5 := d5 + (d1 :: List.replicate 4 (d2 / d1)).prod
  IO.println d5
  guard (d5 == 6935 )



#exit


--  too low:    2294250386306
--  too high: 710208077120586
--  let d1 := ((← IO.FS.lines input).map fun f => tot <| extendRed (String.ep f) 1)
--  let d2 := ((← IO.FS.lines input).map fun f => tot <| extendRed (String.ep f) 2)

#eval do
--  let x := atest.map fun f => extendRed (String.ep f) 1
  let x := atest
  let x ← IO.FS.lines input
  let ls := ((x.map String.ep)).map tot
  let lsort := (ls.zip x).qsort (Prod.fst · > Prod.fst ·)
--  for l in lsort do
--    IO.println l
--  IO.println ls.sum
  let mut strip := lsort
  let lsort := lsort.filter (Prod.fst · == 1)
  for k in [:29] do
    let f := strip.back
    strip := strip.pop
    if (String.ep f.2).1.getLast! = '.' then
      IO.println f
      let mut ris := #[]
      for i in [:6] do
        ris := ris.push <| tot <| extendRed (String.ep f.2) i
      IO.println ris
      IO.println ""
#exit
  for o in d1.zip d2 do
      let mut d5 := 0
      let (init, fm) := o
      d5 := d5 + (init :: List.replicate 5 (fm / init)).prod
      IO.println d5
      --return d5
--  IO.println ris
--  let x := (← IO.FS.lines input).map fun f => extendRed (String.ep f) 1
--  let res := x.map fun a => (String.mk a.1).push '\n'
--  let res := x.map tot
--  let ris := (res.sum, res)
--  guard (ris == (525152, #[1, 16384, 1, 16, 2500, 506250]))
--  IO.println <| ris
  --IO.println <| res[3]! --(res.sum, res)
/-
-/


#exit

(6935, #[2, 68, 6, 12, 7, 3, 1, 2, 3, 3, 4, 7, 2, 15, 2, 14, 2, 3, 1, 1, 2, 4, 9, 7, 4, 6, 15, 8, 3, 19, 3, 4, 1, 1, 6, 12, 7, 4, 4, 3, 3, 1, 3, 3, 2, 2, 4, 3, 6, 7, 4, 3, 1, 3, 1, 1, 4, 1, 6, 3, 3, 10, 4, 4, 1, 23, 1, 2, 34, 2, 2, 3, 57, 2, 2, 23, 3, 3, 40, 4, 5, 5, 4, 4, 2, 6, 14, 4, 2, 6, 2, 3, 10, 6, 5, 2, 3, 6, 9, 2, 2, 13, 4, 48, 46, 10, 1, 44, 3, 13, 2, 9, 2, 4, 10, 8, 2, 6, 16, 2, 3, 2, 2, 2, 8, 2, 9, 12, 3, 3, 2, 2, 1, 3, 3, 4, 2, 1, 1, 3, 2, 3, 3, 3, 6, 34, 1, 3, 4, 23, 3, 4, 4, 14, 17, 2, 6, 4, 6, 4, 2, 3, 1, 4, 2, 5, 1, 14, 4, 1, 14, 1, 3, 5, 2, 4, 7, 9, 3, 3, 2, 4, 4, 34, 24, 6, 1, 4, 2, 16, 1, 3, 3, 4, 1, 9, 12, 1, 3, 1, 34, 4, 3, 3, 16, 2, 3, 2, 7, 57, 2, 2, 1, 6, 2, 3, 4, 2, 2, 15, 2, 3, 4, 2, 1, 2, 7, 4, 4, 2, 11, 2, 10, 2, 2, 4, 1, 4, 1, 2, 3, 32, 4, 2, 1, 2, 15, 4, 11, 4, 2, 3, 5, 8, 6, 40, 8, 11, 4, 3, 3, 4, 1, 6, 2, 1, 12, 2, 3, 8, 2, 4, 6, 8, 4, 2, 6, 5, 5, 12, 5, 6, 5, 8, 1, 3, 36, 52, 2, 4, 4, 21, 12, 2, 16, 3, 35, 10, 1, 2, 16, 9, 1, 3, 3, 3, 4, 3, 3, 14, 6, 2, 1, 2, 4, 13, 8, 2, 5, 4, 33, 3, 18, 9, 8, 1, 4, 9, 1, 2, 7, 17, 5, 7, 11, 2, 2, 1, 1, 11, 1, 2, 7, 2, 1, 3, 3, 1, 6, 6, 3, 2, 2, 2, 6, 6, 4, 1, 2, 3, 4, 3, 16, 4, 4, 3, 5, 6, 1, 1, 3, 7, 1, 1, 1, 1, 1, 1, 2, 15, 6, 3, 3, 4, 3, 4, 8, 1, 35, 14, 4, 2, 8, 10, 13, 41, 28, 2, 4, 9, 3, 3, 6, 10, 1, 5, 6, 4, 126, 2, 3, 18, 3, 5, 2, 7, 1, 1, 5, 5, 9, 24, 6, 2, 2, 4, 2, 14, 12, 6, 1, 7, 2, 2, 2, 3, 3, 2, 2, 3, 9, 16, 2, 2, 9, 6, 3, 9, 5, 11, 4, 6, 8, 3, 5, 2, 8, 24, 8, 3, 2, 2, 7, 7, 1, 2, 2, 4, 2, 28, 3, 22, 2, 3, 14, 4, 2, 2, 13, 2, 1, 11, 5, 6, 2, 1, 1, 1, 1, 2, 1, 4, 11, 5, 1, 34, 4, 1, 10, 2, 2, 4, 4, 4, 2, 2, 1, 1, 12, 6, 6, 14, 20, 8, 8, 5, 4, 4, 3, 2, 4, 2, 2, 3, 6, 3, 10, 2, 13, 2, 5, 6, 8, 3, 3, 2, 4, 3, 3, 1, 4, 33, 12, 25, 4, 16, 2, 13, 7, 4, 6, 6, 4, 8, 8, 6, 4, 3, 15, 8, 2, 1, 4, 2, 9, 3, 2, 3, 3, 4, 5, 70, 11, 11, 2, 3, 12, 2, 10, 8, 6, 2, 10, 3, 8, 6, 12, 3, 2, 5, 2, 1, 2, 2, 3, 1, 8, 4, 6, 4, 7, 4, 42, 3, 6, 2, 4, 7, 18, 5, 3, 15, 2, 13, 15, 2, 3, 4, 14, 2, 51, 3, 4, 3, 3, 4, 6, 6, 9, 8, 6, 4, 2, 9, 7, 3, 4, 2, 4, 16, 3, 2, 1, 2, 4, 1, 10, 22, 4, 15, 1, 1, 2, 13, 3, 9, 9, 5, 6, 6, 10, 22, 12, 5, 10, 5, 21, 1, 1, 10, 3, 6, 1, 10, 2, 3, 5, 10, 3, 2, 4, 3, 9, 4, 1, 3, 1, 13, 1, 5, 1, 1, 10, 4, 5, 4, 26, 14, 14, 1, 8, 10, 2, 1, 2, 2, 7, 23, 10, 18, 6, 4, 11, 1, 1, 3, 4, 2, 9, 6, 10, 45, 41, 7, 3, 6, 7, 5, 4, 3, 8, 2, 14, 5, 2, 1, 3, 4, 10, 3, 4, 1, 4, 2, 1, 3, 6, 8, 1, 4, 7, 1, 2, 3, 5, 3, 4, 1, 3, 7, 2, 8, 2, 3, 9, 20, 6, 2, 12, 3, 6, 2, 4, 20, 5, 41, 4, 1, 25, 13, 3, 20, 3, 1, 2, 10, 9, 10, 2, 3, 1, 12, 18, 1, 4, 5, 10, 7, 4, 2, 8, 2, 2, 2, 3, 2, 2, 7, 1, 12, 40, 4, 2, 3, 2, 4, 1, 4, 41, 1, 18, 1, 21, 8, 3, 3, 4, 3, 6, 4, 12, 1, 2, 21, 4, 8, 1, 9, 7, 2, 2, 4, 3, 9, 2, 15, 4, 31, 4, 1, 9, 2, 6, 9, 1, 8, 3, 2, 2, 13, 126, 2, 2, 18, 4, 4, 8, 3, 3, 12, 5, 2, 1, 6, 18, 15, 3, 3, 3, 16, 2, 8, 2, 1, 4, 8, 2, 4, 6, 5, 3, 10, 3, 2, 56, 2, 3, 3, 6, 7, 7, 4, 2, 3, 1, 2, 23, 2, 12, 2, 13, 12, 6, 4, 6, 8, 4, 2, 25, 1, 1, 1, 3, 3, 1, 3, 6, 2, 1, 4, 3, 78, 4, 3, 2, 2, 4, 2, 6, 9, 2, 8, 2, 1, 1, 21, 1, 1, 2, 3, 7, 1, 4, 5, 1, 4, 9, 36, 14, 4, 1, 3, 5, 4, 6, 4, 3, 8, 17, 2, 3, 4, 4, 5, 2, 4, 19, 16, 5, 1, 3, 4, 3, 2, 13, 4, 7, 28, 19, 6, 20, 36, 4, 1, 6, 6, 4, 16, 1, 4])
(312695, #[8, 13918, 178, 267, 76, 14, 1, 8, 13, 11, 16, 118, 4, 431, 6, 414, 4, 9, 1, 1, 12, 16, 243, 92, 24, 58, 416, 72, 9, 961, 14, 16, 4, 1, 86, 144, 114, 32, 16, 15, 9, 1, 9, 30, 4, 4, 16, 18, 36, 100, 41, 15, 2, 9, 1, 5, 24, 1, 69, 13, 18, 210, 23, 32, 2, 651, 2, 12, 2071, 4, 6, 9, 8966, 8, 8, 704, 31, 14, 2880, 20, 35, 33, 16, 16, 8, 72, 270, 20, 6, 56, 9, 9, 122, 48, 29, 4, 12, 60, 108, 8, 10, 257, 24, 4518, 5333, 120, 2, 3492, 12, 191, 4, 106, 6, 16, 250, 144, 6, 72, 341, 6, 12, 4, 4, 10, 104, 4, 187, 144, 11, 18, 4, 6, 3, 15, 9, 24, 6, 2, 2, 9, 6, 9, 12, 9, 68, 3149, 2, 9, 24, 1209, 9, 31, 43, 434, 740, 12, 150, 16, 36, 20, 5, 11, 2, 16, 4, 37, 1, 256, 32, 1, 348, 1, 15, 39, 6, 36, 115, 133, 9, 15, 4, 16, 20, 2637, 648, 63, 1, 16, 4, 380, 3, 13, 15, 16, 3, 117, 240, 1, 18, 1, 2806, 24, 12, 9, 288, 18, 9, 6, 49, 8318, 4, 6, 1, 36, 4, 21, 44, 6, 4, 300, 4, 9, 97, 4, 2, 4, 104, 36, 19, 4, 144, 4, 221, 4, 4, 28, 1, 16, 1, 4, 27, 1272, 26, 4, 1, 6, 483, 26, 252, 36, 4, 12, 65, 184, 56, 2570, 166, 213, 30, 9, 18, 16, 2, 54, 7, 3, 276, 4, 15, 64, 6, 45, 36, 80, 48, 4, 60, 61, 26, 299, 44, 90, 39, 64, 1, 18, 3060, 7124, 6, 40, 64, 1001, 381, 4, 688, 9, 3544, 268, 1, 6, 443, 162, 2, 28, 18, 15, 36, 9, 18, 364, 63, 4, 2, 4, 20, 291, 192, 4, 39, 28, 2112, 18, 1350, 183, 88, 3, 16, 168, 4, 4, 91, 759, 25, 230, 256, 6, 8, 1, 2, 165, 2, 8, 112, 4, 1, 9, 15, 3, 36, 54, 9, 6, 4, 9, 54, 96, 20, 1, 8, 10, 20, 9, 348, 16, 24, 12, 25, 289, 1, 1, 18, 53, 8, 3, 1, 1, 2, 3, 8, 403, 48, 11, 9, 16, 12, 24, 272, 1, 1821, 338, 36, 4, 146, 176, 269, 3671, 1820, 4, 19, 199, 9, 9, 36, 100, 1, 25, 64, 24, 43758, 12, 12, 750, 9, 30, 6, 97, 3, 2, 45, 33, 103, 1056, 78, 10, 4, 24, 8, 196, 464, 52, 1, 83, 4, 6, 6, 12, 18, 4, 4, 15, 108, 704, 8, 14, 180, 48, 9, 99, 25, 314, 28, 60, 128, 21, 51, 4, 116, 642, 79, 18, 4, 8, 63, 119, 1, 4, 8, 24, 6, 1650, 10, 655, 4, 12, 260, 16, 6, 16, 488, 6, 1, 162, 37, 36, 6, 1, 1, 1, 6, 6, 1, 48, 222, 35, 2, 2976, 20, 3, 317, 4, 4, 29, 40, 36, 6, 4, 1, 5, 216, 72, 36, 251, 598, 174, 112, 52, 24, 16, 18, 6, 21, 4, 4, 18, 36, 9, 310, 4, 223, 4, 33, 77, 96, 15, 18, 4, 20, 18, 48, 1, 24, 1476, 300, 1806, 32, 608, 4, 233, 118, 16, 60, 62, 33, 156, 162, 42, 28, 9, 315, 168, 6, 1, 32, 4, 198, 18, 6, 18, 9, 16, 48, 8820, 241, 246, 4, 12, 264, 4, 282, 98, 84, 4, 100, 11, 104, 52, 244, 34, 4, 59, 8, 2, 4, 6, 12, 3, 120, 36, 60, 24, 93, 16, 1764, 26, 39, 6, 24, 237, 654, 38, 15, 450, 6, 239, 495, 8, 9, 48, 408, 6, 6806, 9, 16, 9, 18, 36, 42, 132, 90, 64, 72, 24, 4, 196, 49, 12, 32, 4, 16, 256, 9, 26, 1, 4, 56, 2, 140, 1548, 37, 330, 2, 1, 4, 379, 12, 191, 156, 31, 54, 72, 210, 728, 664, 35, 180, 49, 914, 1, 1, 201, 25, 65, 2, 200, 4, 13, 45, 145, 30, 4, 37, 19, 180, 16, 9, 13, 1, 240, 1, 84, 4, 1, 272, 16, 28, 19, 1636, 376, 232, 2, 78, 199, 4, 2, 4, 12, 95, 1313, 275, 750, 96, 24, 161, 2, 1, 15, 56, 8, 102, 63, 100, 4895, 3910, 80, 9, 84, 49, 30, 16, 28, 112, 6, 196, 50, 8, 3, 39, 19, 100, 18, 28, 1, 24, 4, 1, 13, 72, 98, 1, 36, 91, 1, 8, 15, 50, 15, 34, 3, 15, 90, 4, 80, 4, 30, 221, 400, 60, 4, 316, 29, 88, 4, 18, 924, 67, 6878, 48, 4, 1755, 521, 14, 400, 12, 2, 4, 100, 81, 230, 4, 15, 1, 250, 450, 4, 64, 30, 200, 98, 20, 4, 80, 4, 20, 4, 9, 8, 6, 49, 1, 228, 5320, 50, 9, 9, 4, 32, 4, 20, 3349, 1, 432, 1, 1206, 80, 9, 20, 24, 9, 81, 16, 378, 2, 4, 1001, 43, 133, 3, 147, 133, 8, 6, 41, 18, 135, 6, 476, 16, 1284, 48, 1, 144, 78, 48, 81, 2, 120, 18, 6, 4, 225, 15876, 10, 8, 468, 16, 25, 96, 18, 9, 144, 43, 8, 1, 42, 624, 408, 21, 90, 38, 466, 8, 102, 4, 1, 24, 142, 4, 20, 36, 50, 15, 156, 9, 6, 5264, 6, 9, 9, 36, 94, 98, 20, 4, 12, 1, 4, 1340, 4, 216, 6, 350, 309, 72, 137, 54, 126, 26, 4, 857, 1, 1, 1, 9, 18, 3, 9, 75, 4, 2, 16, 30, 15004, 16, 20, 15, 9, 16, 8, 71, 315, 8, 80, 4, 2, 1, 713, 2, 1, 4, 9, 89, 1, 32, 47, 4, 28, 142, 1296, 244, 16, 3, 9, 37, 20, 36, 20, 9, 104, 460, 6, 9, 36, 24, 47, 8, 24, 1135, 418, 39, 1, 9, 24, 12, 8, 341, 20, 78, 1910, 525, 36, 1324, 2024, 25, 1, 36, 36, 60, 506, 7, 38])


/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
