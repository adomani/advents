import Advents.Utils
import Batteries.Data.List.Basic

namespace Day19

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day19.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- A `part` is a container for `xmas`. -/
structure part where
  /-- The `s` in `xmas`. -/
  x : Nat
  /-- The `m` in `xmas`. -/
  m : Nat
  /-- The `a` in `xmas`. -/
  a : Nat
  /-- The `s` in `xmas`. -/
  s : Nat
  deriving Inhabited, Repr, DecidableEq, BEq

/-- Representing a `part` as a string. -/
instance : ToString part where
  toString p := s!"\{ x = {p.x}, m = {p.m}, a = {p.a}, s = {p.s} }"

/-- `parseData pts` parses the array `pts` of strings,
interpreting each entry as a `part`. -/
def parseData (pts : Array String) : Array part :=
  pts.map parseDataOne where
    /-- `parseDataOne s` is an auxilliary function to `parseData`:
    it is the parsing of each individual data line. -/
    parseDataOne (s : String) : part :=
      match (((s.drop 1).dropRight 1).splitOn ",").map String.toList with
        | ['x'::'='::xs, 'm'::'='::ms, 'a'::'='::as, 's'::'='::ss] =>
          { x := String.toNat! ⟨xs⟩
            m := String.toNat! ⟨ms⟩
            a := String.toNat! ⟨as⟩
            s := String.toNat! ⟨ss⟩ }
        | y => dbg_trace s!"parseDataOne error {y}"; default

#assert parseData.parseDataOne "{x=2127,m=1623,a=2188,s=1013}" ==
  { x := 2127
    m := 1623
    a := 2188
    s := 1013 }

/-- `parseWFOne s p` parses one individual instruction `s` inside a workflow
and returns its value on `p`.
The output is
* `some str` if `s` accepts `p` and maps it to workflow `str`
  (including `R` and `A` as workflows) and
* `none` if `s` rejects `p`.
-/
def parseWFOne (s : String) (p : part) : Option String :=
  if ! s.contains ':' then some s else
  match s.toList with
    | ['A'] => some "A"
    | ['R'] => some "R"
    | c::lt::xs =>
      let pt : Nat := match c with
        | 'x' => p.x
        | 'm' => p.m
        | 'a' => p.a
        | 's' => p.s
        | _ => dbg_trace s!"WF error {s}"; default
      let (val, tgt) := match xs.splitOnP (· == ':') with
        | [v, t] => ((String.mk v).toNat!, String.mk t)
        | y => dbg_trace s!"parseWF error {y}"; default
      let rel : Bool := (if lt == '<' then (pt < val) else (pt > val))
      if rel then some tgt else none
    | y => dbg_trace s!"parseWF0 error {y}"; some s

/-- `parseWF s p` parses the single workflow line `s` and
evaluates it on the part `p`. -/
def parseWF (s : String) (p : part) : List (Option String) :=
  let dat := (((s.dropWhile (! · == '{')).drop 1).dropRight 1).splitOn ","
  dat.map (parseWFOne · p)

/-- `valPart dat pt` takes as input an array `dat` of strings and a part `pt`.
It returns the value of the part, according to the workflow rules in `dat`.
* If the workflow rejects `pt` (i.e. it returns `R`), then the value is 0.
* If the workflow accepts `pt` (i.e. it returns `A`), then the value is the
  sum of the `xwmas` entries of `pt`.
-/
def valPart (dat : Array String) (pt : part) : Nat :=
  let instrs := dat.takeWhile (! · == "")
  let AorR := Id.run do
    let mut wf := "in"
    while wf != "A" ∧ wf != "R" do
      let init := (instrs.find? (fun s => (wf.push '{').isPrefixOf s)).get!
      let instr := (parseWF init pt).reduceOption
      wf := instr[0]!
    return wf
  match AorR with
    | "A" => pt.x + pt.m + pt.a + pt.s
    | "R" => 0
    | y => dbg_trace s!"valPart? {y}"; default

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let ptsStx := dat.filter (String.front · == '{')
  let dataParts := parseData ptsStx
  let res := dataParts.map (valPart dat)
  res.sum

#assert part1 atest == 19114

solve 1 342650

/-!
#  Question 2
-/

/-- `icc` is an interval, encoded by its two endpoints. -/
structure icc where
  /-- `icc.l` is the left endpoint of an interval. -/
  (l : Nat)
  /-- `icc.r` is the right endpoint of an interval. -/
  (r : Nat)
  deriving Repr, BEq, DecidableEq

/-- The `default` element of an interval is `[1, 4000]`,
the largest range of values. -/
instance : Inhabited icc := ⟨1, 4000⟩

/-- A convenience instance, to pretty-print an interval. -/
instance : ToString icc where
  toString i := s!"[{i.l}, {i.r}]"

/-- The whole available interval, from `1` to `4000`. -/
def icc.all  : icc := default

/-- The empty interval, represented as the interval from `1` to `0`. -/
def icc.none : icc := ⟨1, 0⟩

/--  Adding two intervals returns the intersection of the two.
If the intersection is empty, the it returns `icc.none`. -/
instance : Add icc where
  add a b :=
    let nl := max a.l b.l
    let nr := min a.r b.r
    if nr < nl then .none else ⟨nl, nr⟩

/-- `xmasWt` is a structure containing four values, `x`, `m`, `a`, `s`,
all of type `Nat`. -/
structure xmasWt where
  /-- The `s` in `xmas`. -/
  (x : icc)
  /-- The `m` in `xmas`. -/
  (m : icc)
  /-- The `a` in `xmas`. -/
  (a : icc)
  /-- The `s` in `xmas`. -/
  (s : icc)
  deriving Inhabited, Repr, DecidableEq, BEq

/-- Representing a `xmasWt` as a string. -/
instance : ToString xmasWt where
  toString p := s!"\{ x = {p.x}, m = {p.m}, a = {p.a}, s = {p.s} }"

/-- We can add two `xmas` weights. -/
instance : Add xmasWt where
  add w z := { x := w.x + z.x
               m := w.m + z.m
               a := w.a + z.a
               s := w.s + z.s }

/-- `Char.Xmk c p` creates the `xmas` weight with interval `p` at `c`. -/
def _root_.Char.Xmk (c : Char) (p : icc) : xmasWt :=
  match c with
    | 'x' => { x := p
               m := default
               a := default
               s := default }
    | 'm' => { x := default
               m := p
               a := default
               s := default }
    | 'a' => { x := default
               m := default
               a := p
               s := default }
    | 's' => { x := default
               m := default
               a := default
               s := p }
    | _ => default

/-- `xmasWt.update w c p` adds the interval `p` to the `xmas` weight `w`
in the position `c`. -/
def xmasWt.update (w : xmasWt) (c : Char) (p : icc) : xmasWt :=
  w + c.Xmk p

/-- `parseAlone s` takes as input a string `s` and returns sufficient data
for producing the workflow:
* the relevant `xmas` character;
* the two complementary intervals determined by the inequality;
* the target string for the rest of the workflow.
-/
def parseAlone (s : String) : Char × (icc × icc) × String :=
  if ! s.contains ':' then ('@', default, s) else
  match s.toList with
    | c::lt::xs =>
      let (val, tgt) := match xs.splitOnP (· == ':') with
        | [v, t] => ((String.mk v).toNat!, String.mk t)
        | y => dbg_trace s!"parseWF error {y}"; default
      let int : icc × icc := match lt with
        | '<' => (⟨0, val - 1⟩, ⟨val, 4000⟩)
        | '>' => (⟨val + 1, 4000⟩, ⟨0, val⟩)
        | y => dbg_trace s!"parseWF0 error {y}"; (.none, .none)
      (c, int, tgt)
    | y => dbg_trace s!"parseWF0 error {y}"; ('#', default, s)

/-- `getInstOne s` takes as input a string `s` representing a workflow line.
It returns the list of strings containing the instructions for the workflow. -/
def getInstOne (s : String) : List String :=
  (((s.dropWhile (! · == '{')).drop 1).dropRight 1).splitOn ","

/-- `workflowOneLine s xm` takes a list `s` of parsed instructions representing
a line in the workflow and an `xmas` wegiht `xm`.
It returns all possible outcomes of the workflow, each with second coordinate
referring to the string that will need to do the rest of the workflow. -/
def workflowOneLine (s : List (Char × (icc × icc) × String)) (xm : xmasWt) :
    Array (xmasWt × String) :=
  Id.run do
  let mut ints := #[]
  let mut xm := xm
  for (c, (i1, i2), tgt) in s do
    ints := ints.push (xm.update c i1, tgt)
    xm := xm.update c i2
  return ints

/--  The `wt` of an `icc` interval the number of its integer points. -/
def icc.wt (i : icc) : Nat := i.r - (i.l - 1)

/--  The `wt` of a `wmasWt` is the product of the weights of
all its constituent intervals. -/
def xmasWt.wt (xs : xmasWt) : Nat :=
  ([xs.x, xs.m, xs.a, xs.s].map icc.wt).prod

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  let instrs := dat.takeWhile (! · == "")
  Id.run do
  let mut accepted := #[]
  let mut toContinue : Array (xmasWt × String) := #[(default, "in")]
  while toContinue.size != 0 do
    let mut newCont := #[]
    for (xm, wf) in toContinue do
      let init := (instrs.find? (fun s => (wf.push '{').isPrefixOf s)).get!
      let instr := (getInstOne init).map parseAlone
      let newInts := workflowOneLine instr xm
      let (toCont, acc) := newInts.partition (!Prod.snd · == "A")
      accepted := accepted ++ acc
      let toCont := toCont.filter (!Prod.snd · == "R")
      newCont := newCont ++ toCont
    toContinue := newCont
  return (accepted.map ((xmasWt.wt ∘ Prod.fst) ·)).sum

#assert part2 atest == 167409079868000

solve 2 130303473508222
