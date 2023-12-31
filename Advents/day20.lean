import Advents.Utils
open Lean (HashMap)

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day20.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test1` is the first test string for the problem. -/
def test1 := "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a"

/-- `atest` is the first test string for the problem, split into rows. -/
def atest := (test1.splitOn "\n").toArray

/-- `test2` is the second test string for the problem. -/
def test2 := "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output"

/-- `btest` is the second test string for the problem, split into rows. -/
def btest := (test2.splitOn "\n").toArray

/-- A `module` is a container with 3 constructors
* `ff`, a *flip-flop*   module -- a single `Bool`ean;
* `cj`, a *conjunction* module -- an array of pairs `String × Bool`;
* `b`,  a *broadcaster* module -- an array of strings.
-/
inductive module
  | ff : Bool → module
  | cj : Array (String × Bool) → module
  | b  : Array String → module
  deriving Inhabited, Repr, BEq, DecidableEq

open module

/-- Pretty printing for `module`. -/
instance : ToString module where toString
  | .ff tf => s!"ff: {tf}"
  | .cj ar => s!"cj: {ar}"
  | .b  ar => s!"b: {ar}"

/-- returns `(source, type of source, targets)` -/
def parseOne (s : String) : String × Char × Array String :=
  match (s.splitOn " -> ") with
    | [l, r] =>
      let (nm, c) :=
        if "broadcaster" = l then
          ("broadcaster", '@')
        else
          (l.drop 1, l.front)
      (nm, c, (r.splitOn ", ").toArray)
    | _ => dbg_trace s!"parseOne error on '{s}'"; default

/-- records all the connections between modules and their types. -/
def grid (dat : Array String) : HashMap String (Char × Array String) :=
  Id.run do
  let mut st : HashMap String (Char × Array String) := .empty
  for d in dat do
    let (nm, c, tgts) := parseOne d
    st := st.insert nm (c, tgts)
  return st

/-- returns the HashMap sending a module `s` to the array of modules having `s` as target. -/
def getSrcs (dat : Array String) : HashMap String (Array String) :=
  Id.run do
  let mut y : HashMap String (Array String) := .empty
  for (s, _, d) in grid dat do
    for t in d do
      let srcs := (y.find? t).getD #[]
      y := y.insert t (srcs.push s)
  return y

#assert (getSrcs atest).find? "c" == #["b", "broadcaster"]
#assert (getSrcs atest).find? "broadcaster" == none

/-- returns the HashMap sending a module `s` to the array of modules having `s` as source. -/
def getTgts (gr : HashMap String (Char × Array String)) : HashMap String (Array String) :=
  Id.run do
  let mut x := .empty
  for (s, _, a) in gr do x := x.insert s a
  return x

#assert (getTgts (grid atest)).find! "broadcaster" == #["a", "b", "c"]
#assert (getTgts (grid (← IO.FS.lines input))).find! "broadcaster" == #["fb", "xk", "gr", "vj"]

/-- `init dat` takes as input an array `dat` of strings.
It returns the initial grid, according to the layout in `dat`. -/
def init (dat : Array String) : HashMap String (module × Option Bool) :=
  let srcs := getSrcs dat
  Id.run do
  let mut st : HashMap String (module × Option Bool) := .empty
  for (nm, c, tgts) in dat.map parseOne do
    match c with
      | '@' => st := st.insert nm (.b tgts, false)
      | '%' => st := st.insert nm (.ff false, none)
      | '&' =>
        let cons := (srcs.find? nm).getD #[]
        st := st.insert nm (.cj <| cons.map (Prod.mk · false), false)
      | _ => dbg_trace "init error"
  return st

/-- processes a single `pulse` to the module `s`. It returns the modified state. -/
def pulseOne (srcs' : HashMap String (Array String)) (st : HashMap String (module × Option Bool))
    (pulse : Bool) (s : String) :
    HashMap String (module × Option Bool) × Array (String × Bool) :=
  match st.find? s with
    --  process flip-flop, uses the input `pulse`
    | some (.ff tf, _sgn) =>
      if ! pulse then
        (st.insert s (.ff (!tf), some (!tf)), #[(s, !tf)])
      else (st, #[])
    --  process conjunction
    | some (.cj ar, _sgn) =>
      let srcs := (srcs'.find? s).getD #[]
      let news := srcs.zipWith ar (fun nm a =>
        match st.find? nm with
          | none => a
          | some (_, tf) => (a.1, tf.getD a.2))
      let signal := news.any (! Prod.snd ·)
      (st.insert s (.cj news, signal), #[(s, signal)])
    --  process broadcaster: this should not happen
    | some (.b _ar, _sgn) =>
      dbg_trace "pulseOne error: you should not be here"; default
    --  process everything else: this should only happen for leaf modules
    | none =>
      if s ∈ ["output", "rx"] then (st, #[]) else
        dbg_trace "pulseOne received none? {s}"; (st, #[])

/-- `onePush dat st` takes as input an array `dat` of strings and a module configuration `st`.
Given the layout determined by `dat`, `onePush` returns the configuration obtained from `st`
as a consequence of pushing the button once.
The configuration `st` records, besides the module associated with each string, also
the "last emitted signal", following the convention:
* a *high* pulse is `some true`;
* a *low* pulse is `some false`;
* no pulse is `none`.
-/
def onePush (dat : Array String) (st : HashMap String (module × Option Bool))
    (ct : Nat × Nat) (v? : Bool := false) :
    HashMap String (module × Option Bool) × (Nat × Nat):=
  let gr := grid dat
  let tgts := getTgts gr
  let srcs := getSrcs dat
  Id.run do
  let mut str := ""
  if v? then for s in st do str := str ++ s!"{s}\n"
             dbg_trace str
  let mut next := st
  let mut con := 0
  let mut queue := #[("broadcaster", false)]
  let mut hl := ct + (0, 1)  -- count from wherever we start, plus the extra `low` impulse of `button`
  while queue.size ≠ 0 do
    if v? then dbg_trace s!"* Step {con}: process {queue}\n"
    let (curr, signal) := queue.back
    let nbs := (tgts.find? curr).getD #[]
    queue := queue.pop
    let sig := if signal then "high" else "low"
    for ns in nbs do
      let (next1, newTgs) := pulseOne srcs next signal ns
      next := next1
      queue := queue ++ newTgs
      hl := hl + if signal then (1, 0) else (0, 1)
      if v? then dbg_trace "# {curr} → '{sig}' → '{ns}' -- (high, low) = {hl}\n"
    con := con + 1
  (next, hl)

/-- `pushN dat n` returns the effect of pushing the button `n` times on the configuration
determined by `dat`. -/
def pushN (dat : Array String) (n : Nat := 1000) (ct : Nat × Nat := default) (v? : Bool := false) : Nat :=
  Id.run do
    let mut fin := init dat
    let mut hl := ct
    for i in [:n] do
      if v? then dbg_trace "*** PUSH {i+1} ***\n"
      if v? ∧ i ≤ 3 then
        (fin, hl) := onePush dat fin hl v?
      else
        (fin, hl) := onePush dat fin hl
    if v? then dbg_trace hl
    return hl.1 * hl.2

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := pushN dat

#assert part1 atest == 32000000
#assert part1 btest == 11687500

/- an example of verbose printing -- emulates the text of the question
#eval do
  let _dat ← IO.FS.lines input
  let _dat := btest
  return pushN _dat 4 (v? := true)
-/

solve 1 730797576
