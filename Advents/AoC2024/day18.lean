import Advents.Utils
open Lean

namespace Day18

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day18.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

def exit := (70, 70)
def one := 1024
def texit := (6, 6)
def tone := 12

structure MS where
  f : Std.HashSet pos
  S : pos := (0, 0)
  tot : Nat := 0
  sz : Nat
  frontHistorians : Std.HashSet pos := {(0, 0)}
  visitedHistorians : Std.HashSet pos := {(0, 0)}
  frontCorrupted : Std.HashSet pos := f.filter fun (p, q) => p == 0 || q == sz
  visitedCorrupted : Std.HashSet pos := f.filter fun (p, q) => p == 0 || q == sz


def MS.available (ms : MS) (p : pos) : Bool :=
  (! ms.f.contains p) && 0 ≤ min p.1 p.2 && max p.1 p.2 ≤ ms.sz

def inputToMS (s : Array String) (init sz : Nat) : MS :=
  { f := (s.take init).foldl (fun h p =>
          if let [x, y] := p.getNats then h.insert (y, x) else panic "wrong input") ∅
    sz := sz }

def drawMS (ms : MS) : IO Unit := do
  IO.println s!"Steps: {ms.tot}"
  draw <| drawSparseWith (ms.visitedHistorians.union ms.f) (ms.sz + 1) (ms.sz + 1) (fun p =>
    if ms.f.contains p && ms.visitedCorrupted.contains p then "*" else
    if ms.f.contains p then "#" else if ms.visitedHistorians.contains p then "O" else "·")

#eval do
  let (dat, sz, ex) := (atest, tone, texit)
  let ms := inputToMS dat sz ex.1
  drawMS ms

abbrev nbs : Array pos := #[(0, 1), (0, - 1), (1, 0), (- 1, 0)]

def move (ms : MS) : MS :=
  let (newFront, newVisited) := ms.frontHistorians.fold (init := (∅, ms.visitedHistorians)) fun (hf, hv) p =>
    let new := nbs.filterMap fun n =>
      let pNew := p + n
      if ms.f.contains pNew || ! ms.available pNew then none else
      if !hv.contains pNew then some pNew else none
    (hf.insertMany new, hv.insertMany new)
  {ms with
    frontHistorians := newFront
    visitedHistorians := newVisited
    tot := ms.tot + 1 }

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) (one : Nat := one) : Nat := Id.run do
  let (sz, ex) := if dat.size ≤ 1000 then (tone, texit) else (one, exit)
  let mut ms := inputToMS dat sz ex.1
  while (! ms.visitedHistorians.contains ((ex.1, ex.2) : pos)) do
    ms := move ms
  ms.tot

#assert part1 atest == 22

solve 1 380

/-!
#  Question 2
-/

abbrev blockNbs : Array pos := nbs ++ #[(1, 1), (1, - 1), (- 1, 1), (- 1, 1)]

def moveBoth (ms : MS) : MS :=
  -- expanding the accessible locations
  let (newFrontH, newVisitedH) :=
    ms.frontHistorians.fold (init := (∅, ms.visitedHistorians)) fun (hf, hv) p =>
      let new := nbs.filterMap fun n =>
        let pNew := p + n
        if ms.f.contains pNew || ! ms.available pNew then none else
        if !hv.contains pNew then some pNew else none
    (hf.insertMany new, hv.insertMany new)
  -- expanding the inaccessible locations
  let (newFrontC, newVisitedC) :=
    ms.frontCorrupted.fold (init := (∅, ms.visitedCorrupted)) fun (hf, hv) p =>
      let new := blockNbs.filterMap fun n =>
        let pNew := p + n
        if (! ms.f.contains pNew) || hv.contains pNew then none else
        some pNew
    (hf.insertMany new, hv.insertMany new)
  {ms with
    frontHistorians := newFrontH
    visitedHistorians := newVisitedH
    frontCorrupted := newFrontC
    visitedCorrupted := newVisitedC
    tot := ms.tot + 1 }

def blocker (ms : MS) : MS :=
  let (newFront, newVisited) := ms.frontCorrupted.fold (init := (∅, ms.visitedCorrupted)) fun (hf, hv) p =>
    let new := blockNbs.filterMap fun n =>
      let pNew := p + n
      if (! ms.f.contains pNew) || hv.contains pNew then none else
      some pNew
    (hf.insertMany new, hv.insertMany new)
  {ms with
    frontCorrupted := newFront
    visitedCorrupted := newVisited
    tot := ms.tot + 1 }

#eval one / (2 ^ 9)

#eval show Elab.Term.TermElabM _ from do
  let dat := atest
  let (sz, ex) := (tone, texit)
  let dat ← IO.FS.lines input
  let one := 2 * one + one / 2 + one / (2 ^ 2) + one / (2 ^ 3) + one / (2 ^ 7) + one / (2 ^ 8) + 1
  IO.println <| dat[one]!
  let (sz, ex) := (one, exit)
  let mut ms := inputToMS dat sz ex.1
  --drawMS ms
  let mut vs := 0
  while (! ms.visitedHistorians.contains ((ex.1, ex.2) : pos)) || vs == ms.visitedHistorians.size do
    vs := ms.visitedHistorians.size
    ms := moveBoth ms
  guard <| ms.visitedHistorians.contains ((ex.1, ex.2) : pos)

  drawMS ms
  --IO.println ms.visited.toArray

def HorC (dat : Array String) (one : Nat) : Bool := Id.run do
    let ex := exit
    let mut ms := inputToMS dat one ex.1
    --let last : pos := let cs := dat[one - 1]!.getNats; (cs[0]!, cs[1]!)
    let newTarget : Std.HashSet pos :=
      ms.f.filter fun (p, q) => q == 0 || p == exit.1
      --(Std.HashSet.ofArray <| (Array.range (exit.1 - 1)).map (·.cast + 1, 0)).union
      --  (.ofArray <| (Array.range exit.1).map (exit.1, ·.cast)) --== 0 || p == exit.1

    --IO.println <| s!"position: {one - 1}, last: {dat[one - 1]!}, {ms.f.contains last}, next: {dat[one]!}, {ms.f.contains next}"
    --let newFront := ms.f.filter fun (p, q) => p == 0 || q == exit.1
    --ms := {ms with frontCorrupted := newFront, visitedCorrupted := newFront}
    --drawMS ms
    --dbg_trace ((newTarget.filter ms.visitedCorrupted.contains).isEmpty,
    --      (ms.visitedHistorians.contains ((exit.1, exit.2) : pos)))
    while ((newTarget.filter ms.visitedCorrupted.contains).isEmpty &&
          !(ms.visitedHistorians.contains ((exit.1, exit.2) : pos))) do
      --vs := ms.visitedCorrupted.size
      ms := moveBoth ms
    --guard <| ms.visited.contains ((ex.1, ex.2) : pos)
    return ms.visitedHistorians.contains (exit.1, exit.2)
    -- then IO.println "Historians" else IO.println "Corrupted"

#check Array.binSearch

partial
def search (fin : Nat) (cond : Nat → Bool) (st : Nat := 0) : Option Nat :=
  if st == fin then (if cond st then none else some st) else
  let mid := (st + fin) / 2
  dbg_trace mid
  if cond mid then search fin cond (mid + 1) else search mid cond st

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : pos :=
  let low := (search dat.size (HorC dat ·)).get!
  let cs := dat[low - 1]!.getNats
  (cs[0]!, cs[1]!)

#eval part2 atest --== ???
--#assert part2 atest == ???

--set_option trace.profiler true in solve 2 --(26, 50)  -- takes approximately 30s



#exit
eval show Elab.Term.TermElabM _ from do
  --let dat := atest
  --let (sz, ex) := (tone, texit)
  let dat ← IO.FS.lines input
  --let one := (dat.size - 1) / 2 + (dat.size - 1) / 3 + (dat.size - 1) / 40 - 1
  --let pathExists := one - 1
  --let pathDoesNotExist := one
  --let ex := exit
  --IO.println <| HorC dat 2958
  --let (sz, ex) := (0, exit)
  --let mut ms : MS := {f := {}, sz := 0}
    --(Std.HashSet.ofArray <| (Array.range (exit.1 - 1)).map (·.cast + 1, 0)).union
    --  (.ofArray <| (Array.range exit.1).map (exit.1, ·.cast)) --== 0 || p == exit.1
  --draw <| drawSparse newTarget (ex.1 + 1) (ex.1 + 1)
--#exit
  --ms := moveBoth ms
  --drawMS ms
  --if true then
  --let low := 1 --2956
  --let low := Array.binSearch (Array.range dat.size) 0 fun one _ => (HorC dat one).1
  let low := (search dat.size (HorC dat ·)).get!
  let cs := dat[low - 1]!.getNats
  IO.println (cs[0]!, cs[1]!)
  --for one in [low:low + 3] do
  ----for one in [2955:2970] do
  --  IO.println <| HorC dat one
#exit
    let mut con := 0
    --ms := inputToMS dat one ex.1
    --IO.println <| part1 dat one --pathExists
    ms := inputToMS dat one ex.1
    let newTarget : Std.HashSet pos :=
      ms.f.filter fun (p, q) => q == 0 || p == exit.1
    let last : pos := let cs := dat[one - 1]!.getNats; (cs[0]!, cs[1]!)
    let next : pos := let cs := dat[one]!.getNats; (cs[0]!, cs[1]!)
    --IO.println <| s!"position: {one - 1}, last: {dat[one - 1]!}, {ms.f.contains last}, next: {dat[one]!}, {ms.f.contains next}"
    --let newFront := ms.f.filter fun (p, q) => p == 0 || q == exit.1
    --ms := {ms with frontCorrupted := newFront, visitedCorrupted := newFront}
    --drawMS ms
    let mut vs := 0
    --dbg_trace ((newTarget.filter ms.visitedCorrupted.contains).isEmpty,
    --      (ms.visitedHistorians.contains ((exit.1, exit.2) : pos)))
    while ((newTarget.filter ms.visitedCorrupted.contains).isEmpty &&
          !(ms.visitedHistorians.contains ((exit.1, exit.2) : pos)) --do
          --||
          --vs == ms.visitedCorrupted.size
          ) do --&&
            --con ≤ 5000 do
      --vs := ms.visitedCorrupted.size
      ms := moveBoth ms
      con := con + 1
    --guard <| ms.visited.contains ((ex.1, ex.2) : pos)
    IO.print s!"{con} "
    if ms.visitedHistorians.contains (exit.1, exit.2) then IO.println "Historians" else IO.println "Corrupted"
    --if ! (newTarget.filter ms.visitedCorrupted.contains).isEmpty then IO.println "Corrupted" else IO.println "Historians"
    --drawMS ms

#exit

end Day18

-- 26,50 -- path found
-- 52,45 wrong
-- 26,66 wrong
