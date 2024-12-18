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
  front : Std.HashSet pos := {(0, 0)}
  visited : Std.HashSet pos := {(0, 0)}
  sz : Nat

def MS.available (ms : MS) (p : pos) : Bool :=
  (! ms.f.contains p) && 0 ≤ min p.1 p.2 && max p.1 p.2 ≤ ms.sz

def inputToMS (s : Array String) (init sz : Nat) : MS :=
  { f := (s.take init).foldl (fun h p =>
          if let [x, y] := p.getNats then h.insert (y, x) else panic "wrong input") ∅
    sz := sz }

def drawMS (ms : MS) : IO Unit := do
  IO.println s!"Steps: {ms.tot}"
  draw <| drawSparseWith (ms.visited.union ms.f) (ms.sz + 1) (ms.sz + 1) (fun p =>
    if ms.f.contains p && ms.visited.contains p then "*" else
    if ms.f.contains p then "#" else if ms.visited.contains p then "O" else "·")

#eval do
  let (dat, sz, ex) := (atest, tone, texit)
  let ms := inputToMS dat sz ex.1
  drawMS ms

abbrev nbs : Array pos := #[(0, 1), (0, - 1), (1, 0), (- 1, 0)]

def move (ms : MS) : MS :=
  let (newFront, newVisited) := ms.front.fold (init := (∅, ms.visited)) fun (hf, hv) p =>
    let new := nbs.filterMap fun n =>
      let pNew := p + n
      if ms.f.contains pNew || ! ms.available pNew then none else
      if !hv.contains pNew then some pNew else none
    (hf.insertMany new, hv.insertMany new)

  {ms with
    front := newFront
    visited := newVisited
    tot := ms.tot + 1 }

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) (one : Nat := one) : Nat := Id.run do
  let (sz, ex) := if dat.size ≤ 1000 then (tone, texit) else (one, exit)
  let mut ms := inputToMS dat sz ex.1
  while (! ms.visited.contains ((ex.1, ex.2) : pos)) do
    ms := move ms
  ms.tot

#assert part1 atest == 22

solve 1 380

/-!
#  Question 2
-/

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
  while (! ms.visited.contains ((ex.1, ex.2) : pos)) || vs == ms.visited.size do
    vs := ms.visited.size
    ms := move ms
  guard <| ms.visited.contains ((ex.1, ex.2) : pos)

  drawMS ms
  --IO.println ms.visited.toArray

abbrev blockNbs : Array pos := nbs ++ #[(1, 1), (1, - 1), (- 1, 1), (- 1, 1)]

def blocker (ms : MS) : MS :=
  let (newFront, newVisited) := ms.front.fold (init := (∅, ms.visited)) fun (hf, hv) p =>
    let new := blockNbs.filterMap fun n =>
      let pNew := p + n
      if (! ms.f.contains pNew) || hv.contains pNew then none else
      some pNew
    (hf.insertMany new, hv.insertMany new)
  {ms with
    front := newFront
    visited := newVisited
    tot := ms.tot + 1 }


#eval show Elab.Term.TermElabM _ from do
  --let dat := atest
  --let (sz, ex) := (tone, texit)
  let dat ← IO.FS.lines input
  let one := (dat.size - 1) / 2 + (dat.size - 1) / 3 + (dat.size - 1) / 40 - 1
  let pathExists := one - 1
  let pathDoesNotExist := one
  let (sz, ex) := (one, exit)
  IO.println <| part1 dat pathExists
  let mut ms := inputToMS dat sz ex.1
  let last : pos := let cs := dat[one - 1]!.getNats; (cs[0]!, cs[1]!)
  let next : pos := let cs := dat[one]!.getNats; (cs[0]!, cs[1]!)
  IO.println <| s!"position: {one - 1}, last: {dat[one - 1]!}, {ms.f.contains last}, next: {dat[one]!}, {ms.f.contains next}"
  let newFront := ms.f.filter fun (p, q) => p == 0 || q == exit.1
  let newTarget := ms.f.filter fun (p, q) => q == 0 || p == exit.1
  ms := {ms with front := newFront, visited := newFront}
  drawMS ms
  let mut vs := 0
  while (newTarget.filter ms.visited.contains).isEmpty || vs == ms.visited.size do
    vs := ms.visited.size
    ms := blocker ms
  --guard <| ms.visited.contains ((ex.1, ex.2) : pos)

  drawMS ms
#exit

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day18

-- 26,50 -- path found
-- 52,45 wrong
-- 26,66 wrong
