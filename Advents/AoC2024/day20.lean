import Advents.Utils
open Lean

namespace Day20

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day20.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure Race where
  grid : Std.HashSet pos
  front : Std.HashSet pos
  visited : Std.HashSet pos
  E : pos
  nbs : Std.HashSet pos
  cheat : Option pos := none

def path (r : Race) : Std.HashMap pos Nat := Id.run do
  let mut curr := r.E
  let mut dists := {(curr, 0)}
  let mut gr := r.grid.erase curr
  let tgt := r.front.toArray[0]!
  let nbs : Std.HashSet pos := {(0, 1), (0, - 1), (1, 0), (- 1, 0)}
  let mut con := 0
  while tgt != curr do
    --let p := gr.toArray[0]!
    con := con + 1
    for n in nbs do
      let newPos := curr + n
      if gr.contains newPos then
        gr := gr.erase newPos
        dists := dists.insert newPos (dists[curr]! + 1)
        curr := newPos
  return dists

def inputToRace (dat : Array String) : Race :=
  let fr := sparseGrid dat "S".contains
  let nbs : Std.HashSet pos := {(0, 1), (0, - 1), (1, 0), (- 1, 0)}
  {
    grid := sparseGrid dat "SE.".contains
    front := fr
    visited := fr
    E := (sparseGrid dat "E".contains).toArray[0]!
    nbs := nbs.fold (init := nbs) fun h p =>
      let sums := nbs.fold (init := nbs) fun h' p' => h'.insert (p + p')
      h.union sums |>.erase (0, 0)
  }

def drawRace (r : Race) : IO Unit := do
  let sz := if r.grid.size ≤ 1000 then 15 else 141
  match r.cheat, r.nbs.size with
    | none, 12 => IO.println s!"Not cheated yet"
    | some p, 4 => IO.println s!"Already cheated at {p}"
    | _, _ => panic "Did you update the neighbours?"
  draw <| drawSparse r.grid sz sz

instance : HMul Nat pos pos where hMul a p := (a * p.1, a * p.2)

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let r := inputToRace dat
  let path := path r
  let _chs : Std.HashSet pos :=
    {(-1, 1), (2, 0), (1, -1), (-1, -1), (0, 2), (0, -2), (-2, 0), (1, 1)}
  let nbs : Std.HashSet pos := {(0, 1), (0, - 1), (1, 0), (- 1, 0)}
  let S := r.front.toArray[0]!
  let noCheatDist := path[S]!
  let mut improve : Std.HashMap Nat Nat := ∅
  for (p, toE) in path do
    --if p.1 != 1 then continue
    for n in nbs do
      let pNew := p + n
      if path[pNew]?.isSome then continue
      let pNew := p + 2 * n
      match path[pNew]? with
        | none => continue
        | some newDistToE =>
          if toE ≤ newDistToE + 2 then continue
          improve := improve.alter (toE - newDistToE - 2) (some <| ·.getD 0 + 1)
          --IO.println s!"improve: {toE - newDistToE - 2} (newDistToE, toE) {(newDistToE, toE)} at {p} --> {pNew}"
    --let toE :=
  let big := improve.fold (init := 0) fun tot imp mult => if 100 ≤ imp then tot + mult else tot
  IO.println big
  --for (imp, mult) in improve.filter (fun (imp, mult) : Nat × Nat => (100 ≤ imp : Bool)) do
  --  IO.println s!"{mult} cheats improve by {imp} picoseconds"
  for (imp, mult) in improve.toArray.qsort (·.1 < ·.1) do
    IO.println s!"{mult} cheats improve by {imp} picoseconds"
  let _ : ToString Nat := {toString := (s!"{· % 10}")}
  draw <| drawHash path dat.size dat.size
  --drawRace r
  --IO.println s!"{r.nbs.toArray}"
  --draw <| drawSparse r.grid dat.size dat.size

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

end Day20
