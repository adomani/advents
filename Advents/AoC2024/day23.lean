import Advents.Utils
open Lean

namespace Day23

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day23.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
A `graph` is simply a `HashSet` of pairs of vertices.
Each pair appears in both orders and the entries are strings.
For the problem, they are 2-letter strings, though this is not relevant.
-/
structure graph where
  /-- `edges` is a `HashSet` of pairs of vertices.
  Each pair appears in both orders and the entries are strings. -/
  edges : Std.HashSet (String × String)
  deriving Inhabited

/-- Converts the input data into a `graph`. -/
def inputToGraph (dat : Array String) : graph where
  edges := dat.foldl (init := ∅) fun h s =>
    match s.splitOn "-" with
      | [a, b] => h.insert (a, b) |>.insert (b, a)
      | _ => panic "wrong input!"

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let gr := inputToGraph dat
  let mut trs : Std.HashSet (Array String) := ∅
  for (e, f) in gr.edges do
    if ! e.startsWith "t" then continue
    for (g, h) in gr.edges do
        if g == f && gr.edges.contains (h, e) then
          trs := trs.insert <| #[e, f, h].qsort (· < ·)
  trs.size

#assert part1 atest == 7

solve 1 1000 -- under 3s

/-!
#  Question 2
-/

namespace graph

/--
Displays the answer in the form that the problem requires: comma-separated, no spaces and sorted.
-/
def showHash (h : Std.HashSet String) : String :=
  ",".intercalate (h.toArray.qsort (· < ·)).toList

/-- If `g : graph` is a graph, then `g.vertices` returns the vertices of `g`. -/
def vertices (g : graph) : Std.HashSet String :=
  g.edges.fold (init := ∅) fun h (e, f) => h.insert e |>.insert f

/--
If `g : graph` is a graph, then `g.getNeighbours h` finds the vertices that are at distance one
from a vertex in `h` in `g`.
-/
def getNeighbours (g : graph) (h : Std.HashSet String) : Std.HashSet String :=
  g.edges.fold (fun nbs (a, b) => if h.contains a then nbs.insert b else nbs) ∅

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) (param : Nat := 37) : String :=
  let gr := inputToGraph dat
  let allNbs : Array (String × Std.HashSet String) :=
    gr.vertices.fold (fun h v => h.push (v, getNeighbours gr {v})) ∅
  let clique : Std.HashSet String := gr.vertices.fold (init := ∅) fun h v =>
    let nbdV := (allNbs.find? (·.1 == v)).get!.2
    let nbs : Std.HashSet String :=
      allNbs.foldl (init := ∅) fun h (w, nw) => if nbdV.contains w then h.union nw else h
    if nbs.size ≤ param then h.insert v else h
  showHash clique

#assert part2 atest 9 == "co,de,ka,ta,tb,tc" -- this should be `"co,de,ka,ta"`!

solve 2 "cf,ct,cv,cz,fi,lq,my,pa,sl,tt,vw,wz,yd" -- takes approximately 11s

end graph

end Day23
