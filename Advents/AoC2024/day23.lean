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

structure graph where
  edges : Std.HashSet (String × String)
  deriving Inhabited

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


/-
tc,td,wh

co,de,ta
co,ka,ta
de,ka,ta
qp,td,wh
tb,vc,wq
td,wh,yn
-/

namespace graph

def cliqueContaining (gr : graph) (e : String × String) : Std.HashSet String := Id.run do
  let st := e.1
  let mut fin := {st, e.2}
  for (_, f) in gr.edges.filter (·.1 == st)  do
    if gr.edges.contains (f, e.2) then
      fin := fin.insert <| f
  return fin

def cliqueContainingAnd (gr : graph) (e : String × String) : Std.HashSet String × graph := Id.run do
  let st := e.1
  let mut fin := {st, e.2}
  let mut leftGraph := gr.edges.erase e |>.erase (e.2, st)
  for new@(_, f) in gr.edges.filter (·.1 == st)  do
    if gr.edges.contains (f, e.2) then
      fin := fin.insert <| f
      leftGraph := leftGraph.erase new |>.erase (f, st)
  return (fin, ⟨leftGraph⟩)

def showClique (h : Std.HashSet String) : String :=
  ",".intercalate (h.toArray.qsort (· < ·)).toList

def valence (g : graph) (e : String) : Nat :=
  g.edges.filter (·.1 == e) |>.size

def vertices (g : graph) : Std.HashSet String :=
  g.edges.fold (init := ∅) fun h (e, f) => h.insert e |>.insert f

def vals (g : graph) : Std.HashMap String Nat :=
  g.vertices.fold (init := ∅) fun h v => h.insert v (g.edges.filter (·.1 == v)).size

def showVals (vs : Std.HashMap String Nat) : IO Unit := do
  for (e, v) in vs do
    IO.println s!"'{e}' connects to {v} vertices"

structure clique where
  gr : graph --Std.HashSet (String × String)
  cl : Std.HashSet String
  left : Std.HashSet String
  deriving Inhabited

def expandClique (c : clique) : Array clique := Id.run do
  let mut nextCliques := #[] --clique.cl
  for v in c.left do
    if (c.cl.toArray.map fun w => c.gr.edges.contains (w, v)).all id then
      --if (nextCliques.filter fun clique => clique.cl == c.cl.insert v).isEmpty then
      nextCliques := nextCliques.push ({c with cl := c.cl.insert v, left := c.left.erase v})
  return nextCliques

set_option trace.profiler true in
/-
ac,ei,gj,kh,li,lx,mz,oo,pr,sk,uh,wm
ez,fg,jt,kv,ng,nv,nx,oa,ph,uw,wh,xn
ez,fg,jt,kv,ng,nv,nx,oa,ph,uw,wh,xn
-/
/-

-/
eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut gr := inputToGraph dat
  --let verts := gr.vertices
  --IO.println s!"There are {verts.size} vertices and {(verts.size * (verts.size - 1)) / 2} possible edges.\nThere are {gr.edges.size} edges"
  for v in gr.vertices do
    IO.println s!"** {v}"
    let cl : clique := {gr := gr, cl := {v}, left := gr.vertices.erase v}
    let cls := expandClique cl
    for next in cls do
      let cl2 := expandClique next
      let sz := cl2.size
      if sz != 10 then
        IO.println s!"{showClique next.cl}: {sz}"
/-!
-/

def edgNs (g : graph) (h : Std.HashSet String) : Nat :=
  (g.edges.filter fun (a, b) => h.contains a && h.contains b).size / 2
#eval 13 * 7
/-
set_option trace.profiler true in
eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let mut gr := inputToGraph dat
  --let verts := gr.vertices
  --IO.println s!"There are {verts.size} vertices and {(verts.size * (verts.size - 1)) / 2} possible edges.\nThere are {gr.edges.size} edges"
  for v in gr.vertices do
    IO.println s!"** {v}"
    let cl : clique := {gr := gr, cl := {v}, left := gr.vertices.erase v}
    let cls := expandClique cl
    let nbs := cls.foldl (fun (h : Std.HashSet String) (c : clique) => h.union c.cl) (∅ : Std.HashSet String)
    IO.println s!"vertices: {nbs.size}, edges: {edgNs gr nbs}"
    --IO.println <| (expandClique {cl with cl := nbs}).size
    --if cls.size != 0 then  IO.println s!"** {v} {cls.size}"
    --for next in cls do
    --  let cl2 := expandClique next
    --  let sz := cl2.size
    --  if sz == 11 then
    --    IO.println s!"{showClique next.cl}: {sz}"
-/

/-!
-/

def getNbs (g : graph) (h : Std.HashSet String) : Std.HashSet String :=
  g.edges.fold (fun nbs (a, b) => if h.contains a then nbs.insert b else nbs) ∅

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) (param : Nat := 37) : String := Id.run do
  let mut gr := inputToGraph dat
  let mut nbs13 := gr.vertices
  for v in gr.vertices do
    let nbs := getNbs gr (getNbs gr {v})
    if nbs.size ≤ param then
      nbs13 := nbs13.filter nbs.contains
  showClique nbs13

#eval part2 atest 9 --== "co,de,ka,ta"
#assert part2 atest 17 == "co,de,ka,ta"

set_option trace.profiler true in solve 2 "cf,ct,cv,cz,fi,lq,my,pa,sl,tt,vw,wz,yd"
#exit
set_option trace.profiler true in
#eval do
  let dat ← IO.FS.lines input
  let mut gr := inputToGraph dat
  let mut nbs13 := gr.vertices
  for v in gr.vertices do
    let nbs := getNbs gr (getNbs gr {v})
    if nbs.size ≤ 37 then
      nbs13 := nbs13.filter nbs.contains
  IO.println <| showClique nbs13
#exit
  let c : clique := {
    gr := gr
    cl := {"cf","ct","cv","cz","fi","lq","my","pa","sl","tt","vw","wz","yd"}
    left := gr.vertices
  }
  IO.println <| showClique c.cl
  IO.println <| edgNs gr c.cl
  let nbs := getNbs gr c.cl
  IO.println <| nbs.size
  let nbs := getNbs gr nbs
  IO.println <| nbs.size
  let nbs := getNbs gr nbs
  IO.println <| nbs.size
  let nbs := getNbs gr nbs
  IO.println <| nbs.size
  let nbs := getNbs gr nbs
  IO.println <| nbs.size
  --let cl2 := expandClique c -- empty array
  --IO.println <| cl2.size
  --IO.println <| showClique cl2.cl
#eval Nat.factors (520 - 26)
#eval Nat.factors (181 - 26)
#exit
  for v in gr.vertices do -- ["ct"] do --
    --IO.println s!"** {v}"
    let nbs : Std.HashSet String :=
      gr.edges.fold (fun h (e, f) => if e == v then h.insert f else h) ∅

    if edgNs gr (nbs.insert v) == 79 then
      IO.println <| showClique <| nbs.insert v
      --IO.println s!"{edgNs gr (nbs.insert v)}"

#eval
  (Array.range 4).map fun i' => let i := i' + 12; (i, (i * (i - 1)) / 2)

#exit
--set_option trace.profiler true in
#eval do
  --let dat := atest
  let dat ← IO.FS.lines input
  let mut gr := inputToGraph dat
  --let verts := gr.vertices
  --IO.println s!"There are {verts.size} vertices and {(verts.size * (verts.size - 1)) / 2} possible edges.\nThere are {gr.edges.size} edges"
  for v in gr.vertices do -- ["ct"] do --
    IO.println s!"** {v}"
    --let cl : clique := {gr := gr, cl := {v}, left := gr.vertices.erase v}
    --let cls := expandClique cl
    --let nbs := cls.foldl (fun (h : Std.HashSet String) (c : clique) => h.union c.cl) (∅ : Std.HashSet String)
    let nbs : Std.HashSet String :=
      gr.edges.fold (fun h ((e, f) : String × String) => if e == v then h.insert f else h) {v}
    IO.println nbs.toArray
    IO.println s!"{edgNs gr nbs}"
    --IO.println <| (expandClique {cl with cl := nbs}).size
    --if cls.size != 0 then  IO.println s!"** {v} {cls.size}"
    --for next in cls do
    --  let cl2 := expandClique next
    --  let sz := cl2.size
    --  if sz == 11 then
    --    IO.println s!"{showClique next.cl}: {sz}"


#exit
  let cl : clique := {gr := gr, cl := {"ng"}, left := gr.vertices}
  let con := 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  IO.println <| showClique cl.cl
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  IO.println <| showClique cl.cl
--#exit
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  let con := con + 1
  let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  let con := con + 1
  --let cl := let new := expandClique cl; dbg_trace (con, new.size); new[0]!
  --let cl := (expandClique cl)[0]!
  --IO.println cl.cl.size
  IO.println <| showClique cl.cl
  --for c in cl.cl do
  --  IO.println c
#exit

set_option trace.profiler true in
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let mut gr := inputToGraph dat
  let verts := gr.vertices
  --IO.println verts.size
  let cl : clique := {gr := gr, cl := {}, left := gr.vertices}
  let ex := expandClique cl |>.flatMap expandClique
  let ex := ex.foldl (init := (#[] : Array clique)) fun (h : Array clique) (c : clique) =>
    if (h.filter fun (cliqu : clique) => cliqu.cl == c.cl).isEmpty then h.push c else h
  let ex := ex.flatMap expandClique
  let ex := ex.foldl (init := (#[] : Array clique)) fun (h : Array clique) (c : clique) =>
    if (h.filter fun (cliqu : clique) => cliqu.cl == c.cl).isEmpty then h.push c else h
  let ex := ex.flatMap expandClique
  let ex := ex.foldl (init := (#[] : Array clique)) fun (h : Array clique) (c : clique) =>
    if (h.filter fun (cliqu : clique) => cliqu.cl == c.cl).isEmpty then h.push c else h
  IO.println ex.size
  --for c in ex do
  --  IO.println c.cl.toArray
  --let vs := vals gr
  --showVals vs
#exit
/-!
-/
/-
there are 11011 cliques with at least 3 vertices(?).  Found in [1171.946153]s
-/
  while !gr.edges.isEmpty do
    let next := gr.edges.toArray[0]!
    let (cl, rest) := cliqueContainingAnd gr next
    IO.println <| showClique cl
    gr := rest
  --IO.println <| showClique (cliqueContaining gr ("ka", "co"))


end Day23
