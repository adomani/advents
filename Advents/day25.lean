import Advents.Utils
open Lean

namespace Day25

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day25.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `wires` is the type containing all (ordered) pairs of `String`s that correspond
to wires in the graph of today's puzzle. -/
abbrev wires := Std.HashSet (String × String)

/-- `getStringsOne1 s` takes as input a string `s` and returns the ordered pair
consisting of the string on the left of the input and the array of strings on the right. -/
def getStringsOne1 (s : String) : String × Array String :=
  match s.splitOn " " with
    | a::as => (a.dropRight 1, as.toArray)
    | _ => dbg_trace "oh no!"; default

/-- `getStringsOne s` takes as input a string `s` and returns the list of ordered pairs
consisting of the string on the left and each one of the strings on the right. -/
def getStringsOne (s : String) : List (String × String) :=
  match s.splitOn " " with
    | a::as => as.map (Prod.mk (a.dropRight 1)) ++ as.map (Prod.mk · (a.dropRight 1))
    | _ => dbg_trace "oh no!"; default

/-- `getWires dat` converts the array of strings that is the input to a term of
type `wires`, encoding the whole graphs. -/
def getWires (dat : Array String) : wires :=
  .ofList (dat.toList.map getStringsOne).flatten

/-- `getVerts dat` returns the array of vertices of the graph determined by the input. -/
def getVerts (dat : Array String) : Array String :=
  let many := (dat.map fun x => let (a, as) := getStringsOne1 x; as.push a)
  (many.foldr (· ++ ·) #[]).sortDedup

/-- prints an output that can be directly pasted to the command-line:
it uses `dot` to visualize the graph encoded by `dat`. -/
def printGraph (dat : Array String) (name : String := "test_graph") : IO Unit := do
  let w := getWires dat
  let vs := getVerts dat
  IO.println s!"echo 'digraph \{ "
  for v1 in vs do
    for v2 in vs do
      if v1 < v2 ∧ w.contains (v1, v2) then IO.println s!"  {v1} -> {v2}"
  IO.println s!"}' | dot -Tgv > {name}.gv"

/-
#eval do
  let dat ← IO.FS.lines input
  printGraph dat "Day25_graph"

awk -F'"' '/[^[]pos/ {
  split($2, coos, ",")
  if (coos[1] < 28610) { bef++ } else { aft++ }
} END {
  printf("before: %s\nafter: %s\ntotal: %s\nanswer: %s\n", bef, aft, bef+aft, bef*aft)
}' Day25_graph.gv

# before: 799
# after: 775
# total: 1574
# answer: 619225
-/
#eval IO.println "Day 25, part 1: 619225"
