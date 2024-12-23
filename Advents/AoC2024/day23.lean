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

def inputToGraph1 (dat : Array String) : graph where
  edges := dat.foldl (init := ∅) fun h s =>
    --if (s.splitOn "t").length == 1 then h else
    match s.splitOn "-" with
      | [a, b] => h.insert (a, b) |>.insert (b, a)
      | _ => panic "wrong input!"

/-
tc,td,wh

co,de,ta
co,ka,ta
de,ka,ta
qp,td,wh
tb,vc,wq
td,wh,yn
-/

#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let gr := inputToGraph1 dat
  --IO.println gr.edges.toArray
  let mut trs : Std.HashSet (Array String) := ∅
  for (e, f) in gr.edges do
    if ! e.startsWith "t" then continue
    for (g, h) in gr.edges do
    --let fromF : Std.HashSet (Array String) :=
      --gr.edges.fold (init := ∅) fun trf (g, h) =>
        if g == f && gr.edges.contains (h, e) then
          trs := trs.insert <| #[e, f, h].qsort (· < ·)
        --else trf
  --trs := trs.union <| fromF --.fold (init := ∅)
  for t in trs do let T : Std.HashSet String :=  .ofArray t; if T.size != 3 then IO.println t
  IO.println trs.size --toArray
  for t in trs do IO.println t--rs.toArray

-- 2195


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

end Day23
