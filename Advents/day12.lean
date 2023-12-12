import Advents.Utils
open Lean

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

def atest := (test.splitOn "\n").toArray

def String.ep (s : String) : List Char × List Nat :=
  match s.splitOn " " with
    | [l, r] => (l.toList, r.toList.getNumbers)
    | _ => dbg_trace "misparsed"; default

abbrev red := List Char × List Nat

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
  (((dat.map String.ep)).map tot).sum

#assert part1 atest == 21

solve 1 6935

/-!
#  Question 2
-/

def extendRed (r : red) : red :=
  let (l, r) : _ := r
  (['?'].intercalate (List.replicate 5 l), (List.replicate 5 r).join)

#eval
  let x : red := (".#".toList, [1])
  extendRed x

/-
#eval show MetaM _ from do
  let x := atest.map (extendRed ∘ String.ep)
  let res := x.map fun a => (String.mk a.1).push '\n'
  let res := x.map tot
  let ris := (res.sum, res)
  guard (ris == (525152, #[1, 16384, 1, 16, 2500, 506250]))
  IO.println <| ris
  --IO.println <| res[3]! --(res.sum, res)
-/


#exit

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 (test.splitOn "\n").toArray == ???

--solve 2
