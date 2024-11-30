import Advents.Utils
open Lean

namespace Day12

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day12.input"

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

/-- `atest`is simply the splitting of `test` into its array of lines. -/
def atest := (test.splitOn "\n").toArray

/-- `String.ep s` takes as input a string `s` and "easy parses" it.
It breaks it at the (unique) space ` `, converts the LHS to a
list of characters and the RHS to a list of natural numbers. -/
def String.ep (s : String) : List Char × List Nat :=
  match s.splitOn " " with
    | [l, r] => (l.toList, r.getNats)
    | _ => dbg_trace "misparsed"; default

/-- `red` is the type representing the parsed input of the problem.
It consists of a pair `(cs, ns)`, where
* `cs` is a list of characters -- a succession of `#`, `?` and `.`;
* `ns` a list of natural numbers, representing consecutive groups of
  `#`s separated by `.`s.
-/
abbrev red := List Char × List Nat

/-- `tot r` takes as input an element of `red` and returns
the number of assignments of `?` in the first element of the
pair that are compatible con the control sequence given by
the second pair. -/
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
  let ls := ((dat.map String.ep)).map tot
  ls.sum

#assert part1 atest == 21

solve 1 6935
