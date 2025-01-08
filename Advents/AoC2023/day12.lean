import Advents.Utils
open Std

namespace Day12

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day12.input"

/-!
#  Question 1
-/

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
def _root_.String.ep (s : String) : List Char × List Nat :=
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

--def oneStep : red → red
--  | ([], 0::cs) => ([], cs)
--  --| ([], []) => 1
--  --| ([], _) => default
--  --| ('#'::_cs, []) => default
--  --| ('#'::_cs, 0::_ns) => default
--  | ('#'::'#'::cs, n::ns) => ('#'::cs, (n - 1)::ns)
--  | ('#'::'.'::cs, 1::ns) => (cs, ns)
--  --| ('#'::'.'::_cs, _::_ns) => default
--  | (['#'], 1::ns) => ([], ns)
--  --| (['#'], _) => 0
--  | ('#'::'?'::cs, 1::ns) => (cs, ns)
--  | ('#'::'?'::cs, n::ns) => ('#'::cs, (n-1)::ns)
--  | ('.'::cs, ns) => (cs, ns)
--  | ('?'::cs, 0::ns) => (cs, ns)
--  | ('?'::cs, []) => (cs, [])
--  | ('?'::cs, ns) => tot ('#'::cs, ns) + tot (cs, ns)
--  | x => panic s!"1000 {x}" --; 1000 --(1000, 1000)

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

def tally (h : HashMap red Nat) (r : red) : HashMap red Nat :=
  if let some val := h[r]? then h.alter r (some <| ·.getD 0 + 1) else
  match r with
  | ([], 0::cs) => tally h ([], cs)
  | ([], []) => h.insert r 1
  --| ([], _) => default
  --| ('#'::_cs, []) => default
  --| ('#'::_cs, 0::_ns) => default
  | ('#'::'#'::cs, n::ns) => tally h ('#'::cs, (n - 1)::ns)
  | ('#'::'.'::cs, 1::ns) => tally h (cs, ns)
  --| ('#'::'.'::_cs, _::_ns) => default
  | (['#'], 1::ns) => tally h ([], ns)
  --| (['#'], _) => 0
  | ('#'::'?'::cs, 1::ns) => tally h (cs, ns)
  | ('#'::'?'::cs, n::ns) => tally h ('#'::cs, (n-1)::ns)
  | ('.'::cs, ns) => tally h (cs, ns)
  | ('?'::cs, 0::ns) => tally h (cs, ns)
  | ('?'::cs, []) => tally h (cs, [])
  | ('?'::cs, ns) =>
    let new := tally h (cs, ns)

    tally new ('#'::cs, ns)
  | _ => h --.insert r 0 --panic s!"1000 {x}" --; 1000 --(1000, 1000)

#assert (atest.map String.ep).map tot == #[1, 4, 1, 1, 4, 10]

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let ls := ((dat.map String.ep)).map tot
  ls.sum

#assert part1 atest == 21

solve 1 6935

/-!
#  Question 2
-/

def unfold (s : red) (n : Nat := 5) : red :=
  let (l, r) := s
  (List.intercalate ['?'] (List.replicate n l), (List.replicate n r).flatten)

#assert unfold (".# 1".ep) ==
  ".#?.#?.#?.#?.# 1,1,1,1,1".ep
#assert unfold ("???.### 1,1,3".ep) ==
  "???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3".ep
--#exit
set_option trace.profiler true in
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let rs := dat.map String.ep
  let mut cache : HashMap red Nat := ∅
  --let mut res := 0
  for r' in rs do
    let r := unfold r' 3
    cache := tally cache r
    --match cache[r]? with
    --  | some n => res := res + n
    --  | none => let n := tot r; cache := cache.insert r n; res := res + n
  IO.println <| cache.fold (init := 0) fun h _ n => h + n
  for c in cache.toArray do IO.println c

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day12
