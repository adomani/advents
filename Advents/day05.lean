import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day05.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

/-- `compact l` takes as input a list of lists and returns the list of lists of lists obtained
from `l` by forming the list of sublists of consecutive elements of `l` that are non-empty.

`compact [[1, 2], [2], [], [], [2, 3], [4], []] = [[[1, 2], [2]], [[2, 3], [4]]]`
-/
partial
def compact (l : List (List α)) : List (List (List α)) :=
  if l.length == 0 then [] else
  let init := l.takeWhile (List.length · != 0)
  let fin := (l.dropWhile (List.length · != 0)).dropWhile (List.length · == 0)
  [init] ++ compact fin

--#assert compact [[], [0], [0], [], [], [0], [1]] == [[], [[0], [0]], [[0], [1]]]

/-- `get_seed_instrs maps` takes as input the input of the problem.
It returns a pair consisting of
* the list of seeds of the problem;
* the list of instructions for each conversion through which the seeds go.
-/
def get_seed_instrs (maps : String) : List Nat × List (List (List Nat)) :=
  let nums := maps.splitOn "map"
  let nums_and_empties := (nums.map (String.splitOn · "\n")).join.map (List.getNumbers ∘ String.toList)
  let cnums := compact nums_and_empties
  (cnums[0]![0]!, cnums.drop 1)

/-- `conv ins n` takes as input a list of natural numbers and a natural number.
It returns the "elementary" transformation that `n` goes through when it finds the
instructions `ins`.

This is a helper function to `conv1` which in turn is a helper function to `pass_through`. -/
def conv (ins : List Nat) (n : Nat) : Nat :=
  match ins with
    | [dest, src, lth] => if src ≤ n && n < src + lth then dest + (n - src) else n
    | _ => dbg_trace f!"oh no! {ins}"; 0

--#assert ([79, 14, 55, 13]).map (conv ([52, 50, 48])) == [81, 14, 57, 13]

/-- `conv1 inss n` takes as input a list of lists of natural numbers and a natural number.
It returns the transformation that `n` goes through following whichever one of the
instructions in `inss` applies.  It uses `conv` for the "elementary transformation".

This is a helper function to `pass_through`. -/
def conv1 (inss : List (List Nat)) (n : Nat) : Nat :=
  match inss.find? fun ins => ins[1]! ≤ n && n < ins[1]! + ins[2]! with
    | none     => n
    | some ins => conv ins n

/-- `pass_through instrs seeds` takes as input the entries of the output of `get_seed_instrs`.
It returns the value of the corresponding transformation on each element of `seeds`. -/
def pass_through (seeds : List Nat) (instrs : List (List (List Nat))) : List Nat :=
  Id.run do
  let mut seeds := seeds
  for ins in instrs do
    seeds := seeds.map (conv1 ins)
  return seeds

/-- `part1 maps` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (maps : String) : Nat :=
  let (seeds, instrs) := get_seed_instrs maps
  (pass_through seeds instrs).foldl min seeds[0]!

--#assert part1 test == 35

#eval show MetaM _ from do
  let ans := part1 (← IO.FS.readFile input)
  IO.println f!"Day 5, part1: {ans}"
  guard (ans == 579439039)

/-!
#  Question 2
-/

/-- `rev_instrs instrs` takes a list of lists of lists of natural numbers.
Interpreting the list as the original instructions,
it produces the instructions for the inverse map. -/
def rev_instrs (instrs : List (List (List Nat))) : List (List (List Nat)) :=
  instrs.reverse.map fun ins => (ins.map fun x => [x[1]!, x[0]!, x[2]!])

-- check that `pass_through` produces inverse maps with and without
-- `rev_instrs`
/-
#eval show MetaM _ from do
  let _maps := test
  let _maps ← IO.FS.readFile input
  let (ini_seeds, instrs) := get_seed_instrs _maps
  let seeds := pass_through ini_seeds instrs
  guard (pass_through seeds (rev_instrs instrs) == ini_seeds)
--/

/-- `part2 maps` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (maps : String) : Nat :=
  let fin_seeds := Id.run do
    let (ini_seeds, instrs) := get_seed_instrs maps
    let mut breaks := []
    let mut currInst := []
    for ins in [:instrs.length] do
      let new := instrs[ins]!
      currInst := currInst ++ [new]
      let ends := new.map fun x => x[0]!
      let begs := pass_through ends (rev_instrs currInst)
      breaks := breaks ++ begs
    let mut breaks_in_range := []
    for i in [:ini_seeds.length] do
      if i % 2 == 0 then
        breaks_in_range := ini_seeds[i]! :: breaks_in_range ++ breaks.filter fun x =>
          ini_seeds[i]! ≤ x && x < ini_seeds[i]! + ini_seeds[i+1]!
    pass_through breaks_in_range instrs
  (fin_seeds).foldl min fin_seeds[0]!

--#assert part2 test == 46

#eval show MetaM _ from do
  let ans := part2 (← IO.FS.readFile input)
  IO.println f!"Day 5, part2: {ans}"
  guard (ans == 7873084)
