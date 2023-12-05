import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day05.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

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

def List.toRanges (l : List Nat) : List (Nat × Nat) :=
  match l with
    | [dest, src, lth] => Id.run do
      let mut lkup := #[]
      for i in [:lth] do
        lkup := lkup.push (src + i, dest + i)
      return lkup.toList
    | _ => dbg_trace f!"oh no! {l}"; []

#eval IO.println <| ([[52, 50, 48], [50, 98, 2]].map List.toRanges).join

def convert (n : Nat) (l : List (Nat × Nat)) : Nat :=
  (l.lookup n).getD n

#eval (List.iota 100).reverse.map (convert · ([[52, 50, 48], [50, 98, 2]].map List.toRanges).join)

def conv (ins : List Nat) (n : Nat) : Nat :=
  match ins with
    | [dest, src, lth] => if src ≤ n && n < src + lth then dest + (n - src) else n
    | _ => dbg_trace f!"oh no! {ins}"; 0

def conv1 (inss : List (List Nat)) (n : Nat) : Nat :=
  let ins := inss.find? fun ins => ins[1]! ≤ n && n < ins[1]! + ins[2]!
  match ins with
    | none     => n
    | some ins => conv ins n

#assert ([79, 14, 55, 13]).map (convert · ([[52, 50, 48], [50, 98, 2]].map .toRanges).join) == [81, 14, 57, 13]

#eval (List.iota 100).reverse.map (convert · ([[52, 50, 48], [50, 98, 2]].map .toRanges).join) --== [81, 14, 57, 13]
#eval (List.iota 100).reverse.map (conv [52, 50, 48] ∘ (conv [50, 98, 2])) --==
#assert ([79, 14, 55, 13]).map (conv ([52, 50, 48])) == [81, 14, 57, 13]
--, [50, 98, 2]

#eval
  let l1 := (List.iota 100).reverse.map (convert · ([[52, 50, 48], [50, 98, 2]].map .toRanges).join)
--  let l2 := (List.iota 100).reverse.map (conv [52, 50, 48] ∘ (conv [50, 98, 2]))
  let l2 := (List.iota 100).reverse.map (conv1 [[50, 98, 2], [52, 50, 48]])
  let diffs := (List.iota 100).reverse.map fun i => if l1[i-1]! != l2[i-1]! then (l1[i-1]! , l2[i-1]!) else (0, 0)
  diffs.filter (· != (0,0))


#eval [[0], [], [], [1]].partition (List.length · != 0)

partial
def compact (l : List (List α)) : List (List (List α)) :=
  if l.length == 0 then [] else
  let init := l.takeWhile (List.length · != 0)
  let fin := (l.dropWhile (List.length · != 0)).dropWhile (List.length · == 0)
  [init] ++ compact fin

#eval compact [[], [0], [0], [], [], [0], [1]]
#assert compact [[], [0], [0], [], [], [0], [1]] == [[], [[0], [0]], [[0], [1]]]



--#check List.lookup
#eval do
  let maps := test
  let nums := maps.splitOn "map"
  let nums_and_empties := (nums.map (String.splitOn · "\n")).join.map (List.getNumbers ∘ String.toList)
  IO.println <| compact nums_and_empties
  let seeds := nums_and_empties[0]!
  let instrs := (nums_and_empties.drop 1)
--  let nums := nums_and_empties.filter (List.length · != 0)
  IO.println <| nums_and_empties
  IO.println <| f!"\n{seeds}\n{instrs}"

def get_seed_insts (maps : String) : List Nat × List (List (List Nat)) :=
  let nums := maps.splitOn "map"
  let nums_and_empties := (nums.map (String.splitOn · "\n")).join.map (List.getNumbers ∘ String.toList)
  let cnums := compact nums_and_empties
  (cnums[0]![0]!, cnums.drop 1)

def pass_through (instrs : List (List (List Nat))) (seeds : List Nat) : List Nat :=
  Id.run do
  let mut seeds := seeds
  for ins in instrs do
    seeds := seeds.map (conv1 ins)
  return seeds
#eval show MetaM _ from do
  let maps := test
  let maps ← IO.FS.readFile input
  let (seeds, instrs) := get_seed_insts maps
  let seeds := pass_through instrs seeds
--  IO.println <| seeds
  IO.println <| seeds.foldl min seeds[0]!
  guard (seeds.foldl min seeds[0]! == 579439039)
--  IO.println <| nums_and_empties
--  IO.println <| f!"\n{seeds}\n{instrs}"


#eval do
--  let maps ← IO.FS.readFile input
  let maps := test
  let nums := maps.splitOn "map"
  let nums_and_empties := (nums.map (String.splitOn · "\n")).join.map (List.getNumbers ∘ String.toList)
  let cnums := compact nums_and_empties
  let mut seeds := cnums[0]![0]!
  let instrs := (cnums.drop 1)  -- a list of lists of Nats
--  let instrs := (nums_and_empties.drop 1)
--  let nums := nums_and_empties.filter (List.length · != 0)
  for ins in instrs do
    let lkup := (ins.map List.toRanges).join
    seeds := seeds.map (convert · lkup)
  IO.println <| seeds.foldl min seeds[0]!
--  IO.println <| nums_and_empties
--  IO.println <| f!"\n{seeds}\n{instrs}"

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
  let (ini_seeds, instrs) := get_seed_insts _maps
  let seeds := pass_through instrs ini_seeds
  guard (pass_through (rev_instrs instrs) seeds == ini_seeds)
-/

def part2 (maps : String) : Nat :=
  let fin_seeds := Id.run do
    let (ini_seeds, instrs) := get_seed_insts maps
    let mut breaks := []
    let mut currInst := []
    for ins in [:instrs.length] do
      let new := instrs[ins]!
      currInst := currInst ++ [new]
      let ends := new.map fun x => x[0]!
      let begs := pass_through (rev_instrs currInst) ends
      breaks := breaks ++ begs
    let mut breaks_in_range := []
    for i in [:ini_seeds.length] do
      if i % 2 == 0 then
        breaks_in_range := ini_seeds[i]! :: breaks_in_range ++ breaks.filter fun x =>
          ini_seeds[i]! ≤ x && x < ini_seeds[i]! + ini_seeds[i+1]!
    pass_through instrs breaks_in_range
  (fin_seeds).foldl min fin_seeds[0]!

--#assert part2 test == 46

#eval show MetaM _ from do
  let ans := part2 (← IO.FS.readFile input)
  IO.println f!"Day 5, part2: {ans}"
  guard (ans == 7873084)
