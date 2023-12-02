import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/i02.txt"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

abbrev cols := Nat × Nat × Nat

instance : Add cols where
  add x y := (x.1 + y.1, x.2.1 + y.2.1, x.2.2 + y.2.2)

def one_color (s : String) : cols :=
  let no_spaces := s.toList.filter (· != ' ')
  let (number, color) := no_spaces.partition Char.isDigit
  let (number, color) : Nat × String := (String.toNat! ⟨number⟩, ⟨color⟩)
  match color with
    | "red" => (number, 0, 0)
    | "green" => (0, number, 0)
    | "blue" => (0, 0, number)
    | _ => default

--#assert one_color "  45   blue" == (0, 0, 45)

def get_color_number (s : String) : Nat × List cols :=
  let x := s.splitOn ":"
  let gameID := x[0]!
  let runs := ((x[1]!).splitOn ";").map (String.splitOn · ",")
  let colors_lists := runs.map fun x => x.map one_color
  (String.toNat! (gameID.dropWhile (! Char.isDigit ·)), colors_lists.map List.sum)

/-
#assert
  get_color_number "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" ==
    (1, [(4, 0, 3), (1, 2, 6), (0, 2, 0)])
--/

def limit : cols := (12, 13, 14)

instance : LE cols where
  le x y := (x.1 ≤ y.1) ∧ (x.2.1 ≤ y.2.1) ∧ (x.2.2 ≤ y.2.2)

theorem cols.le_def {x y : cols} : x ≤ y ↔ (x.1 ≤ y.1) ∧ (x.2.1 ≤ y.2.1) ∧ (x.2.2 ≤ y.2.2) := Iff.rfl

instance : DecidableRel (LE.le : cols → cols → Prop) :=
  fun _ _ => decidable_of_iff' _ cols.le_def

--#assert (((1, 2, 3) : cols) ≤ (4, 1, 3)) == false
--#assert (((0, 2, 13) : cols) ≤ (4, 2, 13)) == true

def test := "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

def part1 (rows : Array String) : Int :=
  let games := rows.map get_color_number
  let small := games.filter fun ((_, gms) : Nat × List cols) => gms.all (· ≤ limit)
  let smallIDs := small.map Prod.fst
  smallIDs.sum

--#assert part1 (test.splitOn "\n").toArray == 8
--#assert part1 (← IO.FS.lines input) == 2169

#eval do IO.println <| "Day 2, part 1: " ++ f!"{part1 (← IO.FS.lines input)}"

/-!
#  Question 2
-/

def sup (x y : cols) : cols := (max x.1 y.1, max x.2.1 y.2.1, max x.2.2 y.2.2)

--#assert sup (sup (4, 0, 3) (1, 2, 6)) (0, 2, 0) == (4, 2, 6)

def part2 (rows : Array String) : Int :=
  let games := rows.map get_color_number
  let sups := games.map fun ((_, gms) : Nat × List cols) => match gms with
    | []    => default
    | m::ms => ms.foldl sup m
  let powers := sups.map fun ((a, b, c) : cols) => a * b * c
  powers.sum

--#assert part2 (test.splitOn "\n").toArray == 2286
--#assert part2 (← IO.FS.lines input) == 60948

#eval do IO.println <| "Day 2, part 1: " ++ f!"{part2 (← IO.FS.lines input)}"
