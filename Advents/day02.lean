import Advents.Utils
open Lean Elab Tactic TacticM

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/i02.txt"

/-!
#  Question 1
-/

#eval do IO.println (← IO.FS.readFile input)

abbrev cols := Nat × Nat × Nat

instance : Add cols where
  add x y := (x.1 + y.1, x.2.1 + y.2.1, x.2.2 + y.2.2)
#check Syntax

def one_color (s : String) : cols :=
  let no_spaces := s.toList.filter (· != ' ')
  let (number, color) := no_spaces.partition Char.isDigit
  let (number, color) : Nat × String := (String.toNat! ⟨number⟩, ⟨color⟩)
--  dbg_trace s!"{no_spaces}\n{number}\n{color}"
  match color with
    | "red" => (number, 0, 0)
    | "green" => (0, number, 0)
    | "blue" => (0, 0, number)
    | _ => default

#eval one_color "  45   blue"

def get_color_number (s : String) : Nat × List cols :=
  let x := s.splitOn ":"
  let gameID := x[0]!
  let runs := ((x[1]!).splitOn ";").map (String.splitOn · ",")
  let colors_lists := runs.map fun x => x.map one_color
--  dbg_trace (gameID, runs)
--  dbg_trace colors_lists.map List.sum
  (String.toNat! (gameID.dropWhile (! Char.isDigit ·)), colors_lists.map List.sum)

#eval get_color_number "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

def limit : cols := (12, 13, 14)

instance : LE cols where
  le x y := (x.1 ≤ y.1) ∧ (x.2.1 ≤ y.2.1) ∧ (x.2.2 ≤ y.2.2)

theorem cols.le_def {x y : cols} : x ≤ y ↔ (x.1 ≤ y.1) ∧ (x.2.1 ≤ y.2.1) ∧ (x.2.2 ≤ y.2.2) := Iff.rfl

instance : DecidableRel (LE.le : cols → cols → Prop) :=
  fun _ _ => decidable_of_iff' _ cols.le_def

#eval ((1, 2, 3) : cols) ≤ (4, 1, 3)

def test := "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

#eval do
  let rows := test.splitOn "\n"
  let rows ← IO.FS.lines input
--  IO.println rows
  let games := rows.map get_color_number
  let small := games.filter fun ((_, gms) : Nat × List cols) => gms.all ( · ≤ limit)
  let smallIDs := small.map Prod.fst
--  IO.println games
  IO.println <| smallIDs.sum

/-!
#  Question 2
-/

def sup (x y : cols) : cols := (max x.1 y.1, max x.2.1 y.2.1, max x.2.2 y.2.2)

#eval sup (sup (4, 0, 3) (1, 2, 6)) (0, 2, 0)

#eval do
  let rows := test.splitOn "\n"
  let rows ← IO.FS.lines input
  let games := rows.map get_color_number
  let sups := games.map fun ((_, gms) : Nat × List cols) => match gms with
    | []    => default
    | m::ms => ms.foldl sup m
  IO.println sups
  let powers := sups.map fun ((a, b, c) : cols) => a * b * c
  IO.println <| powers.sum
