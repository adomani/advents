import Advents.Utils
import Lean.Elab.Frontend

open Lean Elab

namespace Day02

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2023"/"day02" : FilePath).withExtension "input"

/-
Day 2, 2023, part 1: 2169
Day 2, 2023, part 2: 60948
-/
#eval show MetaM _ from do
  let init ← IO.FS.readFile "Advents/AoC2023/day02_syntax.lean"
  let games ← IO.FS.readFile input
  let ngames := (← IO.FS.lines input).size + 1
  let fin := "#eval do Day02.addMyGames " ++ String.ofList (Nat.toDigits 10 ngames)
  let _ ← runFrontend (init ++ "\n" ++ games ++ fin) (.empty) "" `day02_syntax

end Day02
