import Advents.Utils
import Lean.Elab.Frontend

open Lean Elab

namespace Day02

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day02.input"

#eval show MetaM _ from do
  let init ← IO.FS.readFile "Advents/day02_syntax.lean"
  let games ← IO.FS.readFile input
  let ngames := (← IO.FS.lines input).size + 1
  let fin := "#eval do addMyGames " ++ ⟨Nat.toDigits 10 ngames⟩
  let _ ← runFrontend (init ++ "\n" ++ games ++ fin) (.empty) "" `day02_syntax
