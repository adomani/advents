import Advents.Utils
import Lean.Elab.Frontend

open Lean Elab Expr Meta

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/i02.txt"

def getVal : ConstantInfo → Expr
  | .defnInfo val => val.value
  | _ => default

def cadd (t : Nat) (na : Name) : MetaM Nat := do
  if let some nav := (← getEnv).find? na then
    let d := ← (evalNat <| getVal nav).run
    return t + d.getD 0
  else
    return t

#eval show MetaM _ from do
  let init := ""
  let init ← IO.FS.readFile "Advents/day02syntax.lean"
  let games ← IO.FS.readFile input
  let ngames := (← IO.FS.lines input).size + 1
  let fin := "
def num := " ++ ⟨Nat.toDigits 10 ngames⟩ ++ "
#eval show MetaM _ from do
  let mut tot := 0
  for i in [:num] do
    let tadd := ← cadd tot (Name.str .anonymous (\"myGame\" ++ ⟨Nat.toDigits 10 i⟩))
    tot := tadd
  return tot
"
  let _ ← runFrontend (init ++ "\n" ++ games ++ fin) (.empty) "Advents.day02" (.str (.str (.str .anonymous "Advents") "day01") "lean")
  let mut tot := 0
  for i in [:6] do
    let tadd := ← cadd tot (Name.str .anonymous ("myGame" ++ ⟨Nat.toDigits 10 i⟩))
    tot := tadd
  return tot
