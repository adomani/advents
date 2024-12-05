import Advents.AoC2024.day05
import Lean
#check String.intercalate
open Lean

def pathFromYD (year day : String) : Name :=
  `Advents ++ ((.str .anonymous s!"AoC{year}")) ++ (.str .anonymous s!"day{day}")

run_cmd
  let year := "2024"
  let day := "05"
  let mod := pathFromYD year day
  match getModuleDoc? (← getEnv) mod with
      | none => return
      | some docs =>
        let docs := docs.map (·.doc)
        let descDoc := docs.filter fun d => 2 ≤ (d.splitOn " Day ").length

        dbg_trace String.intercalate "\n---\n" docs.toList
        dbg_trace s!"[Solution in Lean](day{day}.lean)"
