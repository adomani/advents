import Advents

open Std Lean Elab Command

def mkTableRow (rs : List String) : String :=
  "| " ++ " | ".intercalate rs ++ " |"

#assert mkTableRow ["Day", "Description"] == "| Day | Description |"
#assert mkTableRow [":-:", "-"] == "| :-: | - |"

def padRight (s : String) (n : Nat) :=
  s ++ String.ofList (List.replicate (n - s.length) ' ')

def mkMdTable (rootFile : String) (descs : List String) (days : List Nat) :
    String :=
  let rows := [["Day", "Description"], [":-:", "-"]] ++
    days.zipWith (fun i s => [s!"[{i}]({rootFile}#day-{i})", s]) descs
  let maxs := rows.foldl (init := List.replicate rows[0]!.length 0) (·.zipWith (max · ·.length) ·)
  let rows := rows.map (·.zipWith padRight maxs)
  ("\n".intercalate <| rows.map mkTableRow).trim.push '\n'

local instance [BEq α] [Hashable α] [ToString α] [LT α] [DecidableRel (α := α) (· < ·)] :
    ToString (HashSet α) where
  toString as := s!"{as.toArray.qsort (· < ·)}"

def _root_.Lean.Expr.rawStrLit? : Expr → Option String
  | .lit (Literal.strVal s) => s
  | _                      => none


run_cmd
  let year := 2025
  let files ← System.FilePath.walkDir s!"Advents/AoC{year}"
  let leanFiles := (files.qsort (·.toString < ·.toString)).filter (·.extension == some "lean")
  let days := leanFiles.map (·.toString.getNats.getLast!)
  let modNames ← leanFiles.mapM (moduleNameOfFileName · none)
  --dbg_trace modNames
  let env ← getEnv
  --let imports := env.imports.map (·.module)
  let csts := env.const2ModIdx--.filter fun _ i => imports[i]!.toString.startsWith "Advent"

  let tests := csts.filter fun c _ =>
    (!c.isInternalDetail) && (c.toString.startsWith s!"AoC{year}_Day") && (c.toString.find? ".test").isSome
  dbg_trace tests.toArray.qsort (·.1.toString < ·.1.toString)
  dbg_trace (tests.filterMap fun n _ => match (env.find? n) with
    | some c => c.value?.map (·.rawStrLit?)
    | none => none).toArray
  let mut entries := []
  for mod in modNames do
    let some docs := Lean.getModuleDoc? env mod | return
    entries := entries ++ [((docs.map (·.doc)).toList.take 1).map (·.takeWhile (· != '\n'))]
  let fileWithLinks :=
    mkMdTable s!"{year}_descriptions_with_tests.md" (entries.map (·[0]!)) days.toList
  --dbg_trace fileWithLinks
  --IO.FS.writeFile (("Advents"/"AoC2025"/"2025_descriptions" : System.FilePath).withExtension "md") fileWithLinks
