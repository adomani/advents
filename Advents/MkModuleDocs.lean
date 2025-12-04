import Advents

open Lean Elab Command

def mkTableRow (rs : List String) : String :=
  "| " ++ " | ".intercalate rs ++ " |"
#check List.replicate
#eval mkTableRow ["Day", "Description"]
#eval mkTableRow [":-:", "-"]
def padRight (s : String) (n : Nat) :=
  s ++ String.ofList (List.replicate (n - s.length) ' ')

def mkMdTable (rootFile : String) (descs : List String)
    (days : List Nat := List.range descs.length) :
    String :=
  let rows := [["Day", "Description"], [":-:", "-"]] ++
    days.zipWith (fun i s => [s!"[{i}]({rootFile}#day-{i})", s]) descs
  Id.run do
  let mut maxs := List.replicate rows[0]!.length 0
  for i in rows do
    maxs := maxs.zipWith max (i.map (·.length))
  let rows := rows.map (·.zipWith padRight maxs)
  return ("\n".intercalate <| rows.map mkTableRow).trim.push '\n'

run_cmd
  let year := 2025
  let files ← System.FilePath.walkDir s!"Advents/AoC{year}"
  let leanFiles := (files.qsort (·.toString < ·.toString)).filter (·.extension == some "lean")
  let days := leanFiles.map (·.toString.getNats.getLast!)
  let modNames ← leanFiles.mapM (moduleNameOfFileName · none)
  --dbg_trace modNames
  let mut entries := []
  for mod in modNames do
    let some docs := Lean.getModuleDoc? (← getEnv) mod | return
    entries := entries ++ [((docs.map (·.doc)).toList.take 1).map (·.takeWhile (· != '\n'))]
  let fileWithLinks :=
    mkMdTable s!"{year}_descriptions_with_tests.md" (entries.map (·[0]!)) days.toList
  dbg_trace fileWithLinks
  --IO.FS.writeFile (("Advents"/"AoC2025"/"2025_descriptions" : System.FilePath).withExtension "md") fileWithLinks
