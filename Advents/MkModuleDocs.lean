import Advents

open Lean Elab Command

def mkTableRow (rs : List String) : String :=
  "| " ++ " | ".intercalate rs ++ " |"

#eval mkTableRow ["Day", "Description"]
#eval mkTableRow [":-:", "-"]

def mkMdTableRow (file : String) (descs : List String) : String :=
  let rows := (List.range descs.length).map fun i => [s!"[{i+1}]({file}#day-{i+1})", descs[i]!]
  "\n".intercalate <| rows.map mkTableRow

def mkMdTable (rootFile : String) (descs : List String)
    (days : List Nat := List.range descs.length) :
    String :=
  let rows := [["Day", "Description"], [":-:", "-"]] ++
    days.zipWith (fun i s => [s!"[{i}]({rootFile}#day-{i})", s]) descs
  ("\n".intercalate <| rows.map mkTableRow).trim.push '\n'

def addHeaderTable (descs : List String) : String :=
  let rows := [["Day", "Description"], [":-:", "-"]]
  "\n".intercalate <| (rows ++ [descs]).map mkTableRow

def formatModuleDocs (ds : Array String) : String :=
  "\n".intercalate ("Header" :: ds.toList)
  --ds.foldl (init := ) (· ++ ·)

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
    --let day := mod.toString.getNats.getLast!
    --dbg_trace docs.map (·.doc)
    entries := entries ++ [((docs.map (·.doc)).toList.take 1).map (·.takeWhile (· != '\n'))]
    --dbg_trace mkMdTable s!"{year}_descriptions_with_tests.md" ((docs.map (·.doc)).toList.take 1)
    --dbg_trace formatModuleDocs (docs.map (·.doc))
  let fileWithLinks :=
    mkMdTable s!"{year}_descriptions_with_tests.md" (entries.map (·[0]!)) days.toList
  dbg_trace fileWithLinks
  --IO.FS.writeFile (("Advents"/"AoC2025"/"2025_descriptions" : System.FilePath).withExtension "md") fileWithLinks

  let withHeader := (([["Day", "Description"], [":-:", "-"]]).map mkTableRow) ++
    entries.map (mkMdTableRow s!"{year}_descriptions_with_tests.md")
  --dbg_trace String.intercalate "\n" withHeader
  --dbg_trace addHeaderTable entries
