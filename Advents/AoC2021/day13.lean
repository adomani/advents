import Advents.Utils
open Lean

namespace Day13

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day13.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- Performs the operation that in the instructions is denoted by `fold along x=n`. -/
def foldX (n : Int) (g : Std.HashSet pos) : Std.HashSet pos :=
  g.fold (init := {}) fun h p => h.insert (if p.1 < n then p else (2 * n - p.1, p.2))

/-- Performs the operation that in the instructions is denoted by `fold along y=n`. -/
def foldY (n : Int) (g : Std.HashSet pos) : Std.HashSet pos :=
  g.fold (init := {}) fun h p => h.insert (if p.2 < n then p else (p.1, 2 * n - p.2))

/--
Scans the input `inp`. First, it constructs the initial grid, then it performs the folds.
The `Bool`ean input is `true`, then Lean will stop `fold`ing, after the first fold:
this is what the answer to part 1 wants.
-/
def getGrid (inp : Array String) (stop : Bool) : Std.HashSet pos := Id.run do
  let mut h := {}
  for l in inp do
    match l.getInts with
      | [x, y] => h := h.insert (y, x)
      | [v] =>
        let v := v
        if l.startsWith "fold along x=" then
          h := foldY v h
          if stop then return h
        else
          h := foldX v h
          if stop then return h
      | _ => continue
  return h

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := (getGrid dat true).size

#assert part1 atest == 17

solve 1 631

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := (getGrid dat false).size

#assert part2 atest == 16

solve 2 92

/-- A function to draw the answer to the second part of the puzzle. -/
def drawHash (h : Std.HashSet pos) (Nx Ny : Nat) : Array String := Id.run do
  let mut fin := #[]
  for i in [0:Nx] do
    let mut str := ""
    for j in [0:Ny] do
      match h.get? (i, j) with
        | some _d => str := str ++ "#"
        | none => str := str.push ' '
    fin := fin.push str
  return fin

#eval show Elab.Term.TermElabM _ from do
  let dat ← IO.FS.lines input
  let grid := getGrid dat false
  let (mx, my) := grid.fold (init := (0, 0)) fun (mx, my) (px, py) => (max mx px, max my py)
  let drawing := drawHash grid (mx.natAbs + 1) (my.natAbs + 1)
  guard (drawing == #[  "#### #### #    ####   ##  ##  ###  ####",
                        "#    #    #    #       # #  # #  # #   ",
                        "###  ###  #    ###     # #    #  # ### ",
                        "#    #    #    #       # # ## ###  #   ",
                        "#    #    #    #    #  # #  # # #  #   ",
                        "#### #    #### #     ##   ### #  # #   "])
  draws <| drawing
  IO.println "EFLFJGRF"

end Day13
