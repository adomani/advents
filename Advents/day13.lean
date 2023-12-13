import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day13.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"

#eval do
  IO.println ((← IO.FS.lines input).size, (← IO.FS.lines input)[0]!.length)

def getPats (s : String) : Array String :=
  (s.splitOn "\n\n").toArray

def transpose (s : Array String) : Array String :=
  let rows := s.map (List.toArray ∘ String.toList)
  let cols := rows[0]!.size
  Id.run do
    let mut ans := #[]
    for c in [:cols] do
      let mut row := ""
      for r in [:rows.size] do
        row := row.push rows[r]![c]!
      ans := ans.push row
    return ans

#eval do
  let ts := getPats test
  let ca := ts[1]!
  let car := (ca.splitOn "\n").toArray
  draw <| car
  draw <| transpose car

/-- Decide whether a horizontal position is a line of symmetry for the array. -/
def isNsymm (s : Array String) (n : Nat) : Bool :=
  let rloc := min n (s.size - n)
--  dbg_trace s!"line between = {(n-1,n)}, range = {rloc}"
  if rloc = 0 then false else
  Id.run do
  let mut cond := true
  for j in [:rloc] do
    if s[n + j]! ≠ s[(n - 1) - j]! then
      cond := false
      break
  cond

/-- Find the array of positions for horizontal symmetries. -/
def rsymm (s : Array String) : Array Nat :=
  Id.run do
    let mut rows := #[]
    for n in [:s.size] do
      if isNsymm s n then rows := rows.push (n)
    return rows

/-- Find the array of positions for vertical symmetries. -/
def csymm (s : Array String) : Array Nat :=
  rsymm (transpose s)

def tally (s : Array String) : Nat :=
  let rs := rsymm s
  let cs := csymm s
  dbg_trace s!"Rows: {rs}\nColumns: {cs}"
  100 * rs.sum + cs.sum

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let ts := getPats dat
  Id.run do
  let mut tot := 0
  for ca in ts do
--    let ca := ts[cai]!
    let t1 := (ca.splitOn "\n").toArray
--    nums t1
    let tal := tally t1
    if tal = 0 then dbg_trace ca
    tot := tot + tal
  return tot

#assert part1 test == 405

--solve 1 33735 file

/-!
#  Question 2
-/

/-- `smudge l r` compares two strings `l` and `r`.
If `l` and `r` differ by exactly one character and
this happens at a position with index `i`, then
`smudge` returns
* `some (true, i)` if `l` contains `#` at position `i`
* `some (false, i)` `p`, otherwise.

If the two strings are identical or differ in more
than one location, then `smudge` returns `false`. -/
def smudge (l r : Array Char) : Option (Bool × Nat) :=
  let lr := l.zipWith r (fun x y =>
    if x = y then none else some (x == '#'))
  match lr.reduceOption with
    | #[f] => (f, (lr.findIdx? (· == some f)).get!)
    | _ => none

#assert (smudge "#.#.#".toList.toArray "#.#.#".toList.toArray == none)
#assert (smudge "#####".toList.toArray "#.#.#".toList.toArray == none)
#assert (smudge "..#.#".toList.toArray "#.#.#".toList.toArray == some (false, 0))
#assert (smudge "#.#.#".toList.toArray "#.#.·".toList.toArray == some (true, 4))

#eval
  smudge "#....#..#".toList.toArray "#...##..#".toList.toArray

def ssmudge (l r : Array Char) : Option Nat :=
  let lr := l.zipWith r (! · = ·)
  if (lr.filter id).size ≠ 1 then none else
  lr.findIdx? (· == true)

#assert (ssmudge "#.#.#".toList.toArray "#.#.#".toList.toArray == none)
#assert (ssmudge "#####".toList.toArray "#.#.#".toList.toArray == none)
#assert (ssmudge "..#.#".toList.toArray "#.#.#".toList.toArray == some 0)
#assert (ssmudge "#.#.#".toList.toArray "#.#.·".toList.toArray == some 4)

#eval
  ssmudge "#....#..#".toList.toArray "#...##..#".toList.toArray

def rsmudge (dat : Array (Array Char)) := --: Array (Option (Bool × Nat)) :=
  Id.run do
    let mut c := #[]
    for i in [:dat.size] do
--      if i ≠ 0 then
      for j in [:i] do
        let sm := smudge dat[i]! dat[j]!
        if sm.isSome then
          let (tf, col) := sm.get!
          let row := if tf then i else j
          c := c.push ([row, col], sm.get!)
          --c := c.push ([i, j], sm.get!)
--        dbg_trace s!"{(i, j, sm)}"
--        c := 0
    return c

#eval do
  let ts := getPats test
  let ind := 1
  let chars := ts.map
    fun x : String => (x.splitOn "\n").toArray.map (List.toArray ∘ String.toList)
  let c0 := (transpose <| chars[ind]!.map (String.mk ∘ Array.toList)).map <| List.toArray ∘ String.toList
  let c0 := chars[ind]!
--  IO.println <| rsmudge c0
  for c in rsmudge c0 do IO.println <| c
  IO.println ""
  draw <| c0.map (String.mk ∘ Array.toList) --(ts[ind]!.splitOn "\n").toArray

def rssmudge (dat : Array (Array Char)) : Array (Nat × Nat) :=
  Id.run do
    let mut c := #[]
    for i in [:dat.size] do
      for j in [:i] do
        let sm := ssmudge dat[i]! dat[j]!
        if sm.isSome then
          let col := sm.get!
          c := (c.push (i, col)).push (j, col)
    return c

#eval do
  let ts := getPats test
  let ind := 1
  let chars := ts.map
    fun x : String => (x.splitOn "\n").toArray.map (List.toArray ∘ String.toList)
  let c0 := (transpose <| chars[ind]!.map (String.mk ∘ Array.toList)).map <| List.toArray ∘ String.toList
  let c0 := chars[ind]!
--  IO.println <| rsmudge c0
  for c in rssmudge c0 do IO.println <| c
  IO.println ""
  draw <| c0.map (String.mk ∘ Array.toList) --(ts[ind]!.splitOn "\n").toArray

def cssmudge (dat : Array (Array Char)) : Array (Nat × Nat) :=
  let tr := (transpose <| dat.map (String.mk ∘ Array.toList)).map (List.toArray ∘ String.toList)
  let sm := rssmudge tr
  sm.map fun x => (x.2, x.1)

def potSmudges (dat : Array (Array Char)) : Array (Nat × Nat) :=
  rssmudge dat ++ cssmudge dat

def rockAshSwap : Char → Char
  | '.' => '#'
  | '#' => '.'
  | d => d

#assert "#.#.#".map rockAshSwap == ".#.#."

def RA (p : Nat × Nat) (dat : Array String) : Array String :=
  (Array.range dat.size).map fun x =>
    if x == p.1 then(String.modify dat[p.1]! ⟨p.2⟩ rockAshSwap)
    else dat[x]!

def tally2 (s : Array String) : Array Nat × Array Nat :=
  (rsymm s, csymm s)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : String) : Nat :=
  let ts := getPats dat
  Id.run do
    let mut tot := 0
--    let mut ind := 0
    for ind in [:ts.size] do
      let chars := ts.map
        fun x : String => (x.splitOn "\n").toArray.map (List.toArray ∘ String.toList)
      let c0 := chars[ind]!
      let rs := rssmudge c0
      let cs := cssmudge c0
      let smudges := rs ++ cs
      let ta := tally2 <| c0.map <| String.mk ∘ Array.toList
      let lr := match ta with
        | (#[], #[a]) => (2, a)
        | (#[a], #[]) => (1, a)
        | _ => dbg_trace "too many refls!"; default
      for s in smudges do
        let smudgeMatrix := RA s <| c0.map (String.mk ∘ Array.toList)
        let ta2 := tally2 smudgeMatrix
        let newt2 := if lr.1 == 1 then
          (ta2.1.erase lr.2, ta2.2) else
          (ta2.1, ta2.2.erase lr.2)
        if (newt2.1 ++ newt2.2).size == 1 then
          tot := tot + 100 * newt2.1[0]! + newt2.2[0]!
          break
    return tot

#assert part2 test == 400

--solve 2 38063 file
