import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day21.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `dirs` is the list of directions `[.U, .D, .L, .R]`. -/
abbrev dirs : List dir := [.U, .D, .L, .R]

/-- converts a unit vector into the direction that is
obtained by a counter-clockwise rotation.
It is useful for defining orientations. -/
def toCCWPos : dir → pos
  | .L => (  0, - 1)
  | .R => (  0,   1)
  | .U => (- 1,   0)
  | .D => (  1,   0)
  | .S => (  0,   0)

/-- finds the character `S` in `dat`, returning its two integer coordinates. -/
def findS (dat : Array String) : pos :=
  let sx :=
    Id.run do
      let mut x := 0
      for i in [:dat.size] do
        if dat[i]!.contains 'S' then
          x := i
      return x
  (sx, (dat[sx]!.find (· == 'S')).byteIdx)

#assert
  let dat := atest
  findS dat == (↑(dat.size / 2), ↑(dat.size / 2))

#assert
  let dat := ← IO.FS.lines input
  findS dat == (↑(dat.size / 2), ↑(dat.size / 2))

/-- returns the position of the first available rock (`#`) in `dat`. -/
def findRk (dat : Array String) : pos :=
  match dat.findIdx? (String.contains · '#') with
    | none => dbg_trace "no rocks!"; default
    | some s => (s, (dat[s]!.find (· == '#')).byteIdx)

#assert findRk atest == (1, 5)
#assert findRk (← IO.FS.lines input) == (1, 19)

/-- creates the `HashSet` containing all the positions of all the rocks in `dat`. -/
def getRocks (dat : Array String) (c : Char := '#') : HashSet pos :=
  Id.run do
    let mut rks : HashSet pos := .empty
    for i in [:dat.size] do
      let row := dat[i]!.toList
      for j in [:row.length] do
        if row[j]! == c then
          rks := rks.insert (i, j)
    return rks

/-- `mvs rk gd bd f` takes as input
* `rk`, the locations of the rocks;
* `gd`, the possible locations of the gardener;
* `bd`, a layer of starting points for potential new positions;
* `f`, an auxilliary function to possibly modify the current position (this is useful for part 2).

It returns a pair consisting of
* the locations where the gardener could be in one more step and
* the array of those positions that were really added to `gd`.

The expectation is that `mvs` will be ran recursively, feeding back its output as new `gd` and
`bd` inputs to itself, until we built the whole `HashSet` of reachable positions. -/
def mvs (rk gd : HashSet pos) (bd : Array pos) (f : pos → pos) : HashSet pos × Array pos :=
  Id.run do
  let mut new : HashSet pos := gd
  let mut nbd := #[]
  for g in bd do
    let mvs := (dirs.map (toCCWPos · + g)).filter fun x => (rk.find? (f x)).isNone
    for m in mvs do
      let (n, tf) := new.insert' m
      new := n
      if !tf then nbd := nbd.push m
  return (new, nbd)

/-- `parts dat n f` takes as input the input of the problem, an optional number of iterations
`n` and an optional function `f : pos → pos`.
It returns the possible locations that the gardener can reach in `n` steps.

It recursively applies `mvs` to its output, starting from the position of `S` and
the layout of the garden. -/
def parts (dat : Array String) (n : Nat := 64) (f : pos → pos := id) : HashSet pos :=
  let rk := getRocks dat
  Id.run do
  let init := findS dat
  let mut bd := #[init]
  let mut gd : HashSet pos := HashSet.empty.insert init
  for _ in [:n] do
    (gd, bd) := (mvs rk gd bd f)
  return gd

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1.

It uses `parts` as the main program. -/
def part1 (dat : Array String) (n : Nat := 64) : Nat :=
  ((parts dat n).toArray.filter fun (x, y) => (x + y) % 2 == n % 2).size

#assert part1 atest 6 == 16

solve 1 3764

/-!
#  Question 2
-/

/-!
We can attempt to solve the second part working with coordinates modulo the grid size.
-/
/- test that we can really solve the second part working modulo the size of the grid
#eval do show MetaM _ from do
  let dat := atest
  let sz := dat.size
  let mut tots := #[]
  for d in [6, 10, 50, 100, 500] do
    tots := tots.push <| part1 dat d (fun (x, y) => (x % sz, y % sz))
  IO.println <| tots
  guard (tots == #[16, 50, 1594, 6536, 167004])
-/

/-!
However, the test is already slow and I do not have the patience to even check the remaining
two data points, `d = 1000, 5000`.

Besides, we should reach up to `26501365`, so we proceed differently.

We are going to proceed in "bulk", counting all "full" gardens at once
and only resorting to evolving our moves for the fringes of the edge gardens.

Let's check that the row and column containing `S` contains only `.`s, besides `S` itself.
-/

/-- `onlyS dat` takes as input an array `dat` of strings and returns
* `true` if the row and column containing `S` consist of a single `S` and the rest is `.`s;
* `false` otherwise.

It is `true` for the puzzle input, as you can see from the `#assert` line below.
-/
abbrev onlyS (dat : Array String) : Bool :=
  let (sx, sy) := findS dat
  let s2 := [(sx, dat), (sy, dat.transpose)].map fun (c, t) =>
    String.mk (t[c.toNat]!.toList.filter (! · == '.'))
  s2 == ["S", "S"]

#assert onlyS (← IO.FS.lines input)

/-- `findUnreachable dat` finds all locations of the centres of

 `#`
`#.#`
 `#`

on the layout determined by `dat`.
They turn out to be the only inaccessible locations on the garden that are marked with a `.`.
-/
def findUnreachable (dat : Array String) : Array pos :=
  let rks := getRocks dat
  Id.run do
  let mut out := #[]
  for i in [:dat.size] do
    for j in [:dat.size] do
      let p : pos := (i, j)
      let cond := (dirs.map (rks.find? <| toCCWPos · + p)).all Option.isSome
      if (rks.find? p).isNone ∧ cond then
        out := out.push p
  return out

#assert findUnreachable atest == #[]
#assert findUnreachable (← IO.FS.lines input) == #[(5, 31), (59, 91), (61, 68), (79, 78), (111, 116)]

def draw2 (dat : Array String) (mh : Nat) : Array String × Nat :=
  let unr := findUnreachable dat
  let S := findS dat
  let rk1 := findRk dat
  let r := getRocks dat
  let g := parts dat mh (fun p@(x, y) =>
    if max (x- S.1).natAbs (y- S.2).natAbs ≤ mh then p else rk1)
  Id.run do
  let mut tot := 0
  let mut rows := #[]
  for i in [:130] do
    let mut row := ""
    for j in [:130] do
      let p : pos := (i, j)
      let pd := p - (65, 65)
      tot := tot + 1
      if mh < pd.1.natAbs + pd.2.natAbs then row := row.push ' '; tot := tot - 1 else
      if i == S.1 && j = S.2 then row := row.push 'S' else
      if (r.find? p).isSome then row := row.push '#' else
      if (g.find? p).isSome then row := row.push 'O' else
      if p ∈ unr then row := row.push 'U' else
      row := row.push '.'
    rows := rows.push row
  return (rows, tot)

#check Nat.binom

#eval show MetaM _ from do
  for mh in [8:12] do --let mh := 4
    let dat ← IO.FS.lines input
    let S := findS dat
    let rks := getRocks dat
    let rk1 := findRk dat
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
--    let res := parts dat mh (fun p@(x, y) =>
--      if max (x- S.1).natAbs (y- S.2).natAbs ≤ (dat.size / 2) then p else rk1)
    let (dh, val) := draw2 dat mh
    if ↑val != 4 * Nat.binom (mh + 1) 2 + 1 + if mh = 2 then 1 else 0 then IO.println s!"ERROR!!! {mh}"
--  IO.println s!"{val} = {4 * Nat.binom (mh + 1) 2 + 1}\n"
    draw <| (draw2 dat mh).1

#eval show MetaM _ from do
  for mh in [85] do --let mh := 4
    let dat ← IO.FS.lines input
    let S := findS dat
    let rks := getRocks dat
    let rk1 := findRk dat
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [2 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [3 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [4 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [5 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [6 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [7 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [8 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [9 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [10 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#eval show MetaM _ from do
  for mh in [11 * 130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
    IO.println (res.toArray.filter fun (x, y) => (x + y) % 2 == 1).size

#exit
#eval show MetaM _ from do
  for mh in [130 + 85] do --let mh := 4
    let dat ← IO.FS.lines input
    let S := findS dat
    let rks := getRocks dat
    let rk1 := findRk dat
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
--    let res := parts dat mh (fun p@(x, y) =>
--      if max (x- S.1).natAbs (y- S.2).natAbs ≤ (dat.size / 2) then p else rk1)
    let (dh, val) := draw2 dat mh
--  IO.println s!"{val} = {4 * Nat.binom (mh + 1) 2 + 1}\n"
    draw <| dh

#eval show MetaM _ from do
  for mh in [85] do --let mh := 4
    let dat ← IO.FS.lines input
    let S := findS dat
    let rks := getRocks dat
    let rk1 := findRk dat
    let res := parts dat mh (fun (x, y) => (x % dat.size, y % dat.size))
--    let res := parts dat mh (fun p@(x, y) =>
--      if max (x- S.1).natAbs (y- S.2).natAbs ≤ (dat.size / 2) then p else rk1)
    let (dh, val) := draw2 dat mh
--  IO.println s!"{val} = {4 * Nat.binom (mh + 1) 2 + 1}\n"
    draw <| toPic res.toArray 170 170 --dh



/-!  This code computes the steps that the gardener can take in a single fundamental domain, assuming
that they can move in there for as long as they want.
-/

/-
#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let totRks : Nat := (dat.map fun s : String => (s.toList.filter (· == '#')).length).sum
  let S := findS dat
  let rk1 := findRk dat
  let res := parts dat 129 (fun p@(x, y) =>
    if max (x- S.1).natAbs (y- S.2).natAbs ≤ (dat.size / 2) then p else rk1)
  IO.println <| s!"{res.size + totRks} vs {(dat.map String.length).sum}"
  let top := toPic res.toArray dat.size dat.size
--  draw <| top
--  draw dat
  let mut out := #[]
  for i in [:dat.size] do
    let di := dat[i]!
    let gi := top[i]!
    for j in [:dat.size] do
      if di.get! ⟨j⟩ == '.' ∧ gi.get! ⟨j⟩ == '.' then out := out.push (i, j); IO.println (i, j)
  let pars := [0, 1].map fun i : Nat => (res.toArray.filter fun ((x, y) : pos) => (x + y).toNat % 2 == i).size
  IO.println <| s!"even: {pars[0]!} odd: {pars[1]!}"

-- (5, 31)
-- (59, 91)
-- (61, 68)
-- (79, 78)
-- (111, 116)
-- even: 7645 odd: 7576
129 finds all odd positions
130 finds all even positions
--/

def eoAccessible (dat : Array String) : Nat × Nat :=
  Id.run do
  let mut rks := (getRocks dat '.').insert (findS dat)
  for i in findUnreachable dat do
    rks := rks.erase i
  let (ev, odd) := rks.toArray.partition fun (x, y) => (x + y) % 2 == 0
  (ev.size, odd.size)

#assert eoAccessible (← IO.FS.lines input) == (7645, 7576)

#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let totRks : Nat := (dat.map fun s : String => (s.toList.filter (· == '#')).length).sum
  let S := findS dat
  let rk1 := findRk dat
  let res := parts dat 150 (fun p@(x, y) =>
    if max (x- S.1).natAbs (y- S.2).natAbs ≤ (dat.size / 2) then p else rk1)
  IO.println <| s!"{res.size + totRks} vs {(dat.map String.length).sum}"
  let top := toPic res.toArray dat.size dat.size
  draw <| top
  draw dat
  let mut out := #[]
  for i in [:dat.size] do
    let di := dat[i]!
    let gi := top[i]!
    for j in [:dat.size] do
      if di.get! ⟨j⟩ == '.' ∧ gi.get! ⟨j⟩ == '.' then out := out.push (i, j); IO.println (i, j)
  let pars := [0, 1].map fun i : Nat => (res.toArray.filter fun ((x, y) : pos) => (x + y).toNat % 2 == i).size
  IO.println <| s!"even: {pars[0]!} odd: {pars[1]!}"



#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let S := findS dat
  let rk1 := findRk dat
  let low := 150
  for i in [low] do
    let res := parts dat i (fun p@(x, y) =>
      if max (x- S.1).natAbs (y- S.2).natAbs ≤ (dat.size / 2) then p else rk1)
    draw <| toPic (res.toArray.map (· + ((1 : Int), (1 : Int)))) (dat.size + 2) (dat.size + 2)
  draw dat


/-- given the locations of the rocks and the possible locations of the gardener
returns the locations where the gardeners could be in one more step. -/
def mvsF (rk gd : HashSet pos) (f : pos → Bool) : HashSet pos :=
  Id.run do
  let mut new : HashSet pos := gd
  for g in gd do
    let mvs := (dirs.map (toPos · + g)).filter fun x => f x && (rk.find? x).isNone
    for m in mvs do
      new := new.insert m
  return new

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def partF (dat : Array String) (f : pos → Bool) (n : Nat := 64) : Nat :=
  let rk := getRocks dat
  Id.run do
  let mut gd : HashSet pos := HashSet.empty.insert (findS dat)
  for _ in [:n] do
    gd := (mvsF rk gd f)
  return gd.size

def partFUpto (dat : Array String) (f : pos → Bool) : Nat × HashSet pos :=
  let rk := getRocks dat
  Id.run do
  let init := findS dat
  let mut boundary := #[init]
  let mut gd : HashSet pos := HashSet.empty.insert (init)
  let mut steps := 0
  let mut (past, curr) := (0, gd.size)
  while past ≠ curr do
    steps := steps + 1
    past := curr
    gd := (mvsF rk (mvsF rk gd f) f)
    curr := gd.size
  return (2 * (steps - 1), gd)

--  determine the number of steps and the number of locations that
--  the gardener can visit while staying in a single fundamental
--  domain for the L¹-norm.
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let res := partFUpto dat (fun (x, y) => x.natAbs + y.natAbs ≤ 65)
--  IO.println res

--  determine the number of steps and the number of locations that
--  the gardener can visit while staying in a single fundamental
--  domain for the L¹-norm.
#eval do
  let dat := atest
  let dat ← IO.FS.lines input
  let S := findS dat
  let res := partFUpto dat (fun (x, y) => max (x.natAbs- S.1) (y.natAbs- S.2) ≤ 6)
--  draw <| toPic res.2.toArray dat.size dat.size
--  IO.println (res.1, res.2.size)


#eval 26501365 % 130

#eval
  let steps := 26501365
  let (q, r) := (steps / 130, steps % 130)
  (q, r, q * 130 + r == steps)

#eval
  let steps := 26501365
  let (q, r) := (steps / 65, steps % 65)
  (q, r, q * 65 + r == steps)

#eval do
  let dat ← IO.FS.lines input
  draw dat

/-
After 74 steps, the gardener visits `4206` places in the garden.
This is the maximum number of places that gardener can visit in a single fundamental domain.
-/

/-
#eval do
  let dat ← IO.FS.lines input
  let dat := atest
  let res := partF dat (fun (x, y) => x.natAbs + y.natAbs ≤ 65) 74
  IO.println res
-/

--def fillUp (dat : Array String)

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2
