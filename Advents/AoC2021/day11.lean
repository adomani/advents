import Advents.Utils
open Lean

namespace Day11

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day11.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
`OctoState` is the main structure keeping track of the state of the octopuses.

* `state` represents current energy levels of the octopuses;
* `flashed` are the octopuses that in this round have already emitted a flash;
* `preflashed` are the octopushes that will emit a flash, but have not yet done so and hence have
  not yet updated the energy level of their neighbours;
* `flashes` is the count of the total number of flashes, since this state started tracking the
  octopuses.
-/
structure OctoState where
  /-- `state` represents current energy levels of the octopuses.-/
  state      : Std.HashMap pos Nat
  /-- `flashed` are the octopuses that in this round have already emitted a flash.-/
  flashed    : Std.HashSet pos := {}
  /-- `preflashed` are the octopushes that will emit a flash, but have not yet done so and
  hence have not yet updated the energy level of their neighbours. -/
  preflashed : Std.HashSet pos := {}
  /-- `flashes` is the count of the total number of flashes, since this state started tracking
  the octopuses. -/
  flashes    : Nat := 0
  deriving Inhabited

/-- Converts the input array of strings into an initial `OctoState`. -/
def inputToOctoState (o : Array String) : OctoState := Id.run do
  let mut h := {}
  let mut con := 0
  for d in o do
    let mut rcon := 0
    for c in d.toList do
      h := h.insert (con, rcon) ("".push c).toNat!
      rcon := rcon + 1
    con := con + 1
  return {state := h}

/-- The neighbours of a given octopus.  Some of these neighbours may be outside of the grid. -/
abbrev nbs (i : pos) : Array pos :=
  #[(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)].map (i + ·)

/--
`dealWithPreflashedOnce` changes the `OctoState` by increasing the energy level of all
the octopuses that are neighbours of the current `preflashed` octopuses, clearing the
old `preflashed` and updating `flashed` and `preflashed` accordingly.
This may leave a non-empty `preflashed` state, since it only deals with the *current* preflashed.
`dealWithPreflashed` performs the update recursively.
-/
def dealWithPreflashedOnce (st : OctoState) : OctoState := Id.run do
  let mut newState := st.state
  let mut preflashed := st.preflashed
  let mut flashed := st.flashed
  for p in preflashed do
    if flashed.contains p then continue
    flashed := flashed.insert p
    preflashed := preflashed.erase p
    for n in nbs p do
      if flashed.contains n then continue
      if let some v := newState.get? n then
        newState := newState.insert n (v + 1)
        if 9 ≤ v then preflashed := preflashed.insert n
      else continue
  return {state := newState, flashed := flashed, preflashed := preflashed, flashes := st.flashes}

/--
Given an `OctoState`, propagate the `preflashed` state recursively,
until `preflashed` becomes empty.
-/
partial
def dealWithPreflashed (st : OctoState) : OctoState :=
  if st.preflashed.isEmpty then st else
  let new := dealWithPreflashedOnce st
  dealWithPreflashed new

/-- Draw the `state` of the `OctoState` -- just needed for pretty pictures, no actual content. -/
def drawHash (h : Std.HashMap pos Nat) (Nx Ny : Nat) : Array String := Id.run do
  let mut fin := #[]
  for i in [0:Nx] do
    let mut str := ""
    for j in [0:Ny] do
      match h.get? (i, j) with
        | some d => str := str ++ s!"{d}"
        | none => str := str.push '·'
    fin := fin.push str
  return fin

/--
Assumes that `preflashed` and `flashed` are both empty. Increases by `1` every energy level,
updating the `preflashed` state as necessary.
-/
def increaseByOne (st : OctoState) : OctoState :=
  st.state.fold (init := st) fun st p o =>
    let preft := if o == 9 then st.preflashed.insert p else st.preflashed
    {state := st.state.insert p (o + 1), preflashed := preft, flashes := st.flashes}

/--
Assumes that `preflashed` and `flashed` are both empty.
Scans the `state` for energy levels above `9`, resets them to `0` and increases
the total `flashes` counter by the number of positions that it reset.
-/
def resetState (st : OctoState) : OctoState :=
  st.state.fold (init := st) fun st p o =>
    if 10 ≤ o then {st with state := st.state.insert p 0, flashes := st.flashes + 1}
    else st

/--
Assumes that `preflashed` and `flashed` are both empty.
A single step:
* increase by `1` the energy levels,
* propagate `preflashed` and `flashed`,
* reset the energy levels, updating the count of `flashes`.
-/
def stepAndFlash (st : OctoState) : OctoState :=
  resetState <| dealWithPreflashed <| increaseByOne st

/-- The iterated version of `stepAndFlash`, where we can specify the number of steps that
we want to perform. -/
def stepAndFlashMany (st : OctoState) : Nat → OctoState
  | 0 => st
  | n + 1 => stepAndFlashMany (stepAndFlash st) n

-- The commented code shows diagrams of the energy levels of the octopuses.
-- It is not needed for the solution.
/-
#eval do
  let st := inputToOctoState (← IO.FS.lines input)
  draw <| drawHash st.state 10 10
  let newOcto := stepAndFlashMany st 100
  IO.println newOcto.flashes
  draw <| drawHash newOcto.state 10 10
-/

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let newOcto := stepAndFlashMany (inputToOctoState dat) 100
  newOcto.flashes

#assert part1 atest == 1656

solve 1 1686

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let mut fl := 0
  let mut st := inputToOctoState dat
  let mut cond := true
  let mut i := 0
  while cond do
    i := i + 1
    st := stepAndFlash st
    cond := (st.flashes != fl + 100)
    fl := st.flashes
  return i

#assert part2 atest == 195

solve 2 360

end Day11
