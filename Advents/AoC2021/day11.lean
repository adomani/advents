import Advents.Utils
open Lean

namespace Day11

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day11.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

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

def inputToData (o : Array String) : Std.HashMap pos Nat := Id.run do
  let mut h := {}
  let mut con := 0
  for d in o do
    let mut rcon := 0
    for c in d.toList do
      h := h.insert (con, rcon) ("".push c).toNat!
      rcon := rcon + 1
    con := con + 1
  return h

abbrev nbs (i : pos) : Array pos :=
  #[(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)].map (i + ·)

structure OctoState where
  state      : Std.HashMap pos Nat
  flashed    : Std.HashSet pos := {}
  preflashed : Std.HashSet pos := {}
  flashes    : Nat := 0
  deriving Inhabited

def dealWithPreflashedOnce (st : OctoState) : OctoState := Id.run do
  if st.preflashed.isEmpty then return st
  else --if !st.preflashed.isEmpty then
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

partial
def dealWithPreflashed (st : OctoState) : OctoState :=
  if st.preflashed.isEmpty then st else
  let new := dealWithPreflashedOnce st
  dealWithPreflashed new

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

/-- assumes that `preflashed` and `flashed` are both empty. -/
def increaseByOne (st : OctoState) : OctoState := Id.run do
  let mut newState := st.state
  let mut preflashed := {}
  for (i, o) in st.state do
    newState := newState.insert i (o + 1)
    if o == 9 then preflashed := preflashed.insert i
  return {state := newState, preflashed := preflashed, flashed := {}, flashes := st.flashes}

/-- assumes that `preflashed` and `flashed` are both empty. -/
def resetState (st : OctoState) : OctoState := Id.run do
  let mut new := st.state
  let mut fl := st.flashes
  for (p, o) in new do
    if 10 ≤ o then new := new.insert p 0; fl := fl + 1
  return {st with state := new, flashes := fl}

/-- assumes that `preflashed` and `flashed` are both empty. -/
def stepAndFlash (st : OctoState) : OctoState :=
  resetState <| dealWithPreflashed (increaseByOne st)

#eval do
  let st : OctoState := {state := inputToData atest, preflashed := {}, flashed := {}, flashes := 0}
  draw <| drawHash st.state 10 10
  let st := stepAndFlash st
  draw <| drawHash st.state 10 10
  let st := stepAndFlash st
  draw <| drawHash st.state 10 10

def stepAndFlashMany (st : OctoState) : Nat → OctoState
  | 0 => st
  | n + 1 => stepAndFlashMany (stepAndFlash st) n

#eval do
  let st : OctoState := {state := inputToData (← IO.FS.lines input), preflashed := {}, flashed := {}, flashes := 0}
  draw <| drawHash st.state 10 10
  let newOcto := stepAndFlashMany st 100
  IO.println newOcto.flashes
  draw <| drawHash newOcto.state 10 10



#eval do
  let st : OctoState := {state := inputToData atest, preflashed := {(3,9), (5,9)}, flashed := {}, flashes := 0}
  draw <| drawHash st.state 10 10
  draw <| drawHash (dealWithPreflashed st).state 10 10


def oneFlash (st : OctoState) : OctoState := Id.run do
  if st.preflashed.isEmpty && !st.flashed.isEmpty then
    let mut newState := st.state
    for p in st.flashed do
      newState := newState.insert p 0
    return {state := newState, flashed := {}, preflashed := {}, flashes := 0}
  else if st.preflashed.isEmpty && st.flashed.isEmpty then
    let mut newState := st.state
    let mut preflashed := {}
    for (i, o) in st.state do
      newState := newState.insert i (o + 1)
      if o == 9 then preflashed := preflashed.insert i
    return {state := newState, preflashed := preflashed, flashed := {}, flashes := 0}
  else --if !st.preflashed.isEmpty then
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
    return {state := newState, flashed := flashed, preflashed := preflashed, flashes := 0}
  --else
  --  return default


def step (st : Std.HashMap pos Nat) : Std.HashMap pos Nat := Id.run do
  let mut new : Std.HashMap pos Nat := {}
  let mut flashed : Std.HashSet pos := {}
  let mut preflashed : Std.HashSet pos := {}
  -- increase all values, update `preflashed`
  for (i, o) in st do
    new := new.insert i (o + 1)
    if o == 9 then preflashed := preflashed.insert i

  return {}

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let newOcto := stepAndFlashMany {state := inputToData dat} 100
  newOcto.flashes

#assert part1 atest == 1656

solve 1 1686

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day11
