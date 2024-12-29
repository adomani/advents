import Advents.Utils
open Lean

namespace Day21

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day21.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "Player 1 starting position: 4
Player 2 starting position: 8"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

structure State where
  p1 : Nat
  score1 : Nat := 0
  p2 : Nat
  score2 : Nat := 0
  round : Nat := 1
  deriving Inhabited

def inputToState (dat : String) : State :=
  match dat.getNats with
    | [1, p1, 2, p2] => {p1 := p1, p2 := p2}
    | _ => panic "Malformed input!"

def showState (s : State) : IO Unit := do
  IO.println s!"next roll: {s.round}\nP1: pos, score: ({s.p1}, {s.score1})\nP2: pos, score: ({s.p2}, {s.score2})\n"

#eval do
  let dat := test
  let dat ← IO.FS.readFile input
  let s := inputToState dat
  showState s

def red (n : Nat) : Nat := (n - 1) % 10 + 1

def deterministicRoll (s : State) : State :=
  let die := 3 * s.round - 2
  let dieRolls := red die + red (die + 1) + red (die + 2)
  let (p1, p2) :=
    if s.round % 2 == 1 then
      (red (s.p1 + dieRolls), s.p2)
    else
      (s.p1, red (s.p2 + dieRolls))
  let (score1, score2) :=
    if s.round % 2 == 1
  then
    (s.score1 + red p1, s.score2)
  else
    (s.score1, s.score2 + red p2)
  {s with
    p1 := p1
    p2 := p2
    score1 := score1
    score2 := score2
    round := s.round + 1}

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let mut s := inputToState dat
  while max s.score1 s.score2 < 1000 do
    s := deterministicRoll s
  let losingScore := if s.round % 2 == 1 then s.score1 else s.score2
  let dieRolls := (s.round - 1) * 3
  losingScore * dieRolls

#assert part1 test == 739785

solve 1 906093 file

/-!
#  Question 2
-/

structure QuantumState where
  p1 : Nat
  score1 : Nat := 0
  p2 : Nat
  score2 : Nat := 0
  p1? : Bool := true
  deriving Inhabited, BEq, Hashable

def inputToQuantumState (dat : String) : QuantumState :=
  match dat.getNats with
    | [1, p1, 2, p2] => {p1 := p1, p2 := p2}
    | _ => panic "Malformed input!"

def showQuantumState (s : QuantumState) : IO Unit := do
  IO.println s!"next roll: {if s.p1? then "P1" else "P2"}\nP1: pos, score: ({s.p1}, {s.score1})\nP2: pos, score: ({s.p2}, {s.score2})\n"

def quantumRoll (s : QuantumState) : Array QuantumState := Id.run do
  let mut qs := #[]
  for dieRolls in #[1, 2, 3] do
    --dbg_trace "rolled {dieRolls}"
    let (p1, p2) :=
      if s.p1? then
        (red (s.p1 + dieRolls), s.p2)
      else
        (s.p1, red (s.p2 + dieRolls))
    let (score1, score2) :=
      if s.p1?
    then
      (s.score1 + red p1, s.score2)
    else
      (s.score1, s.score2 + red p2)
    qs := qs.push {
      p1 := p1
      p2 := p2
      score1 := score1
      score2 := score2
      p1? := !s.p1?}
  return qs

instance : HMul Nat (Nat × Nat) (Nat × Nat) where hMul a p := (a * p.1, a * p.2)

partial
def wins (q : QuantumState) (memo : Std.HashMap QuantumState (Nat × Nat)) :
    (Nat × Nat) × Std.HashMap QuantumState (Nat × Nat) :=
  -- If we cached the value, then retrieve it.
  if let some v := memo[q]? then (v, memo) else
  -- Otherwise, the value is not cached.  If we can determine who wins, then let's do it!
  if 21 ≤ max q.score1 q.score2 then
    let new := if q.p1? then (0, 1) else (1, 0)
    (new, memo) --.insert q new)
  -- Otherwise, the value is not cached and we still need to roll the die: let's roll it!
  else Id.run do
    let mut memo := memo
    let mut vals := (0, 0)
    for q' in quantumRoll q do
      let (v', m') := wins q' memo
      let v3 := 3 * v'
      let v3 := v'
      vals := vals + v3
      memo := m' |>.insert q' v3
    return (vals, memo)

#eval do
  let dat ← IO.FS.readFile input
  let dat := test
  --let mut s := {inputToQuantumState dat with p1 := 0, p2 := 6}
  let mut s := inputToQuantumState dat
  let (res, cache) := wins s ∅
  IO.println s!"{res}, cached {cache.size} entries"
  --for (q, c) in cache do
  --  IO.println s!"cached {c}"
  --  showQuantumState q
#exit
45336756894, 18455834679
128043950504863 65210564232228

534611412180896280 310347373238861569 -- <--- 100
444356092776315 341960390180808
457427060346843

  let mut ss := #[]
  --let mut con := 0
  for i in [0:1] do
  --while max s.score1 s.score2 < 21 do
  --showState s
    ss := quantumRoll s
    --con := con + 1
  --showState s
  --s := deterministicRoll s
  showQuantumState s
  for t in ss do
    showQuantumState t
  --let losingScore := if s.round % 2 == 1 then s.score1 else s.score2
  --let dieRolls := (s.round - 1) * 3
  --IO.println <| (losingScore, dieRolls, losingScore * dieRolls)
  --s := deterministicRoll s
  --showState s
-- 906093
-- 916083  -- too high
-- 918834  -- too high
-- 921585  -- too high

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day21
