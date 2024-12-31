import Advents.Utils
open Lean

namespace Day23

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day23.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `testFinal` is the test string for the final configuration of the problem. -/
def testFinal := "#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########"

/-- `atestFinal` is the final test string for the problem, split into rows. -/
def atestFinal := (testFinal.splitOn "\n").toArray

inductive AP where | A | B | C | D
  deriving BEq, DecidableEq, Hashable

instance : ToString AP where toString
  | .A => "A" | .B => "B" | .C => "C" | .D => "D"

def CharToAP : Char → Option AP
  | 'A' => some .A
  | 'B' => some .B
  | 'C' => some .C
  | 'D' => some .D
  | _ => none

structure Burrow where
  ap : Std.HashMap pos AP
  unmovable : Std.HashSet pos := ∅
  energy : Nat := 0
  deriving Inhabited

structure FBurrow where
  all : Std.HashMap pos Char
  grid : Std.HashSet pos
  ap : Std.HashMap pos AP
  unmovable : Std.HashSet pos := ∅
  energy : Nat := 0
  deriving Inhabited

def Burrow.all := loadGrid atest (fun c => if "ABCD".toList.contains c then '.' else c)
#eval draw <| drawHash Burrow.all 5 13

def Burrow.grid := sparseGrid atest ("ABCD.".toList.contains ·)
#eval draw <| drawSparse Burrow.grid 5 13 "." "#"

def inputToBurrow (dat : Array String) : Burrow where
  ap   := sparseMap dat CharToAP

def drawBurrow (br : Burrow) : IO Unit := do
  let all := Burrow.all
  let bur := br.ap.fold (init := all) fun (h : Std.HashMap _ _) p c => h.insert p <| s!"{c}".get 0
  let (mx, my) := Burrow.all.fold (fun (mx, my) (x, y) _ => (max mx x.natAbs, max my y.natAbs)) (0, 0)
  draw <| drawHash bur (mx + 1) (my + 1)
  IO.println s!"Energy: {br.energy}"

/-- info:
--0123456789012-
0|#############|
1|#...........#|
2|###B#C#B#D###|
3|  #A#D#C#A#  |
4|  #########  |
--0123456789012-

Energy: 0
-/
#guard_msgs in
#eval drawBurrow (inputToBurrow atest)

/-- The L¹-distance in the plane. -/
def dist (p : pos) : Nat := p.1.natAbs + p.2.natAbs

def AP.index : AP → Nat
  | .A => 0
  | .B => 1
  | .C => 2
  | .D => 3

def AP.roomColumn (ap : AP) : Nat := ap.index * 2 + 3

#assert #[AP.A, AP.B, AP.C, AP.D].map (·.roomColumn) == #[3, 5, 7, 9]

def AP.times (ap : AP) : Nat := 10 ^ ap.index

#assert #[AP.A, AP.B, AP.C, AP.D].map (·.times) == #[1, 10, 100, 1000]

def unsafeMove (br : Burrow) (p q : pos) : Option Burrow :=
  let newPos := p + q
  match Burrow.grid.contains newPos, br.ap[p]? with
    | true, some ap => some { br with ap := (br.ap.erase p).insert newPos ap
                                      energy := br.energy + ap.times * dist q }
    | _, _ => none

def _root_.pos.inHall (p : pos) : Bool := p.1 == 1
def _root_.pos.inRoom (p : pos) : Bool := p.1 == 2 || p.1 == 3
def _root_.pos.aboveRoom (p : pos) : Bool := p.inHall && #[3, 5, 7, 9].contains p.2

def AP.inOwnRoom (ap : AP) (p : pos) : Bool :=
  p.inRoom && p.2 == ap.roomColumn

def checkOr (c : Bool) (s : String) (v? : Bool) :=
  if v? then
    if c then c else dbg_trace s; c
  else c

def canWalkthrough (br : Burrow) (p q : pos) (v? : Bool := false) : Bool :=
  let newPos := p + q
  -- the new position is in the grid
  checkOr (Burrow.grid.contains newPos) s!"The new position {newPos} is not in the grid" v? &&
  -- the new position does not already contain an amphipod
  checkOr (!br.ap.contains newPos) s!"The new position {newPos} already contains an amphipod" v? &&
  -- the hallway and room are clear
  ( let hall := (Array.range q.2.natAbs).filterMap fun v =>
        let x : pos := (1, p.2 + q.2.sign * (v.cast + 1))
        if br.ap.contains x then some x else none
    checkOr
      hall.isEmpty
      s!"The amphipod cannot walk through the hall: {hall.getD 0 default} is occupied" v? &&

    let room := (Array.range q.1.natAbs).filterMap fun v =>
      let x : pos := (p.1 + q.1.sign * (v.cast + 1), p.2)
      if br.ap.contains x then some x else none
    checkOr
      room.isEmpty
      s!"The amphipod cannot walk through the room: {room.getD 0 default} is occupied" v?) &&
  match br.ap[p]? with
    -- the starting position must contain an amphipod
    | none => checkOr false s!"The starting position {p} must contain an amphipod" v?
    | some ap =>
      -- amphipods never stop in the hall directly above a room
      checkOr (!newPos.aboveRoom) "Amphipods never stop in the hall directly above a room" v? &&
      -- amphipods do not move out of their own room, once they get back
      checkOr (!br.unmovable.contains q) s!"The amphipod {ap} at {p} is back into its room" v? &&
      -- amphipods only move from a room that is not their own to the hall or
      -- from the hall to a room that is their own
      ( if !((p.inRoom && newPos.inHall) || (p.inHall && newPos.inRoom && ap.inOwnRoom newPos))
        then
          checkOr
            (p.inRoom && newPos.inHall) -- && !ap.inOwnRoom p)
            "Amphipods only move from a room that is not their own to the hall" v? ||
          checkOr (p.inHall && newPos.inRoom && ap.inOwnRoom newPos)
            "Amphipods only move from the hall to a room that is their own" v?
        else true) &&
      ( let posOneBelow := newPos + (1, 0)
        match Burrow.grid.contains posOneBelow, br.ap[posOneBelow]? with
          | false, _ => true
          | true, none => checkOr false s!"The room position {posOneBelow} would be empty." v?
          | _, some apBelow =>
            checkOr
              (apBelow.inOwnRoom posOneBelow)
              s!"Amphipod {apBelow} at {posOneBelow} would be trapped by this move!" v? )

/-
def mightAsWellMove (br : Burrow) : Array (pos × pos) := Id.run do
  let mut fin := #[]
  for (p, ap) in br.ap do
    if p.inRoom then continue
    let roomCol : pos :=
  return fin
-/

#eval do
  let dat := atest
  let br := inputToBurrow dat
  let br := unsafeMove br (3, 5) (- 2, 0) |>.get!
  IO.println <| canWalkthrough br (2, 7) (- 1, -3) true
  IO.println <| canWalkthrough br (3, 3) (- 2, 0) true
  drawBurrow br

#eval do
  let dat := atest
  let br := inputToBurrow dat
  let br := unsafeMove br (3, 5) (- 2, 0) |>.get!
  for (p, ap) in br.ap do
    for newP in Burrow.grid do
      if canWalkthrough br p (newP - p) then
        IO.println s!"The amphipod {ap} at {p} can move to {newP}"
  IO.println <| canWalkthrough br (2, 9) (- 1, 1) true
  --IO.println <| canWalkthrough br (3, 3) (- 2, 0)
  drawBurrow br

/-- info:
Amphipod A at (3, 9) would be trapped by this move! false
The amphipod A at (3, 9) can move to (1, 11)
The amphipod A at (3, 9) can move to (1, 10)
The amphipod C at (2, 5) can move to (1, 2)
The amphipod C at (2, 5) can move to (1, 4)
The amphipod C at (2, 5) can move to (1, 6)
The amphipod C at (2, 5) can move to (1, 1)
The amphipod B at (2, 3) can move to (1, 2)
The amphipod B at (2, 3) can move to (1, 4)
The amphipod B at (2, 3) can move to (1, 6)
The amphipod B at (2, 3) can move to (1, 1)
The amphipod B at (2, 7) can move to (1, 2)
The amphipod B at (2, 7) can move to (1, 4)
The amphipod B at (2, 7) can move to (1, 6)
The amphipod B at (2, 7) can move to (1, 1)
The starting position (2, 9) must contain an amphipod
false
--0123456789012-
0|#############|
1|#.......D...#|
2|###B#C#B#.###|
3|  #A#D#C#A#  |
4|  #########  |
--0123456789012-

Energy: 2000
-/
#guard_msgs in
#eval do
  let dat := atest
  let br := inputToBurrow dat
  let br := unsafeMove br (2, 9) (- 1, - 1) |>.get!
  IO.println <| canWalkthrough br (1, 8) (1, 1) true
  for (p, ap) in br.ap do
    for newP in Burrow.grid do
      if canWalkthrough br p (newP - p) then
        IO.println s!"The amphipod {ap} at {p} can move to {newP}"
  IO.println <| canWalkthrough br (2, 9) (- 1, 1) true
  --IO.println <| canWalkthrough br (3, 3) (- 2, 0)
  drawBurrow br

def move (br : Burrow) (p q : pos) : Option Burrow :=
  if !canWalkthrough br p q true then none else
  let newPos := p + q
  match Burrow.grid.contains newPos, br.ap[p]? with
    | true, some ap => some { br with
      ap        := (br.ap.erase p).insert newPos ap
      unmovable := if newPos.inRoom then br.unmovable.insert newPos else br.unmovable
      energy    := br.energy + ap.times * dist q }
    | _, _ => none

def validMoves (br : Burrow) : Array Burrow := Id.run do
  let mut fin := #[]
  for (p, _) in br.ap do
    for newP in Burrow.grid do
      if canWalkthrough br p (newP - p) then
        fin := fin.push <| (move br p (newP - p)).get!
  return fin

def isFinal (br : Burrow) : Bool :=
  13000 ≤ br.energy || (br.ap.filter (fun p ap => (!ap.inOwnRoom p))).isEmpty

#assert isFinal (inputToBurrow atestFinal)
#assert !isFinal (inputToBurrow atest)

def uniquify (brs : Array Burrow) : Array Burrow := Id.run do
  let mut brs' := brs
  let mut brs := #[]
  let mut left : Std.HashSet Nat := .ofArray <| Array.range brs'.size
  let mut used : Std.HashSet Nat := ∅
  while !left.isEmpty do
  --for ai in left do
    let ai := left.toArray[0]!
    if used.contains ai then continue
    let mut cand := brs'[ai]!
    left := left.erase ai
    let mut currFound : Std.HashSet Nat := ∅
    for bi in left do
      match brs[bi]? with
        | none => continue
        | some b =>
          left := left.erase bi
          if cand.ap.toArray.qsort (·.1 < ·.1) == b.ap.toArray.qsort (·.1 < ·.1) then
            currFound := currFound.insert bi
            cand := {cand with energy := min cand.energy b.energy}
    if !used.contains ai then
      brs := brs.push cand
    used := used.union currFound
  return brs


#eval do
  let dat := atest
  let br := inputToBurrow dat
  let mut brs := #[br]
  let mut final := #[]
  drawBurrow br
  let mut con := 0
  while con ≤ 2 do
    let (final', brs') := (brs.flatMap validMoves).partition isFinal
    final := final ++ final'
    --brs := uniquify brs'
    brs := brs'
    IO.println s!"Step {con}: {brs.size + final.size}, of which {final.size} final"
    con := con + 1
  IO.println s!"Step {con}: {brs.size + final.size}, of which {final.size} final"

  for b in brs ++ final do
    if !b.unmovable.isEmpty then drawBurrow b
  IO.println "Now low energy"
  for b in final do
    if b.energy < 13000 then IO.println b.energy
    drawBurrow b
      --else



#eval do
  let dat := atest
  let br := inputToBurrow dat
  let br := move br (2, 3) (-1, 1)
  drawBurrow br.get!


/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--set_option trace.profiler true in solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day23
