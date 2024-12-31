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
  deriving BEq, DecidableEq, Hashable, Inhabited

instance : ToString AP where toString
  | .A => "A" | .B => "B" | .C => "C" | .D => "D"

def CharToAP : Char → Option AP
  | 'A' => some .A
  | 'B' => some .B
  | 'C' => some .C
  | 'D' => some .D
  | _ => none

structure Burrow where
  ap : Array (pos × AP)
  unmovable : Array pos := ∅
  energy : Nat := 0
  deriving Inhabited, BEq, Hashable

def AP.index : AP → Nat
  | .A => 0
  | .B => 1
  | .C => 2
  | .D => 3

instance : LT AP where lt a b := a.index < b.index

def AP.roomColumn (ap : AP) : Nat := ap.index * 2 + 3

#assert #[AP.A, AP.B, AP.C, AP.D].map (·.roomColumn) == #[3, 5, 7, 9]

def AP.times (ap : AP) : Nat := 10 ^ ap.index

#assert #[AP.A, AP.B, AP.C, AP.D].map (·.times) == #[1, 10, 100, 1000]

structure FBurrow where
  all : Std.HashMap pos Char
  grid : Std.HashSet pos
  ap : Std.HashMap pos AP
  unmovable : Std.HashSet pos := ∅
  energy : Nat := 0
  deriving Inhabited

def Burrow.all := loadGrid atest (fun c => if "ABCD".toList.contains c then '.' else c)
/--
info: --0123456789012-
0|#############|
1|#...........#|
2|###.#.#.#.###|
3|  #.#.#.#.#  |
4|  #########  |
--0123456789012-
-/
#guard_msgs in
#eval draw <| drawHash Burrow.all 5 13

def Burrow.grid := sparseGrid atest ("ABCD.".toList.contains ·)
/--
info: --0123456789012-
0|#############|
1|#...........#|
2|###.#.#.#.###|
3|###.#.#.#.###|
4|#############|
--0123456789012-
-/
#guard_msgs in
#eval draw <| drawSparse Burrow.grid 5 13 "." "#"

def Alt (b c : AP) : Bool := b.index < c.index

def Blt (b c : pos × AP) : Bool :=
  Alt b.2 c.2 || (b.2 == c.2 && b.1 < c.1)

def inputToBurrow (dat : Array String) : Burrow where
  ap := sparseMap dat CharToAP |>.toArray.qsort Blt

def drawBurrow (br : Burrow) : IO Unit := do
  let all := Burrow.all
  let bur := (Std.HashMap.insertMany (∅ : Std.HashMap pos AP) br.ap).fold (init := all)
    fun h p c => h.insert p <| s!"{c}".get 0
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

def unsafeMove (br : Burrow) (p q : pos) : Option Burrow :=
  let newPos := p + q
  match Burrow.grid.contains newPos, br.ap.find? (·.1 == p) with
    | true, some f@(_, ap) => some { br with  ap := ((br.ap.erase f).push (newPos, ap)).qsort Blt
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
  checkOr (br.ap.find? (·.1 == newPos)).isNone s!"The new position {newPos} already contains an amphipod" v? &&
  -- the hallway and room are clear
  ( let hall := (Array.range q.2.natAbs).filterMap fun v =>
        let x : pos := (1, p.2 + q.2.sign * (v.cast + 1))
        br.ap.find? (·.1 == x)
    checkOr
      hall.isEmpty
      s!"The amphipod cannot walk through the hall: {hall.getD 0 default} is occupied" v? &&

    let room := (Array.range q.1.natAbs).filterMap fun v =>
      let x : pos := (p.1 + q.1.sign * (v.cast + 1), p.2)
      br.ap.find? (·.1 == x)
    checkOr
      room.isEmpty
      s!"The amphipod cannot walk through the room: {room.getD 0 default} is occupied" v?) &&
  match br.ap.find? (·.1 == p) with
    -- the starting position must contain an amphipod
    | none => checkOr false s!"The starting position {p} must contain an amphipod" v?
    | some (_, ap) =>
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
        match Burrow.grid.contains posOneBelow, br.ap.find? (·.1 == posOneBelow) with
          | false, _ => true
          | true, none => checkOr false s!"The room position {posOneBelow} would be empty." v?
          | _, some (_, apBelow) =>
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

/--
info: The amphipod cannot walk through the hall: ((1, 5), D) is occupied
false
The amphipod cannot walk through the room: ((2, 3), B) is occupied
false
--0123456789012-
0|#############|
1|#....D......#|
2|###B#C#B#D###|
3|  #A#.#C#A#  |
4|  #########  |
--0123456789012-

Energy: 2000
-/
#guard_msgs in
#eval do
  let dat := atest
  let br := inputToBurrow dat
  let br := unsafeMove br (3, 5) (- 2, 0) |>.get!
  IO.println <| canWalkthrough br (2, 7) (- 1, -3) true
  IO.println <| canWalkthrough br (3, 3) (- 2, 0) true
  drawBurrow br

/--
info: The amphipod B at (2, 3) can move to (1, 2)
The amphipod B at (2, 3) can move to (1, 4)
The amphipod B at (2, 3) can move to (1, 1)
The amphipod B at (2, 7) can move to (1, 6)
The amphipod B at (2, 7) can move to (1, 11)
The amphipod B at (2, 7) can move to (1, 10)
The amphipod B at (2, 7) can move to (1, 8)
The amphipod D at (2, 9) can move to (1, 6)
The amphipod D at (2, 9) can move to (1, 11)
The amphipod D at (2, 9) can move to (1, 10)
The amphipod D at (2, 9) can move to (1, 8)
true
--0123456789012-
0|#############|
1|#....D......#|
2|###B#C#B#D###|
3|  #A#.#C#A#  |
4|  #########  |
--0123456789012-

Energy: 2000
-/
#guard_msgs in
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
The amphipod B at (2, 3) can move to (1, 2)
The amphipod B at (2, 3) can move to (1, 4)
The amphipod B at (2, 3) can move to (1, 6)
The amphipod B at (2, 3) can move to (1, 1)
The amphipod B at (2, 7) can move to (1, 2)
The amphipod B at (2, 7) can move to (1, 4)
The amphipod B at (2, 7) can move to (1, 6)
The amphipod B at (2, 7) can move to (1, 1)
The amphipod C at (2, 5) can move to (1, 2)
The amphipod C at (2, 5) can move to (1, 4)
The amphipod C at (2, 5) can move to (1, 6)
The amphipod C at (2, 5) can move to (1, 1)
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
  match Burrow.grid.contains newPos, br.ap.find? (·.1 == p) with
    | true, some f@(_, ap) => some { br with
      ap        := (br.ap.erase f).push (newPos, ap) |>.qsort Blt
      unmovable := if newPos.inRoom then (br.unmovable.push newPos).qsort (· < ·) else br.unmovable
      energy    := br.energy + ap.times * dist q }
    | _, _ => none

def validMoves (br : Burrow) : Std.HashSet Burrow := Id.run do
  let mut fin := ∅
  for (p, _) in br.ap do
    for newP in Burrow.grid do
      if canWalkthrough br p (newP - p) then
        fin := fin.insert <| (move br p (newP - p)).get!
  return fin

def isFinal (br : Burrow) (bd : Option Nat := some 13000) : Bool :=
  (if let some bd := bd then bd ≤ br.energy else false) ||
    (br.ap.filter (fun (p, ap) => (!ap.inOwnRoom p))).isEmpty

#assert isFinal (inputToBurrow atestFinal)
#assert !isFinal (inputToBurrow atest)

/-
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
-/

/--
info: --0123456789012-
0|#############|
1|#...........#|
2|###D#A#D#C###|
3|  #B#C#B#A#  |
4|  #########  |
--0123456789012-

Energy: 0
Step 16

--0123456789012-
0|#############|
1|#...........#|
2|###A#B#C#D###|
3|  #A#B#C#D#  |
4|  #########  |
--0123456789012-

Energy: 14728
-/
#guard_msgs in
#eval do
  let dat ← IO.FS.lines input
  let mut br := inputToBurrow dat
  drawBurrow br
  let mvs : Array (pos × pos) := #[
       ((2,  5), (-1, -3)) -- A
      ,((2,  9), (-1, -3)) -- C
      ,((3,  9), (-2,  1)) -- A
      ,((2,  7), (-1,  1)) -- D
      ,((1,  8), ( 2,  1)) -- D
      ,((3,  7), (-2,  1)) -- B
      ,((1,  6), ( 2,  1))  -- C
      ,((3,  5), (-2,  1)) -- C
      ,((1,  6), ( 1,  1)) -- C
      ,((1,  8), ( 2, -3)) -- B
      ,((2,  3), (-1,  1)) -- D
      ,((1,  4), ( 1,  5)) -- D
      ,((3,  3), (-2,  1)) -- B
      ,((1,  4), ( 1,  1)) -- B
      ,((1,  2), ( 2,  1)) -- A
      ,((1, 10), ( 1, -7)) -- A
    ]
  for (p, q) in mvs do
    match move br p q with
      | none => IO.println s!"Invalid move {p} {q}!"
      | some b => br := b
  IO.println s!"Step {mvs.size}\n"
  drawBurrow br

#exit
set_option trace.profiler true in
/-
--0123456789012-
0|#############|
1|#...........#|
2|###B#C#B#D###|
3|  #A#D#C#A#  |
4|  #########  |
--0123456789012-

Energy: 0
Step 0: 28, of which 0 final, fin': 13000
Step 1 (after pruning): 28, of which 0 final, fin': 13000
Step 1: 371, of which 0 final, fin': 13000
Step 2 (after pruning): 355, of which 0 final, fin': 13000
Step 2: 2521, of which 33 final, fin': 13000
Step 3 (after pruning): 2227, of which 33 final, fin': 13000
Step 3: 9597, of which 246 final, fin': 13000
Step 4 (after pruning): 7646, of which 246 final, fin': 13000
Step 4: 22052, of which 792 final, fin': 13000
Step 5 (after pruning): 15275, of which 792 final, fin': 13000
Step 5: 15275, of which 792 final, fin': 13000

-/
#eval do
  let dat := atest
  let br := inputToBurrow dat
  let mut brs : Std.HashSet Burrow := {br}
  let mut final : Std.HashSet Burrow := ∅
  drawBurrow br
  let mut con := 0
  let mut fin' := 13000
  while con ≤ 4 do
    fin' := final.fold (init := fin') (min · <| Burrow.energy ·)
    let (final', brs') :=
      (brs.fold (init := ∅) fun (h : Std.HashSet Burrow) n =>
        h.union (validMoves n)).partition (isFinal · fin')
    final := final.union final'
    --brs := uniquify brs'
    brs := brs'
    IO.println s!"Step {con}: {brs.size + final.size}, of which {final.size} final, fin': {fin'}"
    con := con + 1
    let mut left := brs
    for a in brs do
      left := left.erase a
      for b in left do
        if a.ap == b.ap then
          if a.energy ≤ b.energy then brs := brs.erase b else brs := brs.erase a
    IO.println s!"Step {con} (after pruning): {brs.size + final.size}, of which {final.size} final, fin': {fin'}"
      --let mut cand := a
      --left := left.erase a
      --let fd := left.filter fun n : Burrow => (n.ap == a.ap && n.energy ≤ a.energy)
      --
      --if !fd.isEmpty then
      --  IO.println s!"Found {fd.size}"
  IO.println s!"Step {con}: {brs.size + final.size}, of which {final.size} final, fin': {fin'}"

#exit
  for b in brs.union final do
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
