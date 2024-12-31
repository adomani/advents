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

inductive AP where | A | B | C | D
  deriving BEq, Hashable

instance : ToString AP where toString
  | .A => "A" | .B => "B" | .C => "C" | .D => "D"

def CharToAP : Char → Option AP
  | 'A' => some .A
  | 'B' => some .B
  | 'C' => some .C
  | 'D' => some .D
  | _ => none

structure Burrow where
  all : Std.HashMap pos Char
  grid : Std.HashSet pos
  ap : Std.HashMap pos AP
  energy : Nat
  deriving Inhabited

def inputToBurrow (dat : Array String) : Burrow where
  all  := loadGrid dat (fun c => if "ABCD".toList.contains c then '.' else c)
  grid := sparseGrid dat ("ABCD.".toList.contains ·)
  ap   := sparseMap dat CharToAP
  energy := 0

#eval draw <| drawHash (inputToBurrow atest).all 5 13

def drawBurrow (br : Burrow) : IO Unit := do
  let all := br.all
  let bur := br.ap.fold (init := all) fun (h : Std.HashMap _ _) p c => h.insert p <| s!"{c}".get 0
  let (mx, my) := br.all.fold (fun (mx, my) (x, y) _ => (max mx x.natAbs, max my y.natAbs)) (0, 0)
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

def AP.times : AP → Nat
  | .A => 1
  | .B => 10
  | .C => 100
  | .D => 1000

def unsafeMove (br : Burrow) (p q : pos) : Option Burrow :=
  let newPos := p + q
  match br.grid.contains newPos, br.ap[p]? with
    | true, some ap => some { br with ap := (br.ap.erase p).insert newPos ap
                                      energy := br.energy + ap.times * dist q }
    | _, _ => none

def _root_.pos.inHall (p : pos) : Bool := p.1 == 1
def _root_.pos.inRoom (p : pos) : Bool := p.1 == 2 || p.1 == 3
def _root_.pos.aboveRoom (p : pos) : Bool := p.inHall && #[3, 5, 7, 9].contains p.2

def AP.inOwnRoom : AP → pos → Bool
  | .A, p => p.inRoom && p.2 == 3
  | .B, p => p.inRoom && p.2 == 5
  | .C, p => p.inRoom && p.2 == 7
  | .D, p => p.inRoom && p.2 == 9

def checkOr (c : Bool) (s : String) :=
  if c then c else dbg_trace s; c

def canWalkthrough (br : Burrow) (p q : pos) : Bool :=
  let newPos := p + q
  -- the new position is in the grid
  checkOr (br.grid.contains newPos) s!"The new position {newPos} is not in the grid" &&
  -- the new position does not already contain an amphipod
  checkOr (!br.ap.contains newPos) s!"The new position {newPos} already contains an amphipod" &&
  -- the hallway and room are clear
  ( let hall := (Array.range q.2.natAbs).filterMap fun v =>
        let x : pos := (1, p.2 + q.2.sign * v.cast)
        if br.ap.contains x then some x else none
    checkOr
      hall.isEmpty
      s!"The amphipod cannot walk through the hall: {hall.getD 0 default} is occupied" &&

    let room := (Array.range q.1.natAbs).filterMap fun v =>
      let x : pos := (p.1 + q.1.sign * (1 + v.cast), p.2)
      if br.ap.contains x then some x else none
    checkOr
      room.isEmpty
      s!"The amphipod cannot walk through the room: {room.getD 0 default} is occupied" ) &&
  match br.ap[p]? with
    -- the starting position must contain an amphipod
    | none => checkOr false s!"The starting position {p} must contain an amphipod"
    | some ap =>
      -- amphipods never stop in the hall directly above a room
      checkOr (!newPos.aboveRoom) "Amphipods never stop in the hall directly above a room" &&
      -- amphipods only move from a room that is not their own to the hall or
      -- from the hall to a room that is their own
      ( checkOr
          (p.inRoom && newPos.inHall && !ap.inOwnRoom p)
          "Amphipods only move from a room that is not their own to the hall" ||
        checkOr (p.inHall && newPos.inRoom && ap.inOwnRoom newPos)
          "Amphipods only move from the hall to a room that is their own")

#eval do
  let dat := atest
  let br := inputToBurrow dat
  let br := unsafeMove br (3, 5) (- 2, 0) |>.get!
  IO.println <| canWalkthrough br (2, 7) (- 1, -3)
  IO.println <| canWalkthrough br (3, 3) (- 2, 0)
  drawBurrow br

def move (br : Burrow) (p q : pos) : Option Burrow :=
  let newPos := p + q
  match br.grid.contains newPos, br.ap[p]? with
    | true, some ap => some { br with ap := (br.ap.erase p).insert newPos ap
                                      energy := br.energy + ap.times * dist q }
    | _, _ => none

#eval do
  let dat := atest
  let br := inputToBurrow dat
  let br := move br (2, 3) (-1, 0)
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
