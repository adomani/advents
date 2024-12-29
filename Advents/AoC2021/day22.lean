import Advents.Utils
open Lean

namespace Day22

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day22.input"

/-!
#  Question 1
-/

/-- `test1` is the first test string for the problem. -/
def test1 := "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10"

/-- `atest1` is the first test string for the problem, split into rows. -/
def atest1 := (test1.splitOn "\n").toArray

/-- `test2` is the second test string for the problem. -/
def test2 := "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"

/-- `atest2` is the second test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

abbrev vol := Int × Int × Int

instance : ToString vol where toString v := s!"({v.1}, {v.2.1}, {v.2.2})"

structure Reboot where
  grid : vol × vol
  ineqs : Array (Bool × vol × vol)

def inputToReboot (dat : Array String) (small? : Bool := true) : Reboot :=
  let init : vol := (50, 50, 50)
  { grid := (default - init, init)
    ineqs := dat.foldl (init := ∅) fun h s =>
      let new := match s.getInts with
        | [x1, x2, y1, y2, z1, z2] => ((x1, y1, z1), (x2, y2, z2))
        | _ => panic "Malformed input!"
      if small? && 50 < new.1.1.natAbs then h else h.push (s.startsWith "on", new)
  }

#eval do
  let dat := atest1
  let r := inputToReboot dat
  IO.println <| "\n".intercalate <| s!"{r.grid}" :: r.ineqs.foldl (init := []) (· ++ [s!"{·}"])

def filterThrough (r : Reboot) (v : vol) : Bool := Id.run do
  let (x, y, z) := v
  let mut cond := false
  for (on?, (x1, y1, z1), (x2, y2, z2)) in r.ineqs do
    if  x1 ≤ x && x ≤ x2 &&
        y1 ≤ y && y ≤ y2 &&
        z1 ≤ z && z ≤ z2 then cond := on?
  return cond

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := Id.run do
  let r := inputToReboot dat
  let mut count := 0
  for x' in [0:101] do
    let x : Int := x' - 50
    for y' in [0:101] do
      let y : Int := y' - 50
      for z' in [0:101] do
        let z : Int := z' - 50
        if filterThrough r (x, y, z) then count := count + 1
  return count

#assert part1 atest1 == 39  -- takes approx 4s
--set_option trace.profiler true in #assert part1 atest2 == 590784  -- takes approx 20s

--set_option trace.profiler true in solve 1 610196  -- takes approx 21s

/-!
#  Question 2
-/

#eval do
  let dat ← IO.FS.lines input
  let r := inputToReboot dat
  let mut s : Std.HashMap Int Nat := ∅
  for ((_, (v1, v2, v3), (w1, w2, w3)) : Bool × vol × vol) in r.ineqs do
    s := s  |>.alter v1 (some <| ·.getD 0 + 1)
            |>.alter v2 (some <| ·.getD 0 + 1)
            |>.alter v3 (some <| ·.getD 0 + 1)
            |>.alter w1 (some <| ·.getD 0 + 1)
            |>.alter w2 (some <| ·.getD 0 + 1)
            |>.alter w3 (some <| ·.getD 0 + 1)
  for (v, m) in s do
    if m != 1 then IO.println (v, m)

def overlap (v w : vol × vol) : Bool :=
  let ((a1, a2, a3), (b1, b2, b3)) := v
  let ((c1, c2, c3), (d1, d2, d3)) := w
  c1 ≤ b1 && a1 ≤ d1 && c2 ≤ b2 && a2 ≤ d2 && c3 ≤ b3 && a3 ≤ d3

def nonOverlap (v w : vol × vol) : Bool :=
  let ((a1, a2, a3), (b1, b2, b3)) := v
  let ((c1, c2, c3), (d1, d2, d3)) := w
  b1 < c1 || d1 < a1 || b2 < c2 || d2 < a2 || b3 < c3 || d3 < a3

def separateOne (l r l' r' : Int) : Array (Int × Int) :=
  if r' < l then #[(l, r)] else
  if r < l' then #[(l, r)] else
  -- l ≤ r' && l' ≤ r + implicit l ≤ r && l' ≤ r'
  if l < l' && r' < r then -- l ≤ l' ≤ r' ≤ r
    #[(l, l' - 1), (l', r'), (r' + 1, r)] else
  if l < l' && r' == r then -- l ≤ l' ≤ r' ≤ r
    #[(l, l' - 1), (l', r)] else
  if l < l' && r < r' then -- l ≤ l' ≤ r < r'
    #[(l, l' - 1), (l', r)]
  else
  if l == l' && r' == r then -- l ≤ l' ≤ r' ≤ r
    #[(l, r)] else
  if l == l' && r' < r then -- l ≤ l' ≤ r' ≤ r
    #[(l, r'), (r' + 1, r)] else
  if l == l' && r < r' then -- l ≤ l' ≤ r < r'
    #[(l, r)]
  else
  if l' < l && r' < r then -- l' < l ≤ r' ≤ r
    #[(l, r'), (r' + 1, r)] else
  if l' < l && r ≤ r' then -- l' ≤ l ≤ r < r'
    #[(l, r)]
  else
    default

-- `[0..{1..3}]`
/-- info: #[(1, 3)] -/
#guard_msgs in
#eval do
  let (l, r)   := (1, 3)
  let (l', r') := (0, 3)
  IO.println <| separateOne l r l' r'

-- `[{0..1}..4]`
/-- info: #[(0, 1)] -/
#guard_msgs in
#eval do
  let (l, r)   := (0, 1)
  let (l', r') := (0, 4)
  IO.println <| separateOne l r l' r'

-- `[0..{1..3]..4}`
/-- info: #[(1, 3), (4, 4)] -/
#guard_msgs in
#eval do
  let (l, r)   := (1, 4)
  let (l', r') := (0, 3)
  IO.println <| separateOne l r l' r'

-- `[0..{1..3}..4]`
/-- info: #[(1, 3)] -/
#guard_msgs in
#eval do
  let (l, r)   := (1, 3)
  let (l', r') := (0, 4)
  IO.println <| separateOne l r l' r'

-- `[0..1]..{3..4}`
/-- info: #[(3, 4)] -/
#guard_msgs in
#eval do
  let (l, r)   := (3, 4)
  let (l', r') := (0, 1)
  IO.println <| separateOne l r l' r'

-- `{0..1}..[3..4]`
/-- info: #[(0, 1)] -/
#guard_msgs in
#eval do
  let (l, r)   := (0, 1)
  let (l', r') := (3, 4)
  IO.println <| separateOne l r l' r'

def separateX (v w : vol × vol) : Array (vol × vol) :=
  let ((a1, a2, a3), (b1, b2, b3)) := v
  let ((c1, c2, c3), (d1, d2, d3)) := w
  if d1 < a1 then #[v, w] else
  if b1 < c1 then #[v, w] else
  -- a1 ≤ d1 && c1 ≤ b1 + implicit a1 ≤ b1 && c1 ≤ d1
  if a1 ≤ c1 && d1 ≤ b1 then -- a1 ≤ c1 ≤ d1 ≤ b1
    #[((a1, a2, a3), (c1 - 1, b2, b3)), ((c1, a2, a3), (d1 - 1, b2, b3)), ((d1, a2, a3), (b1, b2, b3))]
  else -- a1 ≤ c1 && b1 < d1 then -- a1 ≤ c1 ≤ b1 ≤ d1
    #[((a1, a2, a3), (c1 - 1, b2, b3)), ((c1, a2, a3), (b1, b2, b3))]

#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let r := inputToReboot dat false
  IO.println r.ineqs.size
  --let mut prev : Bool × vol × vol := default
  for i in [0:r.ineqs.size] do
    let (c, curr) := r.ineqs[i]!
    if !c then
      for j in [i + 1:r.ineqs.size] do
        let cj := r.ineqs[j]!
        if cj.1 && overlap curr cj.2 then
          IO.println s!"{(i, j)}:\n{(c, curr)}\n{cj}\n"
--    if overlap prev.2 curr.2 then
--      IO.println s!"\n{prev}\n{curr}"
--    prev := curr

#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let r := inputToReboot dat false
  IO.println r.ineqs.size
  let mut prev : Bool × vol × vol := default
  for curr in r.ineqs do
    if overlap prev.2 curr.2 then
      IO.println s!"\n{prev}\n{curr}"
    prev := curr


#eval do
  let dat ← IO.FS.lines input
  let dat := atest2
  let r := inputToReboot dat
  let mut count := 0
  for x' in [0:10] do
    let x : Int := x' - 50
    for y' in [0:10] do
      let y : Int := y' - 50
      for z' in [0:10] do
        let z : Int := z' - 50
        if filterThrough r (x, y, z) then count := count + 1
  IO.println <| count

/-!
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--set_option trace.profiler true in solve 2

end Day22
