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
  grid : pos × pos × pos
  ineqs : Array (Bool × pos × pos × pos)

def inputToReboot (dat : Array String) (small? : Bool := true) : Reboot :=
  let init : pos := (- 50, 50)
  { grid := (init, init, init)
    ineqs := dat.foldl (init := ∅) fun h s =>
      let new := match s.getInts with
        | [x1, x2, y1, y2, z1, z2] => ((x1, x2), (y1, y2), (z1, z2))
        | _ => panic "Malformed input!"
      if small? && 50 < new.1.1.natAbs then h else h.push (s.startsWith "on", new)
  }

#eval do
  let dat := atest1
  let r := inputToReboot dat
  IO.println <| "\n".intercalate <| s!"{r.grid}" :: r.ineqs.foldl (init := []) (· ++ [s!"{·}"])

/-- Returns whether or not `v` satisfies the inequalities encoded in `cs`. -/
def insideBox (cs : pos × pos × pos) (v : vol) : Bool :=
  let (x, y, z) := v
  let ((x1, x2), (y1, y2), (z1, z2)) := cs
  x1 ≤ x && x ≤ x2 &&
  y1 ≤ y && y ≤ y2 &&
  z1 ≤ z && z ≤ z2

/--
Assumes that the `ineqs` field of `Reboot` is in *reverse order*!
Returns the first `Bool`ean value that it finds in the record that contains `v`.
-/
def filterThrough (r : Reboot) (v : vol) : Bool := Id.run do
  for (on?, cs) in r.ineqs do
    if insideBox cs v then return on?
  return false

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
        if filterThrough {r with ineqs := r.ineqs.reverse} (x, y, z)
        then
          count := count + 1
  return count

--set_option trace.profiler true in #assert part1 atest1 == 39  -- takes approx 5s
--set_option trace.profiler true in #assert part1 atest2 == 590784  -- takes approx 12s

--set_option trace.profiler true in solve 1 610196  -- takes approx 12s

/-!
#  Question 2
-/

#eval do
  let dat ← IO.FS.lines input
  let r := inputToReboot dat
  let mut s : Std.HashMap Int Nat := ∅
  for ((_, ((v1, w1), (v2, w2), (v3, w3))) : Bool × pos × pos × pos) in r.ineqs do
    s := s  |>.alter v1 (some <| ·.getD 0 + 1)
            |>.alter v2 (some <| ·.getD 0 + 1)
            |>.alter v3 (some <| ·.getD 0 + 1)
            |>.alter w1 (some <| ·.getD 0 + 1)
            |>.alter w2 (some <| ·.getD 0 + 1)
            |>.alter w3 (some <| ·.getD 0 + 1)
  for (v, m) in s do
    if m != 1 then IO.println (v, m)

def overlap (v w : pos × pos × pos) : Bool :=
  let ((a1, b1), (a2, b2), (a3, b3)) := v
  let ((c1, d1), (c2, d2), (c3, d3)) := w
  c1 ≤ b1 && a1 ≤ d1 && c2 ≤ b2 && a2 ≤ d2 && c3 ≤ b3 && a3 ≤ d3

def nonOverlap (v w : pos × pos × pos) : Bool :=
  let ((a1, b1), (a2, b2), (a3, b3)) := v
  let ((c1, d1), (c2, d2), (c3, d3)) := w
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

def separateVolX (v w : pos × pos × pos) : Array (pos × pos × pos) := Id.run do
  let ((a1, a2), b, c) := v
  let ((d1, d2), _e, _f) := w
  let arr := separateOne a1 a2 d1 d2
  let mut fin := #[]
  for ix in arr do
    fin := fin.push (ix, b, c)
  return fin

def swap12 (v : pos × pos × pos) : pos × pos × pos := (v.2.1, v.1, v.2.2)
def swap13 (v : pos × pos × pos) : pos × pos × pos := (v.2.2, v.2.1, v.1)

def separateVolY (v w : pos × pos × pos) : Array (pos × pos × pos) :=
  let sw := separateVolX (swap12 v) (swap12 w)
  sw.map swap12

def separateVolZ (v w : pos × pos × pos) : Array (pos × pos × pos) :=
  let sw := separateVolX (swap13 v) (swap13 w)
  sw.map swap13

def separateAll (v w : pos × pos × pos) : Array (pos × pos × pos) :=
  separateVolX v w  |>.foldl (init := #[]) (· ++ separateVolY · w)
                    |>.foldl (init := #[]) (· ++ separateVolZ · w)

#eval do
  IO.println <| separateAll ((1, 2), (3, 4), (5, 6)) ((1, 2), (3, 4), (5, 6))
  IO.println <| separateAll ((1, 2), (2, 5), (5, 6)) ((1, 2), (3, 4), (5, 6))
  IO.println s!"{separateAll ((1, 4), (1, 4), (1, 4)) ((2, 3), (2, 3), (2, 3)) |>.size}"
  IO.println <| separateAll ((1, 4), (1, 4), (1, 4)) ((2, 3), (2, 3), (2, 3))

def contains (f g : pos × pos × pos) : Bool :=
  let ((f1, f2), (f3, f4), (f5, f6)) := f
  let ((g1, g2), (g3, g4), (g5, g6)) := g
  f1 ≤ g1 && g2 ≤ f2 &&
  f3 ≤ g3 && g4 ≤ f4 &&
  f5 ≤ g5 && g6 ≤ f6

-- cuboids that overlap with none of their followers or are both "on" or both "off"
#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let r := inputToReboot dat false
  for i in [0:r.ineqs.size] do
    let (_ci, ri) := r.ineqs[i]!
    let mut nop := 0
    for j in [i + 1:r.ineqs.size] do
      let (_cj, rj) := r.ineqs[j]!
      nop := nop + if (overlap ri rj && _ci != _cj) then 1 else 0
    if nop != 0 then IO.println s!"Cuboid {if i < 10 then "  " else if i < 100 then " " else ""}{i},  {if _ci then "on, " else "off,"}  {nop} overlap{if nop == 1 then "" else "s"}"
      --if contains rj ri then
      --  IO.println s!"← {(j, i)} {(_cj, _ci)} {separateAll rj ri |>.size}"

def v (f : pos × pos × pos) : Int :=
  let ((f1, f2), (f3, f4), (f5, f6)) := f
  #[f2 - f1, f4 - f3, f6 - f5].prod

/-!-/
instance [Ord α] [Ord β] : Ord (α × β) where
  compare f g := match compare f.1 g.1 with
    | .eq => compare f.2 g.2
    | x => x

def prepend (a : α) (as : Array β) : Array (α × β) :=
  as.foldl (init := #[]) (·.push <| Prod.mk a ·)

def disengage (f g : Bool × pos × pos × pos) :
    Array (Bool × pos × pos × pos) × Array (Bool × pos × pos × pos) :=
  --let (onf, (f1, f2), (f3, f4), (f5, f6)) := f
  --let (ong, (g1, g2), (g3, g4), (g5, g6)) := g
  let (onf, f0) := f
  let (ong, g0) := g
  let sf := separateAll f0 g0
  let sg := separateAll g0 f0
  if onf == ong then
    let sfg := sf ++ sg
    (prepend onf sfg.sortDedup, ∅)
  else
    let sfOnly := prepend onf <| sf.filter (!sg.contains ·)
    let sgOnly := prepend ong <| sg.filter (!sf.contains ·)
  (sfOnly, sgOnly)

def updateCount [BEq α] [Hashable α] [BEq R] [Hashable R] [Add R] [Zero R] [OfNat R 1]
    (h : Std.HashMap α R) (a : α) (f : α → R := fun _ => 1) :
    Std.HashMap α R :=
  h.alter a (some <| ·.getD 0 + f a)
#eval 797 * 795 --* 796
-- cuboids that overlap with none of their followers or are both "on" or both "off"
#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let r := inputToReboot dat false
  --let mut left := #[]
  let mut ons : Int := 0
  let mut ends : Std.HashMap Int Nat := ∅
  let mut xs : Std.HashMap Int Nat := ∅
  let mut ys : Std.HashMap Int Nat := ∅
  let mut zs : Std.HashMap Int Nat := ∅
  for i in [0:r.ineqs.size] do --[20:22] do
    let (ci, ri) := r.ineqs[i]!
    if ri.1.1.natAbs ≤ 50 then continue
    --IO.println s!"{i}, {v ri}"
    for r in [ri.1.1, ri.1.2] do
      xs := updateCount xs r
    for r in [ri.2.1.1, ri.2.1.2] do
      ys := updateCount ys r
    for r in [ri.2.2.1, ri.2.2.2] do
      zs := updateCount zs r
    for r in [ri.1.1, ri.1.2, ri.2.1.1, ri.2.1.2, ri.2.2.1, ri.2.2.2] do
      ends := updateCount ends r
  let mut con := 0
  for (x, _) in xs do
    for (y, _) in ys do
      --for (z, _) in zs do
        con := con + x + y
  IO.println con
  IO.println <| (xs.size, ys.size, zs.size) --xs.toArray.qsort (·.2 < ·.2)
  --IO.println <| ends.toArray.qsort (·.2 < ·.2)
#exit
    let mut totalOverlap := 0
    let mut diseng := #[(ci, ri)]
    --let mut doesNotOverlap := true
    let mut isContained := false
    for j in [i + 1:r.ineqs.size] do
      let (_cj, rj) := r.ineqs[j]!
      if 50 < ri.1.1.natAbs then
        let (l, r) := disengage (ci, ri) (_cj, rj)
        IO.println s!"{i}-{j}: {ci}-{_cj} {(l.size, r.size)}\n{l}\n{r}"
      --diseng := diseng.foldl (init := ∅) fun h n => h ++ (disengage n (_cj, rj)).1
      if overlap ri rj then
        --doesNotOverlap := false
        totalOverlap := totalOverlap + 1
      isContained := isContained || contains rj ri
    if isContained then continue
    if totalOverlap == 0 then
      ons := ons + if ci then v ri else 0
    else
      left := left.push (ci, ri)
    IO.println left.size --ons

      --if contains rj ri then
      --  IO.println s!"← {(j, i)} {(_cj, _ci)} {separateAll rj ri |>.size}"

#exit
-- cuboids that overlap with none of their followers or are both "on" or both "off"
#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let r := inputToReboot dat false
  let mut left := #[]
  let mut ons : Int := 0
  for i in [0:r.ineqs.size] do
    let (ci, ri) := r.ineqs[i]!
    let mut totalOverlap := 0
    let mut diseng := #[(ci, ri)]
    --let mut doesNotOverlap := true
    let mut isContained := false
    for j in [i + 1:r.ineqs.size] do
      let (_cj, rj) := r.ineqs[j]!
      if 50 < ri.1.1.natAbs then
        let (l, r) := disengage (ci, ri) (_cj, rj)
        IO.println s!"{i}-{j}: {(l.size, r.size)}"
      --diseng := diseng.foldl (init := ∅) fun h n => h ++ (disengage n (_cj, rj)).1
      if overlap ri rj then
        --doesNotOverlap := false
        totalOverlap := totalOverlap + 1
      isContained := isContained || contains rj ri
    if isContained then continue
    if totalOverlap == 0 then
      ons := ons + if ci then v ri else 0
    else
      left := left.push (ci, ri)
    IO.println left.size --ons

-- cuboids entirely contained in another cuboid
#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let r := inputToReboot dat false
  let mut tots := (0, 1)
  for i in [0:r.ineqs.size] do
    let (_ci, ri) := r.ineqs[i]!
    for j in [i + 1:r.ineqs.size] do
      let (_cj, rj) := r.ineqs[j]!
      if contains ri rj then
        IO.println s!"→ {(i, j)} {(_ci, _cj)} {separateAll ri rj |>.size}"
      if contains rj ri then
        IO.println s!"← {(j, i)} {(_cj, _ci)} {separateAll rj ri |>.size}"

#exit
#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let r := inputToReboot dat false
  let mut tots := (0, 1)
  for i in [0:r.ineqs.size] do
    let (_ci, ri) := r.ineqs[i]!
    for j in [i + 1:r.ineqs.size] do
      let (_cj, rj) := r.ineqs[j]!
      let seps := separateAll ri rj |>.size
      if seps != 1 then IO.println s!"{(i, j)} {(_ci, _cj)} {seps} "
      tots := (tots.1 + seps, tots.2 * seps)

  IO.println (tots)


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
