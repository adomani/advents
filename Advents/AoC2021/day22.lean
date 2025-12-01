import Advents.Utils
open Std

namespace Day22

open System in
/-- `input` is the location of the file with the data for the problem. -/
def input : FilePath := ("Advents"/"AoC2021"/"day22" : FilePath).withExtension "input"

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

/-- `test3` is the third test string for the problem. -/
def test3 := "on x=-5..47,y=-31..22,z=-19..33
on x=-44..5,y=-27..21,z=-14..35
on x=-49..-1,y=-11..42,z=-10..38
on x=-20..34,y=-40..6,z=-44..1
off x=26..39,y=40..50,z=-2..11
on x=-41..5,y=-41..6,z=-36..8
off x=-43..-33,y=-45..-28,z=7..25
on x=-33..15,y=-32..19,z=-34..11
off x=35..47,y=-46..-34,z=-11..5
on x=-14..36,y=-6..44,z=-16..29
on x=-57795..-6158,y=29564..72030,z=20435..90618
on x=36731..105352,y=-21140..28532,z=16094..90401
on x=30999..107136,y=-53464..15513,z=8553..71215
on x=13528..83982,y=-99403..-27377,z=-24141..23996
on x=-72682..-12347,y=18159..111354,z=7391..80950
on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
on x=-52752..22273,y=-49450..9096,z=54442..119054
on x=-29982..40483,y=-108474..-28371,z=-24328..38471
on x=-4958..62750,y=40422..118853,z=-7672..65583
on x=55694..108686,y=-43367..46958,z=-26781..48729
on x=-98497..-18186,y=-63569..3412,z=1232..88485
on x=-726..56291,y=-62629..13224,z=18033..85226
on x=-110886..-34664,y=-81338..-8658,z=8914..63723
on x=-55829..24974,y=-16897..54165,z=-121762..-28058
on x=-65152..-11147,y=22489..91432,z=-58782..1780
on x=-120100..-32970,y=-46592..27473,z=-11695..61039
on x=-18631..37533,y=-124565..-50804,z=-35667..28308
on x=-57817..18248,y=49321..117703,z=5745..55881
on x=14781..98692,y=-1341..70827,z=15753..70151
on x=-34419..55919,y=-19626..40991,z=39015..114138
on x=-60785..11593,y=-56135..2999,z=-95368..-26915
on x=-32178..58085,y=17647..101866,z=-91405..-8878
on x=-53655..12091,y=50097..105568,z=-75335..-4862
on x=-111166..-40997,y=-71714..2688,z=5609..50954
on x=-16602..70118,y=-98693..-44401,z=5197..76897
on x=16383..101554,y=4615..83635,z=-44907..18747
off x=-95822..-15171,y=-19987..48940,z=10804..104439
on x=-89813..-14614,y=16069..88491,z=-3297..45228
on x=41075..99376,y=-20427..49978,z=-52012..13762
on x=-21330..50085,y=-17944..62733,z=-112280..-30197
on x=-16478..35915,y=36008..118594,z=-7885..47086
off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
off x=2032..69770,y=-71013..4824,z=7471..94418
on x=43670..120875,y=-42068..12382,z=-24787..38892
off x=37514..111226,y=-45862..25743,z=-16714..54663
off x=25699..97951,y=-30668..59918,z=-15349..69697
off x=-44271..17935,y=-9516..60759,z=49131..112598
on x=-61695..-5813,y=40978..94975,z=8655..80240
off x=-101086..-9439,y=-7088..67543,z=33935..83858
off x=18020..114017,y=-48931..32606,z=21474..89843
off x=-77139..10506,y=-89994..-18797,z=-80..59318
off x=8476..79288,y=-75520..11602,z=-96624..-24783
on x=-47488..-1262,y=24338..100707,z=16292..72967
off x=-84341..13987,y=2429..92914,z=-90671..-1318
off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
off x=-27365..46395,y=31009..98017,z=15428..76570
off x=-70369..-16548,y=22648..78696,z=-1892..86821
on x=-53470..21291,y=-120233..-33476,z=-44150..38147
off x=-93533..-4276,y=-16170..68771,z=-104985..-24507"

/-- `atest3` is the third test string for the problem, split into rows. -/
def atest3 := (test3.splitOn "\n").toArray

/--
Converts the input an array of
* a `Bool`ean that is `true` if the cuboid is `on` and `false` if the cuboid is `off`;
* 3 `pos`, namely 3 pairs of integers, where each pair records the beginning and ending of
  a cuboid, one `pos` per coordinate.
-/
def inputToInequalities (dat : Array String) (small? : Bool) :
    Array (Bool × pos × pos × pos) :=
  dat.foldl (init := ∅) fun h s =>
    let new := match s.getInts with
      | [x1, x2, y1, y2, z1, z2] => ((x1, x2), (y1, y2), (z1, z2))
      | _ => panic "Malformed input!"
    if small? && 50 < new.1.1.natAbs then h else h.push (s.startsWith "on", new)

/-- Tests whether two cuboids overlap. -/
def overlap (v w : pos × pos × pos) : Bool :=
  let ((a1, b1), (a2, b2), (a3, b3)) := v
  let ((c1, d1), (c2, d2), (c3, d3)) := w
  c1 ≤ b1 && a1 ≤ d1 && c2 ≤ b2 && a2 ≤ d2 && c3 ≤ b3 && a3 ≤ d3

/-- Tests whether the first input cuboid contains the second one. -/
def contains (f g : pos × pos × pos) : Bool :=
  let ((f1, f2), (f3, f4), (f5, f6)) := f
  let ((g1, g2), (g3, g4), (g5, g6)) := g
  f1 ≤ g1 && g2 ≤ f2 &&
  f3 ≤ g3 && g4 ≤ f4 &&
  f5 ≤ g5 && g6 ≤ f6

/-- Computes the volume of a cuboid. -/
def volume (f : pos × pos × pos) : Int :=
  let ((f1, f2), (f3, f4), (f5, f6)) := f
  #[f2 - f1 + 1, f4 - f3 + 1, f6 - f5 + 1].prod

/--
Decomposes a `pos`ition, thought of as an interval, with respect to the integer `h`.

It is very important to get the edge cases right, hence the myriads of tests!
-/
def splitFromIncluding (p : pos) (h : Int) : Array pos :=
  let (p1, p2) := p
  if h ≤ p1 then #[p] else
  if p2 < h then #[p] else
  #[(p.1, h - 1), (h, p.2)]

/-- info: #[(0, 1)] -/
#guard_msgs in
#eval
  splitFromIncluding (0, 1) 0

/-- info: #[(0, 0), (1, 2)] -/
#guard_msgs in
#eval
  splitFromIncluding (0, 2) 1

/-- info: #[(0, 0), (1, 1)] -/
#guard_msgs in
#eval
  splitFromIncluding (0, 1) 1

/-- info: #[(0, 1)] -/
#guard_msgs in
#eval
  splitFromIncluding (0, 1) (- 1)

/-- info: #[(9, 9), (10, 11)] -/
#guard_msgs in
#eval
  splitFromIncluding (9, 11) (10)

/-- info: #[(10, 10), (11, 11)] -/
#guard_msgs in
#eval
  splitFromIncluding (10, 11) (10+1)

/--
Partitions the first box into boxes such that at most one overlaps with the second box.

Uses `splitFromIncluding` on each coordinate.
-/
def splitBox (v w : pos × pos × pos) : Array (pos × pos × pos) :=
  let (x, y, z) := v
  let (s, t, u) := w
  let a1 := splitFromIncluding x s.1
  let a1 := a1.flatMap (splitFromIncluding · (s.2 + 1))

  let a2 := splitFromIncluding y t.1
  let a2 := a2.flatMap (splitFromIncluding · (t.2 + 1))

  let a3 := splitFromIncluding z u.1
  let a3 := a3.flatMap (splitFromIncluding · (u.2 + 1))
  Id.run do
  let mut fin := #[]
  for a in a1 do
    for b in a2 do
      for c in a3 do
        fin := fin.push (a, b, c)
  return fin

/-- info: 27 -/
#guard_msgs in
#eval
  let v := ((9, 11), (9, 11), (9, 11))
  let w := ((10, 10), (10, 10), (10, 10))
  splitBox v w |>.size

/-- info: #[((1, 3), (2, 4), 3, 5)] -/
#guard_msgs in
#eval
  let v := ((1, 3), (2, 4), (3, 5))
  let w := ((1, 3), (2, 4), (3, 5))
  splitBox v w

/-- info: #[((1, 2), (2, 4), 3, 5), ((3, 3), (2, 4), 3, 5)] -/
#guard_msgs in
#eval
  let v := ((1, 3), (2, 4), (3, 5))
  let w := ((1, 2), (2, 4), (3, 5))
  splitBox v w

/-- info: #[((1, 2), (2, 4), 3, 5), ((3, 3), (2, 4), 3, 5)] -/
#guard_msgs in
#eval
  let v := ((1, 3), (2, 4), (3, 5))
  let w := ((3, 4), (2, 4), (3, 5))
  splitBox v w

/-- info: #[((1, 2), (2, 4), 3, 5), ((3, 4), (2, 4), 3, 5), ((5, 5), (2, 4), 3, 5)] -/
#guard_msgs in
#eval
  let v := ((1, 5), (2, 4), (3, 5))
  let w := ((3, 4), (2, 4), (3, 5))
  splitBox v w

/-- info:
#[((1, 1), (2, 2), 3, 4),
((1, 1), (2, 2), 5, 5),
((1, 1), (3, 4), 3, 4),
((1, 1), (3, 4), 5, 5),


((2, 3), (2, 2), 3, 4),
((2, 3), (2, 2), 5, 5),
((2, 3), (3, 4), 3, 4),
((2, 3), (3, 4), 5, 5)]
-/
#guard_msgs in
#eval
  let v := ((1, 3), (2, 4), (3, 5))
  let w := ((2, 4), (3, 4), (3, 4))
  splitBox v w

#guard_msgs in
#eval do
  let v := ((1, 3), (2, 4), (3, 5))
  let w := ((2, 4), (3, 4), (3, 4))
  let mut left := splitBox v w
  for a in splitBox v w do
    left := left.erase a
    for b in left do
      if overlap a b then IO.println <| "Error!"

/--
A `mesh` records the state of finding the number of lit leds, as we walk through the cuboids
in reverse order.
* `visited` is the collection of already visited cuboids.
* `totalVolume` is the number of lit leds so far.
-/
structure mesh where
  /-- `visited` is the collection of already visited cuboids. -/
  visited : HashSet (pos × pos × pos)
  /-- `totalVolume` is the number of lit leds so far. -/
  totalVolume : Int
  deriving BEq

/--
Given a `mesh` and the "next" cuboid `h`, update the mesh by
* inserting the new cuboid into the `mesh`,
* figuring out how to decompose the new cuboid into parts to determine the contribution to
  the volume of the lit leds.
-/
def updateMesh (m : mesh) (h : Bool × pos × pos × pos) : mesh := Id.run do
  let (c, box) := h
  let m' := {m with visited := m.visited.insert box}
  -- the new box is behind the previous ones, it is unlit, so we just add it to the `screen`.
  if !c then
    return m'
  let overlappingBoxes := m.visited.filter (overlap box)
  let mut newTot := m.totalVolume
  let mut split := #[box]
  for r in overlappingBoxes do
    let mut newSplit := #[]
    for s in split do
      newSplit := newSplit ++ (splitBox s r |>.filter (! contains r ·))
    split := newSplit
  newTot := newTot + (split.map volume).sum
  return {m' with totalVolume := newTot}

/-- `parts dat short?` takes as input the input of the problem and returns the common
part of the two solutions.  The `Bool`ean `short?` decides whether
* to just use the cuboids contained in `[-50..50] ^ 3` (`short? = true`), or
* to use them all (`short? = true`).
-/
def parts (dat : Array String) (short? : Bool) : Int :=
  let r := inputToInequalities dat short?
  let m : mesh := r.reverse.foldl (init := {visited := {}, totalVolume := 0}) fun m b =>
    updateMesh m b
  m.totalVolume

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Int := parts dat true

#assert part1 atest1 == 39
#assert part1 atest2 == 590784

solve 1 610196  -- takes less than a second

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Int := parts dat false

#assert part2 atest3 == 2758514936282235

solve 2 1282401587270826  -- takes about 3s

end Day22
