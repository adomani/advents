import Advents.Utils
open Lean

namespace Day19

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day19.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the first test string for the problem. -/
def test := "--- scanner 0 ---
0,2
4,1
3,3

--- scanner 1 ---
-1,-1
-5,0
-2,1"

/-- `atest` is the first test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `test2` is the second test string for the problem. -/
def test2 := "--- scanner 0 ---
-1,-1,1
-2,-2,2
-3,-3,3
-2,-3,1
5,6,-4
8,0,7

--- scanner 0 ---
1,-1,1
2,-2,2
3,-3,3
2,-1,3
-5,4,-6
-8,-7,0

--- scanner 0 ---
-1,-1,-1
-2,-2,-2
-3,-3,-3
-1,-3,-2
4,6,5
-7,0,8

--- scanner 0 ---
1,1,-1
2,2,-2
3,3,-3
1,3,-2
-4,-6,5
7,0,8

--- scanner 0 ---
1,1,1
2,2,2
3,3,3
3,1,2
-6,-4,-5
0,7,-8"

/-- `atest2` is the second test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/-- `test3` is the third test string for the problem. -/
def test3 := "--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14"

/-- `atest3` is the third test for the problem, split into rows. -/
def atest3 := (test3.splitOn "\n").toArray

/-- `vol` is the 3-dimensional space of integer vectors. -/
abbrev vol := Int × Int × Int

/-- A custom printing of vectors: it saves on the number of parentheses. -/
instance : ToString vol where toString := fun (a, b, c) => s!"({a}, {b}, {c})"

/-- The scalar product of two integer vectors. -/
def sc (v w : vol) : Int := v.1 * w.1 + v.2.1 * w.2.1 + v.2.2 * w.2.2

/-- Converts the input data into an array of beacon positions. -/
def inputToData (dat : Array String) : Array (Std.HashSet vol) :=
  let (scanners, final) := dat.foldl (init := (#[], {})) fun (scanners, curr) d =>
    if d.startsWith "--- scanner" then
      if ! curr.isEmpty then
        (scanners.push curr, ∅)
      else
        (scanners, ∅)
    else
      match d.getInts with
        | [a, b, c] => (scanners, curr.insert (a, b, c))
        | l =>
          if !d.isEmpty then
            panic s!"{l} should have had 3 entries!"
          else
            (scanners, curr)
  scanners.push final

/-- A utility function to display configurations of scanners. -/
def showScanners (scs : Array (Std.HashSet vol)) : IO Unit := do
  if scs.isEmpty then IO.println "There are no scanners!"
  let mut con := 0
  let lex : vol → vol → Bool := fun (x1, y1, z1) (x2, y2, z2) =>
        x1 < x2 || (x1 == x2 && y1 < y2) || (x1 == x2 && y1 == y2 && z1 < z2)
  for s in scs do
    IO.println s!"\nScanner {con}"
    for t in s.toArray.qsort lex do
      IO.println t
    con := con + 1

#eval do
  let dat := atest2
  let scs := inputToData dat
  showScanners scs

#eval do
  let dat := atest3
  let scs := inputToData dat
  showScanners scs

/-- Translates the positions of all the scanners in `g` by subtracting the vector `v`. -/
def translateToZero (g : Std.HashSet vol) (v : vol) : Std.HashSet vol :=
  g.fold (·.insert <| · - v) ∅

/--
`workable` is a predicate on pairs of vectors.
It asserts that the
-/
def workable (p q : vol) : Bool :=
  let (p1, p2, p3) := p
  let (q1, q2, q3) := q
  let fst : Std.HashSet Nat := {p1.natAbs, p2.natAbs, p3.natAbs}
  let snd : Std.HashSet Nat := {q1.natAbs, q2.natAbs, q3.natAbs}
  fst != snd || fst.size != 3

def rotateAndSign (p q t : vol) : vol :=
  let (p1, p2, p3) := p
  let (q1, q2, q3) := q
  let (t1, t2, t3) := t
  let fst : Std.HashSet Nat := {p1.natAbs, p2.natAbs, p3.natAbs}
  let snd : Std.HashSet Nat := {q1.natAbs, q2.natAbs, q3.natAbs}
  if fst != snd || fst.size != 3 then
    panic s!"{p} or {q}: should have 3 different absolute values!" else
  let u1 := match p1.natAbs == q1.natAbs, p1.natAbs == q2.natAbs, p1.natAbs == q3.natAbs with
    | true, _, _ => p1.sign * q1.sign * t1
    | _, true, _ => p1.sign * q2.sign * t2
    | _, _, _    => p1.sign * q3.sign * t3
  let u2 := match p2.natAbs == q1.natAbs, p2.natAbs == q2.natAbs, p2.natAbs == q3.natAbs with
    | true, _, _ => p2.sign * q1.sign * t1
    | _, true, _ => p2.sign * q2.sign * t2
    | _, _, _    => p2.sign * q3.sign * t3
  let u3 := match p3.natAbs == q1.natAbs, p3.natAbs == q2.natAbs, p3.natAbs == q3.natAbs with
    | true, _, _ => p3.sign * q1.sign * t1
    | _, true, _ => p3.sign * q2.sign * t2
    | _, _, _    => p3.sign * q3.sign * t3
  (u1, u2, u3)

#assert
  let p := (1, 2, 3)
  let q := (-2, 1, -3)
  let t := (4, 5, 6)
  rotateAndSign p q t == (5, -4, -6)

#assert
  let p := (1, 2, 3)
  let q := (-2, 3, 1)
  let t := (4, 5, 6)
  rotateAndSign p q t == (6, -4, 5)

def completeAlignment (g : Std.HashSet vol) (ref actual : vol) : Std.HashSet vol :=
  g.fold (·.insert <| rotateAndSign ref actual ·) ∅

def quickCheck (g h : Std.HashSet vol) : Bool := Id.run do
  let mut allDists := #[]
  for s in [g, h] do
    let mut dists : Std.HashSet Int := ∅
    let mut left := s
    for a in s do
      left := left.erase a
      for b in left do
        dists := dists.insert (sc (a - b) (a - b))
    allDists := allDists.push dists
  return (12 * 11) / 2 ≤ (allDists[0]!.filter allDists[1]!.contains).size

def sync (g h : Std.HashSet vol) : Option (Std.HashSet vol × vol) := Id.run do
  if ! quickCheck g h then return none else
  let mut pair := #[]
  let mut translation := default
  let mut con := 0
  for a0 in g do
    if 2 ≤ pair.size then continue
    let diffs : Std.HashSet Int := g.fold (init := ∅) fun h a => h.insert <| sc (a - a0) (a - a0)
    for v in h do
      let tr : Std.HashSet Int := h.fold (init := ∅) fun h a => h.insert <| sc (a - v) (a - v)
      let overlap := (diffs.filter (tr.contains)).size
      if 12 ≤ overlap then
        con := con + 1
        --IO.println s!"{con} match: {a0} and {v}"
        --dbg_trace pair.size ≤ 2 && workable translation a0
        if pair.size ≤ 2 && workable translation a0 then
          pair := pair.push (a0, v)
        if pair.isEmpty then
          translation := a0
          pair := pair.push (a0, v)
  if pair.size ≤ 1 then return none
  let (a0, v) := pair[0]!
  let (a1, v1) := pair[1]!
  --IO.println s!"{a1 - a0} and {v1 - v}"
  --dbg_trace "{a0}, {a1}, {a1 - a0}, {v - v1 + a1 - a0}"
  --let h0 := translateToZero h (a1 - a0)
  let rearrangeUniv (old : Std.HashSet vol) : Std.HashSet vol :=
    translateToZero (completeAlignment (translateToZero old v) (a1 - a0) (v1 - v)) ((0, 0, 0) - a0)
  return some <| (rearrangeUniv h, (rearrangeUniv {(0, 0, 0)}).toArray[0]!)
  --drawScanners #[translateToZero sc0 a0, completeAlignment (translateToZero sc1 v) (a1 - a0) (v1 - v)]

def beaconsAndScanners (dat : Array String) : Std.HashSet vol × Std.HashSet vol := Id.run do
  let scs := inputToData dat
  let mut beacs := scs[0]!
  let mut scanners : Std.HashSet vol := {(0, 0, 0)}
  let mut aligned := #[]
  let mut left := #[]
  for n in scs.erase beacs do
    match sync beacs n with
      | none => left := left.push n
      | some (n', scanner) =>
        aligned := aligned.push n'
        scanners := scanners.insert scanner
  let mut con := 0
  while !left.isEmpty do
    con := con + 1
    dbg_trace "\n{con} Before: beacs: {beacs.size} aligned: {aligned.size} left: {left.size}"
    let mut newAligned := #[]
    let mut newLeft := #[]
    beacs := aligned.foldl (init := beacs) (·.union ·)
    for al in aligned do
      for n in left do
        match sync al n with
          | none => if !newLeft.contains n then newLeft := newLeft.push n
          | some (n', scanner) =>
            if !newAligned.contains n' then
              newAligned := newAligned.push n'
              newLeft := newLeft.erase n
              scanners := scanners.insert scanner
      aligned := newAligned
      left := newLeft
      beacs := aligned.foldl (init := beacs) (·.union ·)
      dbg_trace "  After: beacs: {beacs.size} aligned: {aligned.size} left: {left.size}"
  return (beacs, scanners)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := (beaconsAndScanners dat).1.size

#assert part1 atest3 == 79

set_option trace.profiler true in solve 1 405  -- takes approximately 5s

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let (_, scanners) := beaconsAndScanners dat
  let mut Mdist := 0
  let mut leftF := scanners
  for a in scanners do
    leftF := leftF.erase a
    for b in leftF do
      let (d1, d2, d3) := a - b
      let newM := max Mdist (d1.natAbs + d2.natAbs + d3.natAbs)
      if newM != Mdist then
        Mdist := newM
  return Mdist

#assert part2 atest3 == 3621

set_option trace.profiler true in solve 2 12306  -- takes approximately 5s

end Day19
