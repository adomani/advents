import Advents.Utils
open Lean

namespace Day19

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day19.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "--- scanner 0 ---
0,2
4,1
3,3

--- scanner 1 ---
-1,-1
-5,0
-2,1"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `test2` is the test2 string for the problem. -/
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

/-- `atest2` is the test2 string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/-- `test3` is the test3 string for the problem. -/
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

/-- `atest3` is the test3 string for the problem, split into rows. -/
def atest3 := (test3.splitOn "\n").toArray
#eval IO.println atest3
abbrev vol := Int × Int × Int

instance : ToString vol where toString := fun (a, b, c) => s!"({a}, {b}, {c})"

/-- The scalar product of two integer vectors. -/
def sc (v w : vol) : Int := v.1 * w.1 + v.2.1 * w.2.1 + v.2.2 * w.2.2

def inputToData (dat : Array String) : Array (Std.HashSet vol) :=
  let (scanners, _current) := dat.foldl (init := (#[], {})) fun (scanners, curr) d =>
    if d.startsWith "--- scanner" then
      --dbg_trace d
      if ! curr.isEmpty then
        (scanners.push curr, ∅)
      else
        --dbg_trace "{d} again"
        (scanners, ∅)
    else
      match d.getInts with
        | [a, b, c] => (scanners, curr.insert (a, b, c))
        | l => if !d.isEmpty then panic s!"{l} should have had 3 entries!" else (scanners, curr)
  scanners.push _current

def lex : vol → vol → Bool := fun (x1, y1, z1) (x2, y2, z2) =>
      x1 < x2 || (x1 == x2 && y1 < y2) || (x1 == x2 && y1 == y2 && z1 < z2)

/-- A utility function to draw configurations of scanners. -/
def drawScanners (scs : Array (Std.HashSet vol)) : IO Unit := do
  if scs.isEmpty then IO.println "There are no scanners!"
  let mut con := 0
  for s in scs do
    IO.println s!"\nScanner {con}"
    for t in s.toArray.qsort lex do
      IO.println t
    con := con + 1

#eval do
  let dat ← IO.FS.lines input
  let dat := atest2
  let scs := inputToData dat
  drawScanners scs

#eval do
  let dat ← IO.FS.lines input
  let dat := atest3
  let scs := inputToData dat
  drawScanners scs

/-- Tallies the distances between any two beacons. -/
def distances (h : Std.HashSet vol) : Std.HashMap Int Nat := Id.run do
  let mut left := h
  let mut lths := ∅
  for v in h do
    left := left.erase v
    for w in left do
      let diff := v - w
      lths := lths.alter (sc diff diff) (some <| ·.getD 0 + 1)
  return lths

/-- Similar to `distances`, except that it tallies the the symmetric functions of the scalar
products of the relative positions between any two beacons. -/
def threes (h : Std.HashSet vol) : Std.HashMap vol Nat := Id.run do
  let mut left := h
  let mut right := h
  let mut lths := ∅
  for v in h do
    left := left.erase v
    right := right.erase v
    for w in left do
      right := right.erase w
      for u in right do
        let vw := v - w
        let vu := v - u
        let a := sc vw vw
        let b := sc vu vu
        lths := lths.alter (a + b, a * b, (sc vw vu) ^ 2) (some <| ·.getD 0 + 1)
  return lths

--def findTriangle (n : Nat) : Nat :=

/--
Finds the pairs `(v, w)` among the elements of `g` whose difference has the same length as `v0`
-/
def align (g : Std.HashSet vol) (v0 : vol) : Std.HashMap (vol × vol) vol := Id.run do
  let vlth := sc v0 v0
  let mut left := g
  let mut lths := ∅
  for v in g do
    left := left.erase v
    for w in left do
      let diff := v - w
      let lth := sc diff diff
      if lth == vlth then
        lths := lths.insert (v, w) v0
      --lths := lths.alter (sc diff diff) (some <| ·.getD 0 + 1)
  return lths
  --dbg_trace "d1: {d1.size}\nd2: {d2.size}\n12: {(d1.union d2).size}"
  --return default

def translate (ref g : Std.HashSet vol) (v1 v : vol) : Std.HashSet vol :=
  let v' := v - v1
  let g' := g.fold (·.insert <| · + v') {}
  if 12 ≤ (g'.filter ref.contains).size then
    dbg_trace "overlap\n{(g'.filter ref.contains).toArray.qsort (·.1 < ·.1)}\n"
    g'
  else
    dbg_trace "Common: {(ref.filter g'.contains).size} {(g'.filter ref.contains).size}"
    {}

def translateToZero (g : Std.HashSet vol) (v : vol) : Std.HashSet vol :=
  g.fold (·.insert <| · - v) ∅

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
  if fst != snd || fst.size != 3 then panic s!"{p} or {q}: should have 3 different absolute values!" else
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

def sync (g h : Std.HashSet vol) : Option (Std.HashSet vol) := Id.run do
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
  return some <| translateToZero (completeAlignment (translateToZero h v) (a1 - a0) (v1 - v)) ((0, 0, 0) - a0)
  --drawScanners #[translateToZero sc0 a0, completeAlignment (translateToZero sc1 v) (a1 - a0) (v1 - v)]

#eval do
  let dat := atest3
  let dat ← IO.FS.lines input
  let scs := inputToData dat
  --let mut msg := s!"{scs.size} scanners"
  --for i in scs do
  --  msg := msg.push '\n'
  --  for j in scs do
  --    msg := msg ++ s!"{if (sync i j).isSome then 1 else 0} "
  --IO.println msg
  let first := scs[0]!
  let mut fixed := #[first]
  let mut aligned := #[]
  let mut left := #[]
  --let mut reallyLeft := #[]
  for n in scs.erase first do
    match sync first n with
      | none => left := left.push n
      | some n' => aligned := aligned.push n'
  --let news : Array _ := (scs.erase first).foldl (fun h n => match sync first n with | none => h.push n | some n' => h.push n') #[]
  let mut con := 0
  while !left.isEmpty && con < 1 do
    con := con + 1
    dbg_trace "\n{con} Before: fixed: {fixed.size} aligned: {aligned.size} left: {left.size}"
    let mut newAligned := #[]
    let mut newLeft := #[]
    fixed := fixed ++ aligned
    for al in aligned do
      --fixed := fixed.push al
      for n in left do
        match sync al n with
          | none => if !newLeft.contains n then newLeft := newLeft.push n
          | some n' => if !newAligned.contains n' then newAligned := newAligned.push n'
      aligned := newAligned
      --if aligned.isEmpty then reallyLeft := reallyLeft ++ left
      left := newLeft
      fixed := fixed ++ aligned
      dbg_trace "  After: fixed: {fixed.size} aligned: {aligned.size} left: {left.size}"

  --drawScanners (fixed ++ aligned ++ left)
  --for f in fixed do
  --  IO.println (sync f reallyLeft[0]!).isSome
  --drawScanners reallyLeft
  let beacs : Std.HashSet vol := fixed.foldl (init := ∅) (·.union ·)
  IO.println <| beacs.size
  --drawScanners fixed
  --IO.println s!"03: {(sync scs[0]! scs[3]!).isSome}"
  --IO.println s!"13: {(sync scs[1]! scs[3]!).isSome}"
  --IO.println s!"23: {(sync scs[2]! scs[3]!).isSome}"
--
  --IO.println s!"02: {(sync scs[0]! scs[2]!).isSome}"
  --IO.println s!"12: {(sync scs[1]! scs[2]!).isSome}"
--
  --IO.println s!"01: {(sync scs[0]! scs[1]!).isSome}"
  --drawScanners #[sc0, (sync sc0 sc1).get!]

/-!-/

#exit

#eval do
  let dat := atest3
  let dat ← IO.FS.lines input
  let scs := inputToData dat
  let sc0 := scs[0]!
  let sc1 := scs[2]!
  drawScanners #[sc0, (sync sc0 sc1).get!]
#exit
  --let a0 := sc0.toArray[0]!
  let mut pair := #[]
  let mut con := 0
  for a0 in sc0 do
    let diffs := sc0.fold (init := ∅) fun (h : Std.HashSet Int) a => h.insert <| sc (a - a0) (a - a0)
    for v in sc1 do
      let tr := sc1.fold (init := ∅) fun (h : Std.HashSet Int) a => h.insert <| sc (a - v) (a - v)
      let overlap := (diffs.filter (tr.contains)).size
      if 12 ≤ overlap then
        con := con + 1
        IO.println s!"{con} match: {a0} and {v}"
        if pair.size ≤ 2 then
          pair := pair.push (a0, v)
  let (a0, v) := pair[0]!
  let (a1, v1) := pair[1]!
  IO.println s!"{a1 - a0} and {v1 - v}"
  drawScanners #[translateToZero sc0 a0, completeAlignment (translateToZero sc1 v) (a1 - a0) (v1 - v)]
  --drawScanners #[translateToZero sc0 a0, translateToZero sc1 v]
      --IO.println s!"· - {v}: overlap = {(diffs.filter (tr.contains)).size}"

#exit
  for sc1 in scs do
    let mut tal : Std.HashMap Int Nat := ∅
    let mut left := sc1
    for a in sc1 do
      left := left.erase a
      for b in left do
        let diff := a - b
        tal := tal.alter (sc diff diff) (some <| ·.getD 0 + 1)
    IO.println <| (tal.filter fun _ m => m == 1).toArray.qsort (·.1 < ·.1)
#exit
  let v0 := scs[0]!.getD (-618, -824, -621) default
  let v1 := scs[0]!.getD (-537, -823, -458) default
  let v2 := scs[0]!.getD (-447, -329, 318) default
  IO.println s!"v0 and v1: {v0} and {v1}"
  let als := align scs[1]! (v1 - v0)
  IO.println als.toArray
  IO.println ""
  IO.println s!"v0 and v2: {v0} and {v2}"
  let als := align scs[1]! (v2 - v0)
  IO.println als.toArray
  IO.println ""
  IO.println s!"v1 and v2: {v1} and {v2}"
  let als := align scs[1]! (v2 - v1)
  IO.println als.toArray

#eval do
  let dat := atest2
  let dat ← IO.FS.lines input
  let dat := atest3
  let h := inputToData dat
  IO.println s!"{h.map (Std.HashSet.size)}"
  let g1 := h[0]!
  --let g2 := h[3]!
  let v0 := g1.toArray[0]!
  let v1 := g1.toArray[1]!
  --let v2 := g1.toArray[2]!
  --let v3 := g1.toArray[3]!
  let v01 := v0 - v1
  let mut tot := g1
  for g2 in h do
    --IO.println s!"{v0 - v1}\n{v0 - v2}\n{v0 - v3}"
    --for gi in [0:h.size] do
    --  let g := h[gi]!
    --  let gs := g.size
    --  if gs != (g.union g1).size then IO.println s!"{gi}"
    let new := align g2 v01
    dbg_trace "new.size: {new.size}"
    if ! new.isEmpty then
      let ((f1, f2), f3) := new.toArray[0]!
      dbg_trace "translating by {f1 + v01}, {f2 + v01}, {f3}, {f3 + v01}, {new.size}"
      tot := tot.union (translate g1 g2 f1 v0)
      IO.println tot.size
    --let ((f1, f2), _) := (align g2 v01).toArray[0]!
    --IO.println <| (f1, f2)
    --IO.println <| (translate g1 g2 f1 v0).toArray[0]!
    --IO.println <| (align g2 (v0 - v2)).size --toArray
    --IO.println <| (align g2 (v0 - v3)).size --toArray
  IO.println <| String.intercalate "\n" ((tot.toArray.qsort (·.1 < ·.1)).toList.map (s!"{·}"))

#eval Int.factors (8 * 1078 + 1)
#eval (Nat.sqrt (8 * 1078 + 1) + 1) / 2
#eval (46 * 47) / 2

#eval do
  let src : Nat := 9498
  let src : Nat := 1128
  let src : Nat := 325
  let rt := (Nat.sqrt (8 * src + 1) + 1) / 2
  let repr := (rt * (rt - 1)) / 2
  IO.println <| Int.factors (8 * src + 1)
  IO.println <| rt
  IO.println <| (repr, repr == src)

#exit
#eval do
  let dat := atest2
  let dat := atest3
  let dat ← IO.FS.lines input

  let mut scanners := #[]
  let mut dists := #[]
  let mut thrs := #[]
  let mut curr : Std.HashSet vol := {(0, 0, 0)}
  for d in dat do
    if d.startsWith "--- scanner" && ! curr.isEmpty then
      scanners := scanners.push curr
      --IO.println <| (distances curr).toArray
      dists := dists.push (distances curr)
      thrs := thrs.push (threes curr)
      curr := ∅
    else
      match d.getInts with
        | [a, b, c] => curr := curr.insert (a, b, c)
        | l => if !d.isEmpty then panic s!"{l} should have had 3 entries!"
  --let tot : Std.HashMap Int Nat := dists.foldl .union ∅
  --IO.println tot.size
  --IO.println tot.toArray
  let tot : Std.HashMap vol Nat := thrs.foldl .union ∅
  IO.println tot.size
  IO.println tot.toArray

  --IO.println <| scanners.map (Std.HashSet.toArray)
--  let scanners := dat.splitOn "scanner" |>.map fun s : String =>
--    let vals := s.getInts
--    vals.erase vals[0]!


/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat := sorry
--def part1 (dat : String) : Nat := sorry

--#assert part1 atest == ???

--solve 1

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day19
