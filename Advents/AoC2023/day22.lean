import Advents.Utils
open Std

namespace Day22

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2023/day22.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `vol` is the type of triples of integers: a `vol`ume. -/
abbrev vol := Int × Int × Int

/-- Component-wise scalar multiplication of a `vol`ume by a natural number. -/
instance : HMul Nat vol vol where
  hMul a x := (a * x.1, a * x.2.1, a * x.2.2)

instance : HMul Nat pos pos where
  hMul a x := (a * x.1, a * x.2)

/-- A `brick` is a "linear" string of `vol`umes.  It is encoded by
* `src`, its beginning position -- chosen so that the brick is in the
  positive orthant starting from `src`;
* `dir`, its direction -- one of the standard unit vectors;
* `lth`, its length -- a natural number that represents one less than the number of blocks
  of the brick.
-/
structure brick where
  src : vol
  dir : vol
  lth : Nat
  deriving Inhabited, BEq, Hashable

structure state where
  bricks : HashMap pos pos
  fallen : HashMap pos pos
  profile : HashMap pos pos

def inputToState (dat : Array String) : state :=
  let bricks := dat.foldl (init := ∅) fun h b =>
    match b.getNats with
      | [a, b, c, d, e, f] =>
        -- first, we reorganize `(a, b, c), (d, e, f)` so that `(a, b, c) < (d, e, f)`
        let (x, y) := ((a, b, c), (d, e, f))
        let ((a, b, c), (d, e, f)) := if y < x then (y, x) else (x, y)
        -- second, we extract the direction `v` and length `l` of the brick
        let (dir, lth) : vol × Nat := if a < d then ((1, 0, 0), d - a) else
                                      if b < e then ((0, 1, 0), e - b) else
                                      ((0, 0, 1), f - c)
        --h.insert (a, b) <|
        --  if a < d then (d - a) else
        --  if b < e then (e - b) else
        --  (f - c)
        if dir != (0, 0, 1) then
          h.insertMany <| (Array.range (lth + 1)).map fun v =>
            (((a, b) : pos) + v * (dir.1, dir.2.1), (c.cast, c.cast))
        else
          h.insert ((a, b) : pos) (c, f)
      | _ => panic "Malformed input!"
  { bricks := bricks, fallen := ∅, profile := ∅}

nonrec
def min1 : Option Int → Option Int → Option Int
  | none, some a => some a
  | some a, none => some a
  | some a, some b => some (min a b)
  | none, none => none

def fallsBy (s : state) (b : Array vol) : Int :=
  let minDiff : Int := b.foldl (init := (b.getD 0 (0, 0, 0)).2.2) fun h (a, b, c) =>
    let ht := c - (s.profile[(a, b)]?.getD 1)
    min ht h
  minDiff

#eval do
  let dat := atest
  let s := inputToState dat
  let s := {inputToState dat with profile := {((1, 1), 1), ((1, 2), 2)}}
  IO.println <| fallsBy s #[(1, 2, 5)]
  IO.println <| fallsBy s #[(1, 1, 5), (1, 2, 5)]

def mkFall (s : state) (b : Array vol) (h : Int) : state :=
  { bricks := s.bricks.erase b
    fallen := s.fallen.insert b
    profile :=
      let brickProfile : HashMap pos Int := b.foldl (init := ∅) fun b (x, y, z) =>
        b.alter (x, y) (some <| z - ·.getD 0)
      b.foldl (init := s.profile) fun hp (x, y, z) =>
        hp.alter (x, y) (some <| ·.getD (0) + z - h)
  }




#eval do
  let dat := atest
  let s := inputToState dat
  let bk := #[(1, 1, 5), (1, 2, 5)]
  let ht := fallsBy s bk
  IO.println <| ht
  let s := mkFall s bk ht
  IO.println s.profile.toArray
  let bk := #[(1, 1, 5), (1, 2, 5)]
  let ht := fallsBy s bk
  let s := mkFall s bk ht
  IO.println s.profile.toArray
  let bk := #[(1, 1, 5), (1, 2, 5)]
  let ht := fallsBy s bk
  let s := mkFall s bk ht
  IO.println s.profile.toArray

/-- A pretty-printer for `brick`s. -/
instance : ToString brick where
  toString b :=
    let (x, y, z) := b.src
    let (p, q, r) := b.dir
    s!"({x}, {y}, {z}) → {b.lth} * ({p}, {q}, {r})"

/-- `String.toBrick s` converts a string `s` to a brick, by parsing `s` according to the
rules of the day. -/
def String.toBrick (s : String) : brick :=
  match s.getNats with
    | [a, b, c, d, e, f] =>
      -- first, we reorganize `(a, b, c), (d, e, f)` so that `(a, b, c) < (d, e, f)`
      let (x, y) := ((a, b, c), (d, e, f))
      let ((a, b, c), (d, e, f)) := if y < x then (y, x) else (x, y)
      -- second, we extract the direction `v` and length `l` of the brick
      let (v, l) : vol × Nat := if a < d then ((1, 0, 0), d - a) else
                                if b < e then ((0, 1, 0), e - b) else
                                ((0, 0, 1), f - c)
      ⟨(a, b, c), v, l⟩
    | _ => panic "oh no!"

/-- `bricks dat` parses all the strings in the array `dat`, producing an array of `brick`s. -/
def bricks (dat : Array String) : Array brick :=
  dat.map String.toBrick

/-- `brick.toArray bk` takes as input a `brick` `bk` and converts it into the array of `vol`umes
that `bk` occupies. -/
def brick.toArray (bk : brick) : Array vol :=
  (Array.range (bk.lth + 1)).map (bk.src + · * bk.dir)

/-- `getPos dat` takes as input an array `dat` of strings.
It returns the collection of all the `vol`umes occupied by some brick determined by `dat`.
-/
def getPos (dat : Array String) : HashSet vol :=
  (bricks dat).foldl (·.insertMany ·.toArray) ∅

/-- `checkNbr bks d bk` takes as input
* a collection of occupied `vol`umes `bks,
* a `vol`ume `d`, and
* a `brick` `bk`.

It determines if translating `bk` by `d` we overlap with a `vol`ume occupied by `bks`. -/
def checkNbr (bks : HashSet vol) (d : vol) (bk : brick) : Bool :=
  let (bk, dir, lth) :=
    -- if the brick is vertical...
    if bk.dir = (0, 0, 1) then
      -- and we are looking for neighbours below
      (if d = (0, 0, -1) then (bk.src, bk.dir, 0) else (bk.src + bk.lth * bk.dir, bk.dir, 0)) else
      (bk.src, bk.dir, bk.lth)
  (2 ≤ bk.2.2) && ((Array.range lth.succ).map fun n =>
    (bks.get? (bk + n * dir + d)).isNone).all (·)

/-- `canFall bks bk` check whether, with the occupied space in `bks`, the brick `bk` can fall. -/
def canFall (bks : HashSet vol) (bk : brick) : Bool :=
  checkNbr bks (0, 0, -1) bk

/-- `supports bks bk` check whether, with the occupied space in `bks`, the brick `bk` has
some brick on top of itself. -/
def supports (bks : HashSet vol) (bk : brick) : Bool :=
  checkNbr bks (0, 0, 1) bk

/-- We can add a `vol`ume to a brick, simply by adding the `vol`ume to the source of the brick. -/
instance : HAdd brick vol brick where
  hAdd b p := ⟨b.src + p, b.dir, b.lth⟩

/-- `canFallBy bks bk` determines by how many steps can a brick `bk` fall before meeting
either the plane with `z`-coordinate equal to `1` or a `vol`ume occupied by `bks`. -/
def canFallBy (bks : HashSet vol) (bk : brick) : Nat :=
  let down : vol := (0, 0, -1)
  Id.run do
  let mut amt := 0
  let mut bk := bk
  while canFall bks bk do
    amt := amt + 1
    bk := bk + down
  return amt

/-- `fallOne bksH bk` takes as input a collection `bksH` of occupied volume and a brick`bk`.
It replaces the volume in `bksH` occupied by `bk` by the volume occupied by the result of
making `bk` fall as much as `canFallBy bksH bk` allows. -/
def fallOne (bksH : HashSet vol) (bk : brick) : HashSet vol × brick :=
  let n := canFallBy bksH bk
  let old := (Array.range bk.lth.succ).map (bk.src + · * bk.dir)
  let s : HashSet vol := old.foldl (init := bksH) fun s x =>
    (s.erase x).insert (x + n * ((0, 0, -1) : vol))
  (s, ⟨bk.src + n * ((0, 0, -1) : vol), bk.dir, bk.lth⟩)

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  Id.run do
  let mut bksH := getPos dat
  let mut bks := (bricks dat).qsort fun b c => (b.src.2.2 < c.src.2.2)
  let mut bksFall := bks.find? <| canFall bksH
  let mut con := 0
  while bksFall.isSome do
    con := con + 1
    let curr := bksFall.get!
    let (newH, newB) := fallOne bksH curr
    bksH := newH
    bks := (bks.modify (bks.findIdx? (· == curr)).get! (fun _ => newB))
    bksFall := bks.find? <| canFall bksH
  let mut tot := 0
  let bks' : HashSet brick := .ofArray bks
  for bk in bks' do
    let old := bk.toArray
    let mut s : HashSet vol := bksH
    for x in old do
      s := (s.erase x)
    let fallList := Id.run do
      for x in (bks'.erase bk) do
        if (canFall s x) then return false
      return true
    if fallList then tot := tot + 1
  return tot

#assert part1 atest == 5

#eval "Day 22, part 1: 441   (prerecorded, the actual computation takes: ~45 seconds!"
--set_option trace.profiler true in solve 1 441

/-!
#  Question 2
-/

def findFalls (bksH : HashSet vol) (bks : Array brick) (one : brick) : Nat :=
  Id.run do
  let mut falling := #[one]
  let mut falls := 0
  let mut bksH' := bksH
  let mut bks' := bks
  let mut curr := falling[0]!
  while falling != #[] do
    curr := falling[0]!
    for i in curr.toArray do bksH' := bksH'.erase i
    bks' := bks'.erase curr
    falls := falls + 1
    falling := falling.erase curr
    for i in bks' do
      if canFall bksH' i ∧ ! falling.contains i then
        falling := falling.push i
  falls - 1

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat :=
  Id.run do
  let mut bksH := getPos dat
  let mut bks := (bricks dat).qsort fun b c => (b.src.2.2 < c.src.2.2)
  let mut bksFall := bks.find? <| canFall bksH
  let mut con := 0
  while bksFall.isSome do
    con := con + 1
    let curr := bksFall.get!
    let (newH, newB) := fallOne bksH curr
    bksH := newH
    bks := (bks.modify (bks.findIdx? (· == curr)).get! (fun _ => newB))
    bksFall := bks.find? <| canFall bksH
  let tots := bks.map <| findFalls bksH bks
  return tots.sum

#assert part2 atest == 7

#eval "Day 22, part 2: 80778 (prerecorded, the actual computation takes: ~24 minutes!"
--solve 2 80778

end Day22
