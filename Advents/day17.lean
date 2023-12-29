import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day17.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- convert the input for the problem into a `HashMap`,
assigning to each `pos`ition the corresponding natural number. -/
def Array.toNats (dat : Array String) : HashMap pos Nat :=
  let lth := dat.size
  let wdth := dat[0]!.length
  .ofList <| Id.run do
    let mut tot := []
    for i in [:lth] do
      let digs := dat[i]!.toList.map (String.toNat! ⟨[·]⟩)
      let row : List (pos × Nat) := (List.range wdth).zipWith (fun e : Nat => Prod.mk (i, e)) digs
      tot := tot ++ row
    return tot

/-- `dir.rev d` reverses the direction `d`:
it swaps the pairs `U ↔ D` and `L ↔ R` and fixes `S`. -/
def dir.rev : dir → dir
    | .U => .D
    | .L => .R
    | .D => .U
    | .R => .L
    | .S => .S

def dirs : Array dir := #[.U, .D, .R, .L]

#assert (dirs.push .S).map dir.rev == #[.D, .U, .L, .R, .S]

/-- Adding a direction to a position moves one step in the corresponding direction. -/
instance : HAdd dir pos pos where
  hAdd := (dir.toPos · + ·)

/-- the available moves in a grid of size `sz × sz` at the `pos`ition `p`. -/
def mvs (p : pos) (szx : Nat) (szy : Nat) : Array dir :=
  let up    : Option dir := if 0 < p.1   then some .U else none
  let left  : Option dir := if 0 < p.2   then some .L else none
  let down  : Option dir := if p.1 < szx then some .D else none
  let right : Option dir := if p.2 < szy then some .R else none
  #[right, down, up, left].reduceOption

#assert mvs (4, 3) 4 4 == #[.R, .U, .L]

--abbrev path := Array Nat × pos × pos
--
--instance : Ord path where
--  compare x y := compare x.1.sum y.1.sum

/-- A `path` is a structure containing
* `.sum`, the current sum of the values along the path;
* `.loc`, the array of visited `pos`itions;
* `.cpos`, the current position;
* `.past`, the last three `dir`ections in which the path moved.
-/
structure path where
  /-- `path.sum`, the current sum of the values along the path -/
  (sum  : Nat)
  /-- `path.loc`, the array of visited `pos`itions -/
  (loc  : Array pos)
  /-- `path.cpos`, the current position -/
  (cpos : pos)
  /-- `path.past`, the last three `dir`ections in which the path moved -/
  (past : dir × dir × dir)

instance : Inhabited path := ⟨default, #[default], default, (.S, .S, .S)⟩

instance : ToString path where
  toString x := s!"\n* sum: {x.sum}, past: {x.past}\n* {x.loc}"

#eval
  let df : path := default
  (df.sum, df.loc, df.cpos, df.past)

instance {α} [Ord α] : Ord (List α) where
  compare := LexCompare where
  LexCompare {α} [Ord α] : List α → List α → Ordering
    | [], [] => .eq
    | [], _ => .lt
    | _, [] => .gt
    | x::xs, y::ys => match compare x y with
      | .eq => LexCompare xs ys
      | c => c

partial
instance {α} [Inhabited α] [Ord α] : Ord (Array α) where
  compare := ArrayLexCompare where
  ArrayLexCompare {α} [Inhabited α] [Ord α] (a b : Array α) : Ordering :=
  match a.size, b.size with
    | 0, 0 => .eq
    | 0, _ => .lt
    | _, 0 => .gt
    | _, _ => match compare a.back b.back with
      | .eq => ArrayLexCompare a.pop b.pop
      | c => c

instance {α β} [Ord α] [Ord β] : Ord (α × β) where
  compare x y := match compare x.1 y.1 with
    | .eq => compare x.2 y.2
    | g => g

local instance : ToString Ordering where
  toString | .eq => "(=)" | .lt => "(<)" | .gt => "(>)"

#eval do IO.println <| Ord.compare (0, 2) (1, 1)

instance : Ord path where
  compare x y := compare (x.sum, -(x.cpos.1 + x.cpos.2), x.loc) (y.sum, -(y.cpos.1 + y.cpos.2), y.loc)

def path.add (gr : HashMap pos Nat) (p : path) (x : dir) : path :=
  let new := x + p.cpos
  { sum  := p.sum + gr.findD new 0
    loc  := p.loc.push new
    cpos := new
    past := (p.past.2.1, p.past.2.2, x) }

def getNbs (curr : path) (sz : Nat) (szy : Nat) : Array dir :=
  let cpos := curr.cpos
  (mvs cpos sz szy).filter fun d : dir =>
    let (d0, d1, d2) := curr.past
    (! (d == d2.rev)) &&
    (! (d0 == d1 && d0 == d2 && d0 == d))

def pos.approx (c : pos) : dir :=
  match compare c.1 c.2, compare c.1 (-c.2) with
    | .lt, .lt => .D
    | .lt, _   => .D
    | .gt, .lt => .U
    | .gt, _   => .U
    | .eq, .lt => .R
    | .eq, .gt => .L
    | .eq, .eq => .S

#exit

def pointTo (c d : pos) : Array dir :=
  match compare c.1 d.1, compare c.2 d.2 with
    | .lt, .lt => #[.D, .R, .U, .L]
    | .lt, _   => #[.D, .L, .U, .R]
    | .gt, .lt => #[.U, .R, .D, .L]
    | .gt, _   => #[.U, .L, .D, .R]
    | .eq, .lt => #[.R, .D, .U, .L]
    | .eq, .gt => #[.L, .D, .U, .R]
    | .eq, .eq => dirs

def tryStraight (curr : path) (tgt : pos) : Array dir :=
  let cpos := curr.cpos
  match dirs.filter (dir.toPos · + cpos == tgt) with
    | #[d] => #[d]
    | _ =>
      let M := (max 0 (max (max cpos.1 cpos.2) (max tgt.1 tgt.2))).toNat
      let cands := getNbs curr M M
      (pointTo cpos tgt).filter cands.contains

#eval
  let src : path := default
  let tgts : Array pos := #[(10, 0), (0, 10)]
  tgts.map <| tryStraight src

partial
def mkLine (grid : HashMap pos Nat) (p : path) (sz : Nat) (szy : Nat) : path :=
  let pcurr := p.cpos
  if pcurr = ((sz, szy) : pos) then p else
    mkLine grid (p.add grid (tryStraight p (sz, szy))[0]!) sz szy

def getNbs' (curr : path) (sz : Nat) (szy : Nat) : Array dir :=
  let cpos := curr.cpos
  (mvs cpos sz szy).filter fun d : dir =>
    let cp3 := curr.past.2.2
    ((curr.loc.contains cpos)) &&
    ( let (dx, dy) := cpos - ((sz/2 : Int), (sz/2 : Int))
      (10 * sz / (2 * 11))^2 ≤ dx ^ 2 + dy ^ 2) &&
--    ((if 3 ≤ curr.loc.size then let c1 := curr.loc.pop.pop.back; c1.1 < cpos.1 || c1.2 < cpos.2 else true )) &&
    (! (d == cp3.rev)) &&
    (! (curr.past.1 == cp3 && curr.past.2.1 == cp3 && d == cp3)) --&& (! (d + cpos) ∈ curr.loc)

#eval getNbs ⟨0, #[(1, 0), (1, 1)], (1, 1), (.R, .R, .R)⟩ 10 10

--#exit

#eval do
  let dat := atest
  let sz := dat.size
  let grid := dat.toNats
  let ip : pos := (0, 0)
  let ip : pos := (0, sz)
  let init : path := ⟨grid.findD ip default, #[ip], ip, (.S, .S, .S)⟩ --(#[grid.findD ip default], (.X, ip))
  IO.println <| getNbs init sz sz


def test1 := "03
21"

def test2 :=
"111112
222212
221122
222112
222211
222221"

set_option profiler true
def btest := (test2.splitOn "\n").toArray

#eval atest
#eval btest

#eval (atest.size, atest[0]!.length)
#eval (btest.size, btest[0]!.length)
def mkLB (p : path) (sz szy : Nat) : Nat :=
  p.sum + (sz - p.cpos.1).natAbs + (szy - p.cpos.2).natAbs

def mkUB (p : path) (sz szy : Nat) : Nat :=
  p.sum + 1 * ((sz - p.cpos.1).natAbs + (szy - p.cpos.2).natAbs)

partial
def mkPath1 (grid : HashMap pos Nat) (p : path) (sz : Nat) (szy : Nat) : path :=
  let pcurr := p.cpos
  if pcurr = ((sz, szy) : pos) then p else
    let next := getNbs p sz szy
    if pcurr.1 ≤ pcurr.2 then
      mkPath1 grid (p.add grid next[0]!) sz szy
    else
      let mv : dir := if next.contains .D then .D else if next.contains .R then .R else .U
      mkPath1 grid (p.add grid mv) sz szy

partial
def mkPath (grid : HashMap pos Nat) (p : path) (sz : Nat) (szy : Nat) : path :=
  let pcurr := p.cpos
  if pcurr = ((sz, szy) : pos) then p else
    let next := getNbs p sz szy
    if pcurr.1 ≤ pcurr.2 then
      if szy-2 ≤ pcurr.2 then
        let mv : dir := if next.contains .D then .D else if next.contains .R then .R else .L
        mkPath grid (p.add grid mv) sz szy
      else
      if pcurr.1 ≤ 2 then
        let mv : dir := if next.contains .R then .R else if next.contains .U then .U else .D
        mkPath grid (p.add grid mv) sz szy
      else
        mkPath1 grid (p.add grid next[0]!) sz szy
    else
      let mv : dir := if next.contains .D then .D else if next.contains .R then .R else .U
      mkPath grid (p.add grid mv) sz szy

def findPaths (grid : HashMap pos Nat) (ip fin : pos) (szx szy : Nat) : Array path :=
  let init : path := ⟨0, #[ip], ip, (.S, .S, .S)⟩
  Id.run do
  let mut toEnd : Array path := #[]
  let mut pths : RBTree path compare := RBTree.empty.insert init
  let mut upb := (mkPath grid init szx szy).sum
  let mut con := 1
  let mut curUB := (mkPath grid pths.min.get! szx szy).sum
--  let mut curPth := default
  while ! pths.isEmpty do
    con := con + 1
    for cc in pths do
      curUB := min curUB (mkPath grid cc szx szy).sum
      if cc.cpos = fin then
        toEnd := toEnd.push cc
        if cc.sum ≤ upb then
          upb := min upb cc.sum
--          curPth := cc
        toEnd := toEnd.filter (path.sum · ≤ upb)
      else
--        if (mkPath grid cc szx szy).sum < curUB + 15 then
          let nbs := (getNbs cc szx szy)
          let news := nbs.map fun (x : dir) => cc.add grid x
          for nn in news do
            if nn.sum ≤ upb then
              pths := pths.insert nn
      pths := pths.erase cc
    dbg_trace s!"con: {con} size: {pths.size}, min: {pths.min.get!.sum}"
  toEnd
  --curPth


/-
con: 2 size: 2, min: 1
con: 3 size: 4, min: 4
con: 4 size: 10, min: 5
con: 5 size: 9, min: 6
con: 6 size: 12, min: 9
con: 7 size: 23, min: 10
con: 8 size: 25, min: 13
con: 9 size: 30, min: 15
con: 10 size: 54, min: 16
con: 11 size: 84, min: 17
con: 12 size: 82, min: 21
con: 13 size: 75, min: 22
con: 14 size: 91, min: 25
con: 15 size: 138, min: 27
con: 16 size: 174, min: 29
con: 17 size: 105, min: 31
con: 18 size: 118, min: 36
con: 19 size: 121, min: 40
con: 20 size: 205, min: 42
con: 21 size: 227, min: 44
con: 22 size: 217, min: 46
con: 23 size: 227, min: 48
con: 24 size: 346, min: 50
con: 25 size: 426, min: 54
con: 26 size: 349, min: 57
con: 27 size: 336, min: 59
con: 28 size: 435, min: 65
con: 29 size: 426, min: 68
con: 30 size: 436, min: 70
con: 31 size: 427, min: 72
con: 32 size: 454, min: 78
con: 33 size: 338, min: 79
con: 34 size: 404, min: 82
con: 35 size: 391, min: 83
con: 36 size: 410, min: 85
con: 37 size: 307, min: 87
con: 38 size: 566, min: 88
con: 39 size: 590, min: 92
con: 40 size: 602, min: 94
con: 41 size: 440, min: 98
con: 42 size: 707, min: 100
con: 43 size: 868, min: 104
con: 44 size: 567, min: 106
con: 45 size: 521, min: 110
con: 46 size: 324, min: 112
con: 47 size: 244, min: 116
con: 48 size: 159, min: 119
con: 49 size: 70, min: 122
con: 50 size: 22, min: 127
con: 51 size: 22, min: 136
con: 52 size: 22, min: 139
con: 53 size: 14, min: 144
con: 54 size: 28, min: 146
con: 55 size: 58, min: 148
con: 56 size: 104, min: 150
con: 57 size: 115, min: 154
con: 58 size: 151, min: 156
con: 59 size: 198, min: 160
con: 60 size: 295, min: 161
con: 61 size: 361, min: 167
con: 62 size: 464, min: 172
con: 63 size: 465, min: 174
con: 64 size: 528, min: 177
con: 65 size: 606, min: 178
con: 66 size: 719, min: 181
con: 67 size: 1004, min: 185
con: 68 size: 1440, min: 188
con: 69 size: 2136, min: 193
con: 70 size: 2574, min: 197
con: 71 size: 1386, min: 198
con: 72 size: 375, min: 202
con: 73 size: 67, min: 203
con: 74 size: 0, min: 0
#[
* sum: 206, past: (→, (↓, →))
* #[(0, 0), (0, 1), (0, 2), (1, 2), (1, 3), (1, 4), (0, 4), (0, 5), (1, 5), (1, 6), (1, 7), (1, 8), (2, 8), (2, 9), (2, 10), (2, 11), (3, 11), (3, 12), (3, 13), (3, 14), (2, 14), (2, 15), (2, 16), (2, 17), (3, 17), (3, 18), (3, 19), (2, 19), (2, 20), (1, 20), (1, 21), (1, 22), (0, 22), (0, 23), (0, 24), (0, 25), (1, 25), (1, 26), (1, 27), (1, 28), (0, 28), (0, 29), (0, 30), (0, 31), (1, 31), (1, 32), (1, 33), (1, 34), (0, 34), (0, 35), (0, 36), (0, 37), (1, 37), (2, 37), (2, 38), (2, 39), (2, 40), (1, 40), (1, 41), (1, 42), (1, 43), (0, 43), (0, 44), (0, 45), (0, 46), (1, 46), (2, 46), (2, 47), (2, 48), (2, 49), (3, 49), (3, 50)],
* sum: 206, past: (→, (→, →))
* #[(0, 0), (0, 1), (0, 2), (1, 2), (1, 3), (1, 4), (0, 4), (0, 5), (1, 5), (1, 6), (1, 7), (1, 8), (2, 8), (2, 9), (2, 10), (2, 11), (3, 11), (3, 12), (3, 13), (3, 14), (2, 14), (2, 15), (2, 16), (2, 17), (3, 17), (3, 18), (3, 19), (2, 19), (2, 20), (1, 20), (1, 21), (1, 22), (0, 22), (0, 23), (0, 24), (0, 25), (1, 25), (1, 26), (1, 27), (1, 28), (0, 28), (0, 29), (0, 30), (0, 31), (1, 31), (1, 32), (1, 33), (1, 34), (0, 34), (0, 35), (0, 36), (0, 37), (1, 37), (2, 37), (2, 38), (2, 39), (2, 40), (1, 40), (1, 41), (1, 42), (1, 43), (0, 43), (0, 44), (0, 45), (0, 46), (1, 46), (2, 46), (2, 47), (3, 47), (3, 48), (3, 49), (3, 50)]]

* sum: 206, past: (→, (↓, →))
* #[(0, 0), (0, 1), (0, 2), (1, 2), (1, 3), (1, 4), (0, 4), (0, 5), (1, 5), (1, 6), (1, 7), (1, 8), (2, 8), (2, 9), (2, 10), (2, 11), (3, 11), (3, 12), (3, 13), (3, 14), (2, 14), (2, 15), (2, 16), (2, 17), (3, 17), (3, 18), (3, 19), (2, 19), (2, 20), (1, 20), (1, 21), (1, 22), (0, 22), (0, 23), (0, 24), (0, 25), (1, 25), (1, 26), (1, 27), (1, 28), (0, 28), (0, 29), (0, 30), (0, 31), (1, 31), (1, 32), (1, 33), (1, 34), (0, 34), (0, 35), (0, 36), (0, 37), (1, 37), (2, 37), (2, 38), (2, 39), (2, 40), (1, 40), (1, 41), (1, 42), (1, 43), (0, 43), (0, 44), (0, 45), (0, 46), (1, 46), (2, 46), (2, 47), (2, 48), (2, 49), (3, 49), (3, 50)]

* sum: 206, past: (→, (→, →))
* #[(0, 0), (0, 1), (0, 2), (1, 2), (1, 3), (1, 4), (0, 4), (0, 5), (1, 5), (1, 6), (1, 7), (1, 8), (2, 8), (2, 9), (2, 10), (2, 11), (3, 11), (3, 12), (3, 13), (3, 14), (2, 14), (2, 15), (2, 16), (2, 17), (3, 17), (3, 18), (3, 19), (2, 19), (2, 20), (1, 20), (1, 21), (1, 22), (0, 22), (0, 23), (0, 24), (0, 25), (1, 25), (1, 26), (1, 27), (1, 28), (0, 28), (0, 29), (0, 30), (0, 31), (1, 31), (1, 32), (1, 33), (1, 34), (0, 34), (0, 35), (0, 36), (0, 37), (1, 37), (2, 37), (2, 38), (2, 39), (2, 40), (1, 40), (1, 41), (1, 42), (1, 43), (0, 43), (0, 44), (0, 45), (0, 46), (1, 46), (2, 46), (2, 47), (3, 47), (3, 48), (3, 49), (3, 50)]

-/

#eval do
  let tgt : Nat × Nat := (10, 125)
  let ip : pos := (3, 100)
  let dat := btest
  let dat := atest
  let dat ← IO.FS.lines input --:= (test1.splitOn "\n").toArray
  let szx : Nat := dat.size-1
  let szy : Nat := dat.size-1
  let grid := dat.toNats
  let init : path := ⟨0, #[ip], ip, (.S, .S, .S)⟩
  let line := mkLine grid init tgt.1 tgt.2
  IO.println <| line
  draw <| toPic line.loc tgt.2.succ tgt.1.succ
#exit

  let toEnd := findPaths grid ip (tgt.1, tgt.2) tgt.1 tgt.2
  IO.println <| toEnd
  for p in toEnd do
    IO.println p
--    draw <| toPic p.loc szx.succ szy.succ
--  draw dat
#exit
--/-
#eval do
  let tak := 2--0
  let dat := btest
  let dat := atest
  let dat ← IO.FS.lines input --:= (test1.splitOn "\n").toArray
  let sz : Nat := dat.size-1
  let grid := dat.toNats
  let ip : pos := (1, 1)
  let init : path := ⟨/-(grid.findD ip default) *-/ 0, #[ip], ip, (.S, .S, .S)⟩
  let compl := mkPath grid init sz sz
  IO.println <| compl
  draw <| toPic compl.loc sz.succ sz.succ
--  draw dat
--/
#exit

-- (  0,   0) --> (  3,  50): 206
-- (  3,  50) --> (  3, 100): 234
-- (  3, 100) --> ( 10, 135): 234

#eval return (← IO.FS.lines input).size

#eval do
  let tak := 4
  let dat := btest
  let dat := atest
  let dat ← IO.FS.lines input --:= (test1.splitOn "\n").toArray
  let sz : Nat := dat.size-1
  let szy : Nat := dat.size-1
  let grid := dat.toNats
  let ip : pos := (0, 0)
  let init : path := ⟨/-(grid.findD ip default) *-/ 0, #[ip], ip, (.S, .S, .S)⟩
  let mut toEnd : Array path := #[]
  let mut pths : RBTree path compare := RBTree.empty.insert init
  let mut upb := (mkPath grid init sz szy).sum
  let mut con := 1
--  let mut curLB := mkLB sz pths.min.get!
  let mut curUB := (mkPath grid pths.min.get! sz szy).sum
  while con < tak && ! pths.isEmpty do
    con := con + 1
--    let mut spth : RBTree path compare := RBTree.empty
    for cc in pths do
--        let (cx, cy) := cc.cpos
        curUB := min curUB (mkPath grid cc sz szy).sum
        if cc.cpos = ((sz, sz) : pos) then
          toEnd := toEnd.push cc
          upb := min upb cc.sum
          toEnd := toEnd.filter (path.sum · ≤ upb + 1)
        else
          if (mkPath grid cc sz szy).sum < curUB + 5 then
--            let bd := 1
--            if ((cx + cy)) * (10 * con + 10) ≤ cc.loc.size * (10 * con + 10) then
--              if cx ≤ bd ∨ sz - bd ≤ cy then
            let nbs := (getNbs cc sz szy)
            --.erase
            --  (if cc.loc.size ≤ (sz - 2) / 2 then .L else
            --   if (sz + 2) / 2 ≤ cc.loc.size then .U else .S)
            let news := nbs.map fun (x : dir) => cc.add grid x
            for nn in news do
              if nn.sum ≤ upb then
                pths := pths.insert nn
        pths := pths.erase cc
--    curLB := min curLB (mkLB sz pths.min.get!)
--    for x in pths do if curLB < mkLB sz x then pths := pths.erase x
    IO.println s!"con: {con} size: {pths.size}, min: {pths.min.get!.sum}"
  IO.println ""
--  IO.print pths.toList
  let pm := pths.min
  IO.print (pm, mkPath grid pm.get! sz szy)
--  draw dat
--  for p in pths do IO.println p.sum
  IO.println "\ntoEnd:\n"
  let sorted := (toEnd.qsort (path.sum · < path.sum ·)) --[0]!
  for s in [sorted[0]!] do IO.println s; draw <| toPic s.loc sz.succ sz.succ
--  draw <| toPic (sorted.map (path.loc)) sz sz
--  IO.println sorted -- (sorted[0]!.sum)
--  for p in toEnd do IO.println p.sum
--  draw dat

#exit
-- to high: 1423
-- to high: 1408
#eval do
  let dat := btest
  let sz := dat.size - 1
  let sz : Nat := 1
  IO.println sz
  let grid := dat.toNats
  let ip : pos := (0, 0)
  let init : path := ⟨(grid.findD ip default) * 0, #[ip], ip, (.X, .X)⟩ --(#[grid.findD ip default], (.X, ip))
  let mut toEnd : Array path := #[]
  let mut pths : RBTree path compare := RBTree.empty.insert init
  let mut con := 0
  let mut upb := 1000
  let mut curr := init
  while --con ≤ 2500000 && pths.min.isSome do --
   ! pths.isEmpty do
--   for p in pths do IO.println p
   for cc in pths do
    con := con + 1
--    let cc := pths.min.get!
--    dbg_trace cc
    if cc.sum ≤ upb then
      let cpos := cc.cpos
      if cc.cpos = ((sz, sz) : pos) then
        toEnd := toEnd.push cc
        upb := min upb cc.sum
      else
        let nbs := getNbs sz cc
  --      dbg_trace s!"cc = {cc} and nbs = {nbs}"
        -- (mvs (sz+1) cpos).filter fun d : dir =>
        --  (! (d == cc.past.2.rev)) &&
        --  (! (cc.past.1 == cc.past.2 && d == cc.past.1)) && (! (d + cpos) ∈ cc.loc)
      --  let news : Array pos := nbs.map fun x : dir => (x + cpos)
        let news := nbs.map fun x : dir => cc.add grid x
        for nn in news do pths := pths.insert nn
--        pths := news.foldl (fun (pts : RBTree path compare) (n : path) =>
--          --let nval := grid.find? n.cpos
--          pts.insert n) pths
    pths := pths.erase cc
--  IO.println s!"con = {con}; Print pths"
--  for p in pths do IO.println p
--  IO.println nbs
--  IO.println news
  IO.println "\ntoEnd:\n"
  IO.println (toEnd.qsort (path.sum · < path.sum ·))[0]!
  for p in toEnd do IO.println p.sum
  IO.println s!"con = {con}"

--  for i in pths do IO.println s!"{i}"
--  for i in grid do IO.println s!"{i}"

  draw <| dat

#exit
  let o : Array (pos × Nat):= dat.map fun i =>
    default
--  let o : Array (pos × Nat):= dat.map fun i =>
--    let digs := i.toList.map Char.toNat
--    let row := (List.range i.length).zipWith (fun e => (dat.findIdx? i)) digs
--    default

--  let o := (List.range dat.size).map fun i =>
--    let digs :=
  .empty






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
