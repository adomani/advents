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

#assert #[.U, .D, .R, .L, .S].map dir.rev == #[.D, .U, .L, .R, .S]

/-- Adding a direction to a position moves one step in the corresponding direction. -/
instance : HAdd dir pos pos where
  hAdd := (dir.toPos · + ·)

/-- the available moves in a grid of size `sz × sz` at the `pos`ition `p`. -/
def mvs (sz : Nat) (p : pos) : Array dir :=
  let up    : Option dir := if 0 < p.1 then some .U else none
  let left  : Option dir := if 0 < p.2 then some .L else none
  let down  : Option dir := if p.1 < sz then some .D else none
  let right : Option dir := if p.2 < sz then some .R else none
  #[up, left, down, right].reduceOption

#assert mvs 4 (4, 3) == #[.U, .L, .R]

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
  deriving Inhabited

instance : ToString path where
  toString x := s!"\n* sum: {x.sum}, past: {x.past}\n* {x.loc}"

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
  compare x y := compare (x.sum, x.loc) (y.sum, y.loc)

def path.add (gr : HashMap pos Nat) (p : path) (x : dir) : path :=
  let new := x + p.cpos
  { sum  := p.sum + gr.findD new 0
    loc  := p.loc.push new
    cpos := new
    past := (p.past.2.1, p.past.2.2, x) }

def getNbs (sz : Nat) (curr : path) :=
  let cpos := curr.cpos
  (mvs sz cpos).filter fun d : dir =>
    let cp3 := curr.past.2.2
    (!(curr.loc.contains cpos)) &&
    ( let (dx, dy) := cpos - ((sz/2 : Int), (sz/2 : Int))
      (10 * sz / (2 * 11))^2 ≤ dx ^ 2 + dy ^ 2) &&
--    ((if 3 ≤ curr.loc.size then let c1 := curr.loc.pop.pop.back; c1.1 < cpos.1 || c1.2 < cpos.2 else true )) &&
    (! (d == cp3.rev)) &&
    (! (curr.past.1 == cp3 && curr.past.2.1 == cp3 && d == cp3)) --&& (! (d + cpos) ∈ curr.loc)

def getNbs' (sz : Nat) (curr : path) :=
  let cpos := curr.cpos
  (mvs sz cpos).filter fun d : dir =>
    let cp3 := curr.past.2.2
    ((curr.loc.contains cpos)) &&
    ( let (dx, dy) := cpos - ((sz/2 : Int), (sz/2 : Int))
      (10 * sz / (2 * 11))^2 ≤ dx ^ 2 + dy ^ 2) &&
--    ((if 3 ≤ curr.loc.size then let c1 := curr.loc.pop.pop.back; c1.1 < cpos.1 || c1.2 < cpos.2 else true )) &&
    (! (d == cp3.rev)) &&
    (! (curr.past.1 == cp3 && curr.past.2.1 == cp3 && d == cp3)) --&& (! (d + cpos) ∈ curr.loc)

#eval getNbs 10 ⟨0, #[(1, 0), (1, 1)], (1, 1), (.R, .R, .R)⟩

--#exit

#eval do
  let dat := atest
  let sz := dat.size
  let grid := dat.toNats
  let ip : pos := (0, 0)
  let ip : pos := (0, sz)
  let init : path := ⟨grid.findD ip default, #[ip], ip, (.S, .S, .S)⟩ --(#[grid.findD ip default], (.X, ip))
  IO.println <| getNbs sz init


def test1 := "03
21"

def test2 :=
"111112
222212
221122
222112
222211
222221"


def btest := (test2.splitOn "\n").toArray

#eval atest
#eval btest

#eval (atest.size, atest[0]!.length)
#eval (btest.size, btest[0]!.length)

#eval do
  let tak := 30
--  let dat := (dat.toList.take tak).toArray.map (String.take · tak)
  let dat := (test1.splitOn "\n").toArray
  let dat := atest
  let dat := btest
--  IO.println dat
--#exit
--  let sz : Nat := 1
  let sz : Nat := dat.size-1
--  IO.println s!"{sz} {dat.size-1}"
  let grid := dat.toNats
--  IO.println grid.toList
  let ip : pos := (0, 0)
  let init : path := ⟨(grid.findD ip default) * 0, #[ip], ip, (.S, .S, .S)⟩
  let mut toEnd : Array path := #[]
  let mut pths : RBTree path compare := RBTree.empty.insert init
  let mut upb := 200
  let mut con := 1
  while con ≤ tak && ! pths.isEmpty do
    con := con + 1
    IO.print s!"{con} "
--      let cc := pths.max.get!
--      pths := pths.erase cc
    let mut spth : RBTree path compare := RBTree.empty
    for cc in pths do
--        IO.println s!"{con} is con and {cc.loc.size} is the length of the path"
--      if cc.sum ≤ upb then
        if cc.cpos = ((sz, sz) : pos) then
--          dbg_trace "final place: {cc.cpos}"
          toEnd := toEnd.push cc
          upb := min upb cc.sum
          toEnd := toEnd.filter (path.sum · ≤ upb + 1)
  --        dbg_trace "updated upb: {upb} {cc.loc.size}"
        else
          let nbs := getNbs sz cc
          let news := nbs.map fun x : dir => cc.add grid x
          for nn in news do
            if nn.sum ≤ upb then
--              dbg_trace "inserting {nn}"
              spth := spth.insert nn
--        dbg_trace spth.toList
    dbg_trace spth.size
    pths := spth
--              pths := pths.insert nn
--    for cc in pths do if cc.loc.size < con then
----      dbg_trace "erasing with con {con} and sum {cc.sum}"
--      pths := pths.erase cc
      --if upb ≤ cc.sum then
  IO.println "\ntoEnd:\n"
  let sorted := (toEnd.qsort (path.sum · < path.sum ·)) --[0]!
--  for s in sorted do IO.println s.sum; draw <| toPic s.loc sz sz
--  draw <| toPic (sorted.map (path.loc)) sz sz
  IO.println sorted -- (sorted[0]!.sum)
--  for p in toEnd do IO.println p.sum
--  draw dat

#exit

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
