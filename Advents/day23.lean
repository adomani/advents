import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day23.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
def test := "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- the four directions `L`eft, `R`ight, `U`p, `D`own,
and... `S`tay. -/
inductive dir | L | R | U | D | S
  deriving BEq, DecidableEq, Inhabited, Repr

/-- represent each direction by the corresponding arrow. -/
instance : ToString dir where
  toString | .L => "←" | .R => "→" | .U => "↑" | .D => "↓" | .S => "·"

/-- `dir.toPos` converts a `dir`ection to the corresponding unit vector. -/
def dir.toPos : dir →  pos
  | .D => (  1,   0)
  | .U => (- 1,   0)
  | .L => (  0, - 1)
  | .R => (  0,   1)
  | .S => (  0,   0)

/-- `Char.toDir` converts a single character to the corresponding unit vector. -/
def Char.toDir : Char → dir
  | '<' => .L
  | '>' => .R
  | '^' => .U
  | 'v' => .D
  | _ => .S

/-- `uts` are the unit vectors pointing in the four cardinal directions. -/
def uts : Array pos := #[(  1,   0), (- 1,   0), (  0, - 1), (  0,   1)]

/-- `getPos dat` takes as input an array `dat` of strings, representing the input of the puzzle.
It returns a `HashMap` enconding, for each `pos`ition on the map, the corresponding information:
* a `.S` `dir`ection, symbolizing that there is no ice on the `pos`ition;
* a `dir`ection pointing where the icy slope pushes.
-/
def getPos (dat : Array String) : HashMap pos dir :=
  Id.run do
  let mut new : HashMap pos dir := .empty
  for i in [:dat.size] do
    let ri := dat[i]!.toList
    for j in [:ri.length] do
      if ri[j]! ≠ '#' then new := new.insert (i, j) ri[j]!.toDir
  return new

/-- `pos.nbs'' mz p` takes as input the `HashMap` `mz` encoding the maze and a position `p`.
It returns an array consisting of the available unit vectors pointing in the directions
accessible from `p`, *ignoring* the icy slopes. -/
def pos.nbs' (mz : HashMap pos dir) (p : pos) : Array pos :=
  uts.filter fun u => (mz.find? (p + u)).isSome

/-- `pos.nbs'' mz p` takes as input the `HashMap` `mz` encoding the maze and a position `p`.
It returns an array consisting of the available unit vectors pointing in the directions
accessible from `p`, *taking into account* the icy slopes. -/
def pos.nbs'' (mz : HashMap pos dir) (p : pos) : Array pos :=
  uts.filter fun u =>
    match (mz.find? (p + u)) with
      | some d => if d.toPos + u = (0, 0) then false else true
      | none => false

/-- `pos.nbs mz p` takes as input the `HashMap` `mz` encoding the maze and a position `p`.
It returns an array consisting of the available positions adjacent to `p`. -/
def pos.nbs (mz : HashMap pos dir) (p : pos) : Array pos :=
  (p.nbs' mz).map (p + ·)

/-
def forced (mz : HashMap pos dir) (p : pos) : Bool :=
  (p.nbs mz).size ≤ 2
-/

/-- `go mz p next` takes as input the `HashMap` `mz` encoding the maze and two consecutive positions
`p next`.
It goes through all the forced steps inside the maze, starting from `p` and moving in the direction
`next` with no backtracking.
It stops when it reaches a location where a choice can be made.
It returns the number of steps taken from `p` until the choice, as well as the array of pairs
consisting of the same final position and each of the available next steps. -/
def go (mz : HashMap pos dir) (p next : pos) : Nat × Array (pos × pos) :=
  let path := Id.run do
    let mut curr := next
    let mut pth := #[].push p
    let mut prev := p
    let mut con := 0
    while con ≤ 2000 do
      con := con + 1
      pth := pth.push curr
      match curr.nbs mz with
        | nb@#[_, _] =>
          let ncurr := (nb.erase prev)[0]!
          prev := curr
          match mz.find? ncurr with
            | none => dbg_trace "oh no!"; return default
            | some d =>
              if d = .S ∨ d.toPos = (ncurr - prev) then
                curr := ncurr
              else return pth
        | _ => return pth
    return pth
  let fin := path.back
  let newSteps := (fin.nbs'' mz).map (· + fin)
  (path.size - 1, newSteps.map (Prod.mk fin))

/-- If `α` is a type with an `Ord` instance, then we introduce an `Ord` instance
on `Array α` lexicogrphically. -/
partial
instance {α} [Inhabited α] [Ord α] : Ord (Array α) where
  compare := ArrayLexCompare where
  /-- `ArrayLexCompare a b` is the lexicogrphic `Ordering` on two arrays `a` and `b`.
  It is used to define the `Ord` instance on `Array`s. -/
  ArrayLexCompare {α} [Inhabited α] [Ord α] (a b : Array α) : Ordering :=
  match a.size, b.size with
    | 0, 0 => .eq
    | 0, _ => .lt
    | _, 0 => .gt
    | _, _ => match compare a.back b.back with
      | .eq => ArrayLexCompare a.pop b.pop
      | c => c

/-- Two types with an `Ord` instance determine an `Ord` instance on their product
by comparing pairs lexicographically. -/
instance {α β} [Ord α] [Ord β] : Ord (α × β) where
  compare x y := match compare x.1 y.1 with
    | .eq => compare x.2 y.2
    | g => g

/-- `findPaths dat` takes as input an array `dat` of strings and returns
the array of all the lengths of the paths without repetitions in the maze
determined by `dat`, sorted in descending order. -/
def findPaths (dat : Array String) : Array Nat :=
  let mz := getPos dat
  let sz := dat.size - 2
  Id.run do
  let mut x : RBTree (Nat × Array (pos × pos)) (fun x y => compare x y) := RBTree.empty
  x := x.insert (0, #[((0, 1), (1, 1))])
  let mut con := 0
  let mut tots := #[]
  while ! x.isEmpty do
    con := con + 1
    for old@(lth, steps) in x do
      let (lthNew, newPairs) := go mz steps.back.1 steps.back.2
      for fins in newPairs do
        if fins.2 = ((sz : Int), (sz : Int)) then
          tots := tots.push (lth + lthNew)
        else x := x.insert (lth + lthNew, steps.push fins)
      x := x.erase old
  tots.qsort (· > ·)

#assert findPaths atest == #[94, 90, 86, 82, 82, 74]

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  (findPaths dat)[0]!

#assert part1 atest == 94

solve 1 2366

/-!
#  Question 2
-/
