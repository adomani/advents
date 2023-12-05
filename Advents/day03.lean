import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day03.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

def test := "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"

instance : Add (Int × Int) where add x y := (x.1 + y.1, x.2 + y.2)

def nbs := Id.run do
  let mut t := #[]
  for i in [-1, 0, 1] do
    for j in [-1, 0, 1] do
      t := t.push (i,j)
  return t.erase (0,0)

#assert nbs == #[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

def has_number_nb (d : List (List Char)) (lx ly : Int) (p : Int × Int) : Array (Nat × Nat) :=
  Id.run do
    let mut cond := #[]
    for o in nbs do
      let (x, y) := p + o
      if (! x < 0) && (! y < 0) && (x < lx) && (y < ly) then
        let (x, y) := (x.natAbs, y.natAbs)
        let nc := d[x]![y]!.isDigit
        if nc then
          cond := cond.push (x, y)
    return cond

def symb_pos (s : List (List Char)) : List (Int × Int) :=
  Id.run do
    let mut tot := #[]
    for i in [:s.length] do
      for j in [:s[0]!.length] do
        let c := s[i]![j]!
        if (! c.isDigit && ! c == '.') then
          tot := tot.push ((i, j) : Int × Int)
    return tot.toList

/-- get the coordinates of digits that are neighbours of symbols. -/
def digs_in_nb (rows : List (List Char)) (symbs : List (Int × Int)) : Array (Nat × Nat) :=
  let rx := rows.length
  let ry := rows[0]!.length
  let tots := Id.run do
    let mut digs := #[]
    for s in symbs do
      let dig_nbs := has_number_nb rows rx ry s
      digs := digs ++ dig_nbs
    return digs
  tots

/-- returns the natural number and the endpoints of the consecutive digits
appearing in rows. -/
def digs_ends (rows : List (List Char)) : Array (Nat × (Nat × Nat) × (Nat × Nat)) :=
  let rx := rows.length
  let ry := rows[0]!.length
  Id.run do
    let mut tot := #[]
    for i in [:rx] do
      let mut inside := false
      let mut temp := (0, 0)
      let mut val := ""
      for j in [:ry] do
        let ch := rows[i]![j]!
        if !inside && ch.isDigit then
          inside := true
          temp := (i, j)
        if inside && ch.isDigit then
          val := val.push ch
        if inside && (!ch.isDigit || j == ry - 1) then
          inside := false
          tot := tot.push (val.toNat!, temp, (i, j))
          val := ""
    return tot

def part1 (rows : List (List Char)) : Nat :=
  let digs := digs_ends rows
  let inn_digs := digs_in_nb rows (symb_pos rows)
  Id.run do
  let mut tot := 0
  for d in digs do
    let (dig, fir, las) := d
    let toPrint? := inn_digs.filter fun (x, y) =>
      (x == fir.1 && fir.2 ≤ y && y ≤ las.2)
    if toPrint?.size != 0 then
      tot := tot + dig
  return tot

#eval show MetaM _ from do
  let answer := part1 ((← IO.FS.lines input).map String.toList).toList
  IO.println (f!"Day 3 part 1: {answer}")
  guard (answer == 531932)

def get_mul_pos (s : List (List Char)) : List (Int × Int) :=
  Id.run do
    let mut tot := #[]
    for i in [:s.length] do
      for j in [:s[0]!.length] do
        let c := s[i]![j]!
        if (c == '*') then
          tot := tot.push ((i, j) : Int × Int)
    return tot.toList

def get_num_nbs (rows : List (List Char)) (p : Int × Int) : Array Nat :=
  let digs := digs_ends rows
  let inn_digs := digs_in_nb rows [p]
  Id.run do
  let mut tot := #[]
  for d in digs do
    let (dig, fir, las) := d
    let toPrint? := inn_digs.filter fun (x, y) =>
      (x == fir.1 && fir.2 ≤ y && y ≤ las.2)
    if toPrint?.size != 0 then
      tot := tot.push dig
  return tot

def part2 (rows : List (List Char)) : Nat :=
  let mul_pos := get_mul_pos rows
  let gearRatios := mul_pos.map fun p =>
    let digs := get_num_nbs rows p
    match digs.toList with
      | [a, b] => a * b
      | _ => 0
  gearRatios.sum

--#assert part2 ((test.splitOn "\n").map String.toList) == 467835

#eval show MetaM _ from do
  let answer := part2 ((← IO.FS.lines input).map String.toList).toList
  IO.println (f!"Day 3 part 1: {answer}")
  guard (answer == 73646890)
