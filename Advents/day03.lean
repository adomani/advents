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
.664.598.."

instance : Add (Int × Int) where add x y := (x.1 + y.1, x.2 + y.2)

def nbs := Id.run do
  let mut t := #[]
  for i in [-1, 0, 1] do
    for j in [-1, 0, 1] do
      t := t.push (i,j)
  return t.erase (0,0)

--#assert nbs == #[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

def has_number_nb (d : Array (Array Char)) (lx ly : Int) (p : Int × Int) : Array (Nat × Nat) :=
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

def symb_pos (s : Array (Array Char)) : Array (Int × Int) :=
  Id.run do
    let mut tot := #[]
    for i in [:s.size] do
      for j in [:s[0]!.size] do
        let c := s[i]![j]!
        if (! c.isDigit && ! c == '.') then
          tot := tot.push ((i, j) : Int × Int)
    return tot

/-- get the coordinates of digits that are neighbours of symbols. -/
def digs_in_nb (rows : Array (Array Char)) (symbs : Array (Int × Int)) : Array (Nat × Nat) :=
  let rx := rows.size
  let ry := rows[0]!.size
  let tots := Id.run do
    let mut digs := #[]
    for s in symbs do
      let dig_nbs := has_number_nb rows rx ry s
      digs := digs ++ dig_nbs
    return digs
  tots

/-- returns the natural number and the endpoints of the consecutive digits
appearing in rows. -/
def digs_ends (rows : Array (Array Char)) : Array (Nat × (Nat × Nat) × (Nat × Nat)) :=
  let rx := rows.size
  let ry := rows[0]!.size
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

def part1 (rows : Array String) : Nat :=
  let rows := rows.map (List.toArray ∘ String.toList)
  let digs := digs_ends <| rows
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

solve 1 531932

/-!
#  Question 2
-/

def get_mul_pos (s : Array (Array Char)) : Array (Int × Int) :=
  Id.run do
    let mut tot := #[]
    for i in [:s.size] do
      for j in [:s[0]!.size] do
        let c := s[i]![j]!
        if (c == '*') then
          tot := tot.push ((i, j) : Int × Int)
    return tot

def get_num_nbs (rows : Array (Array Char)) (p : Int × Int) : Array Nat :=
  let digs := digs_ends rows
  let inn_digs := digs_in_nb rows #[p]
  Id.run do
  let mut tot := #[]
  for d in digs do
    let (dig, fir, las) := d
    let toPrint? := inn_digs.filter fun (x, y) =>
      (x == fir.1 && fir.2 ≤ y && y ≤ las.2)
    if toPrint?.size != 0 then
      tot := tot.push dig
  return tot

def part2 (rows : Array String) : Nat :=
  let rows := rows.map (List.toArray ∘ String.toList)
  let mul_pos := get_mul_pos rows
  let gearRatios := mul_pos.map fun p =>
    -- `nr` are the rows adjacent to the `*` with position `p`.  I checked that the first and last row
    -- contain no `*`, so there is no need to remove the last or the first extracted row.
    let nr := #[rows[p.1.natAbs-1]!, rows[p.1.natAbs]!, rows[p.1.natAbs+1]!]
    let digs := get_num_nbs nr (1, p.2)
    match digs.toList with
      | [a, b] => a * b
      | _ => 0
  gearRatios.sum

--#assert part2 ((test.splitOn "\n").toArray) == 467835

solve 2 73646890
