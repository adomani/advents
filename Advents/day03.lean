import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/day03.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

/-- `test` is the test string for the problem. -/
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

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `nbs` is the list of neighbours of `(0, 0)`, horizontally, vertically and diagonally. -/
def nbs := Id.run do
  let mut t := #[]
  for i in [-1, 0, 1] do
    for j in [-1, 0, 1] do
      t := t.push (i,j)
  return t.erase (0,0)

#assert nbs == #[(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

/-- `has_number_nb` takes as input a "plane" `d` of `Char`acters, the number of columns and rows `lx, ly`
of the plane, a position `p` in the plane and it returns the array of coordinates of locations
neighbouring `p` where `d` contains a digit. -/
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

/-- `symb_pos d` takes as input a "plane" `d` of `Char`acters and returns the array of locations of
all the symbols in `d`. -/
def symb_pos (d : Array (Array Char)) : Array (Int × Int) :=
  Id.run do
    let mut tot := #[]
    for i in [:d.size] do
      for j in [:d[0]!.size] do
        let c := d[i]![j]!
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

/-- `part1 rows` takes as input the input of the problem and returns the solution to part 1. -/
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

#assert part1 atest == 4361

solve 1 531932

/-!
#  Question 2
-/

/-- `get_mul_pos d` takes as input a "plane" `d` of `Char`acters and returns the array of locations of
all the multiplication symbols `*` in `d`. -/
def get_mul_pos (d : Array (Array Char)) : Array (Int × Int) :=
  Id.run do
    let mut tot := #[]
    for i in [:d.size] do
      for j in [:d[0]!.size] do
        let c := d[i]![j]!
        if (c == '*') then
          tot := tot.push ((i, j) : Int × Int)
    return tot

/-- `get_num_nbs rows p` takes as input a "plane" `rows` of `Char`acters and a position `p`.
It returns the array of natural numbers that are neighbours to the position `p`. -/
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

/-- `part2 rows` takes as input the input of the problem and returns the solution to part 2. -/
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

#assert part2 atest == 467835

solve 2 73646890
