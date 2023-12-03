import Advents.Utils
open Lean

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/i03.txt"

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
#check Int.natAbs

def has_number_nb (d : List (List Char)) (lx ly : Int) (p : Int × Int) : Array (Nat × Nat) :=
  Id.run do
    let mut cond := #[]
    for o in nbs do
--      dbg_trace p + o
      let (x, y) := p + o
      if (! x < 0) && (! y < 0) && (x < lx) && (y < ly) then
        let (x, y) := (x.natAbs, y.natAbs)
        let nc := d[x]![y]!.isDigit
        if nc then
          cond := cond.push (x, y)
--          dbg_trace (x, y)
--        cond := cond ∨ nc
--      cond := cond
    return cond
  --true

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
--      dbg_trace dig_nbs
      digs := digs ++ dig_nbs
      --if () then
      --  digs.push
    return digs
--  dbg_trace has_number_nb rows rows.length rows[0]!.length (1, 1)
  tots

--def digs_ends (rows : List (List Char)) : List ((Nat × Nat) × (Nat × Nat)) :=

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
--          tot := tot.push (i, j)
        if inside && (!ch.isDigit || j == ry - 1) then
          inside := false
--          dbg_trace val
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

#eval do
--  dbg_trace test
  let rows := ((← IO.FS.lines input).map String.toList).toList
  IO.println (f!"Day 3 part 1: {part1 rows}")

#assert part1 ((← IO.FS.lines input).map String.toList).toList == 531932

#exit
--  let rows := (test.splitOn "\n").map String.toList
--  IO.println <| ← IO.FS.readFile input
  let digs := digs_ends rows
  let inn_digs := digs_in_nb rows (symb_pos rows)
  IO.println f!"digs: {digs.size}"
--  IO.println digs
  IO.print f!"inn_digs: {inn_digs.size}\n\nDay 3 part 1: "
--  IO.println inn_digs
  let mut tot := 0
  for d in digs do
    let (dig, fir, las) := d
  --  dbg_trace inn_digs.filter (Prod.fst · == fir.1)
    let toPrint? := inn_digs.filter fun x : Nat × Nat => (x.1 == fir.1 && fir.2 ≤ x.2 && x.2 ≤ las.2)
  --  dbg_trace toPrint?
    if toPrint?.size != 0 then
--      dbg_trace dig
      tot := tot + dig
  --  dbg_trace "\n"
--    if let some (x, y) := inn_digs.find? (Prod.fst · == fir.1) then
--      dbg_trace (x, y)
  return tot



#exit
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
--          tot := tot.push (i, j)
        if inside && (!ch.isDigit || j == ry - 1) then
          inside := false
          dbg_trace val
          tot := tot.push (val.toNat!, temp, (i, j))
          val := ""
    return tot
--        let (jx, jy) := j
--        if !((jx, jy+1) ∈ i)
--  default


/-
def trim_consec_pos (pos : List (List (Nat × Nat))) : List (List (Nat × Nat)) :=
  Id.run do
    let mut tot := #[]
    for i in pos do
      for j in i do
        let (jx, jy) := j
        if !((jx, jy+1) ∈ i)
  default
-/

#eval do
  let rows := (test.splitOn "\n").map String.toList
  let rows := ((← IO.FS.lines input).map String.toList).toList
  dbg_trace (⟨rows[0]!⟩ : String)
  dbg_trace (⟨rows[1]!⟩ : String)
  let digs := "0123456789"
  dbg_trace (digs ++ digs ++ digs ++ "\n")
  dbg_trace digs_in_nb rows (symb_pos rows)
  return 0
#exit
  let rx := rows.length
  let ry := rows[0]!.length
  dbg_trace test
  let symbols := symb_pos rows
--  dbg_trace f!"symb: {symbols}"
  let tots := Id.run do
    let mut digs := #[]
    for s in symbols do
      let dig_nbs := has_number_nb rows rx ry s
--      dbg_trace dig_nbs
      digs := digs ++ dig_nbs
      --if () then
      --  digs.push
    return digs
--  dbg_trace has_number_nb rows rows.length rows[0]!.length (1, 1)
  tots

#eval
  let rows := (test.splitOn "\n").map String.toList
  let rx := rows.length
  let ry := rows[0]!.length
  dbg_trace test
  let symbols := symb_pos rows
--  dbg_trace f!"symb: {symbols}"
  let tots := Id.run do
    let mut digs := #[]
    for s in symbols do
      let dig_nbs := has_number_nb rows rx ry s
--      dbg_trace dig_nbs
      digs := digs ++ dig_nbs
      --if () then
      --  digs.push
    return digs
--  dbg_trace has_number_nb rows rows.length rows[0]!.length (1, 1)
  tots
