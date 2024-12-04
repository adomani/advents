import Advents.Utils
open Lean

namespace Day16

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day16.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := ""

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/-- `test1` is a further test string for the problem. -/
def test1 := "8A004A801A8002F478"
/-- `test2` is a further test string for the problem. -/
def test2 := "620080001611562C8802118E34"
/-- `test3` is a further test string for the problem. -/
def test3 := "C0015000016115A2E0802F182340"
/-- `test4` is a further test string for the problem. -/
def test4 := "A0016C880162017C3686B18A3D4780"

def _root_.Char.toBin : Char → String
  | '0' => "0000"
  | '1' => "0001"
  | '2' => "0010"
  | '3' => "0011"
  | '4' => "0100"
  | '5' => "0101"
  | '6' => "0110"
  | '7' => "0111"
  | '8' => "1000"
  | '9' => "1001"
  | 'A' => "1010"
  | 'B' => "1011"
  | 'C' => "1100"
  | 'D' => "1101"
  | 'E' => "1110"
  | 'F' => "1111"
  | _ => ""

def hexToString (s : String) : String :=
  s.foldl (· ++ ·.toBin) ""

#guard hexToString "D2FE28" == "110100101111111000101000"
#guard hexToString "38006F45291200" == "00111000000000000110111101000101001010010001001000000000"
#guard hexToString "EE00D40C823060" == "11101110000000001101010000001100100000100011000001100000"

partial
def bitToNat (s : String) : Nat :=
  if s == "" then 0 else
  (if s.get 0 == '0' then 0 else 2 ^ (s.length - 1)) + bitToNat (s.drop 1)

/-- A packet -/
structure Packet where
  version : Nat
  ID : Nat
  lth : Option (Nat × Nat)
  lit : Option Nat := none
  ps : Array Packet
  deriving Inhabited, BEq

def typeID : Nat → String
  | 0 => "sum"
  | 1 => "product"
  | 2 => "minimum"
  | 3 => "maximum"
  | 4 => "literal"
  | 5 => "greater than"
  | 6 => "less than"
  | 7 => "equal to"
  | _ => "not a valid ID"

partial
def toString (p : Packet) : String :=
    let lID := match p.lth with
      | none => s!"literal: '{p.lit.getD 0}'"
      | some (0, lth) => s!"length ID: subpackets of total length {lth}"
      | some (_, lth) => s!"length ID: {lth} subpackets"
    s!"v:  {p.version}\nID: {p.ID} '{typeID p.ID}' \n{lID}\npackets: {p.ps.map toString}"

instance : ToString Packet where
  toString := toString

def _root_.String.read (s : String) (n : Nat) : Nat × String :=
  (bitToNat <| s.take n, s.drop n)

def decodeLiteral (s : String) : Nat × String := Id.run do
  let mut digits := ""
  let mut s := s
  while s.get 0 == '1' do
    digits := digits ++ (s.drop 1).take 4
    s := s.drop 5
  digits := digits ++ (s.drop 1).take 4
  return (bitToNat digits, s.drop 5)

def decodeOne (s : String) : Packet × String :=
  let (ver, s) := s.read 3
  let (ID, s) := s.read 3
  let (lth, lit, s) :=
    if ID == 4 then
      let (litVal, s) := decodeLiteral s
      (none, some litVal, s)
    else
      let (ltype, s) := s.read 1
      let tk := if ltype == 0 then 15 else 11
      let (len, s) := s.read tk
      (some (ltype, len), none, s)
  ({version := ver, ID := ID, lth := lth, lit := lit, ps := #[]}, s)

partial
def decodeNest (s : String) : Packet :=
  if s.all (· == '0') then default
  else
  let (p, s) := decodeOne s
  match p.lth with
    | none =>
      dbg_trace "nesting in literal: {s}"
      {p with ps := #[decodeNest s]}
    | some (0, l) =>
      let nest := decodeNest (s.take l)
      let cand := {p with ps := #[nest]}
      let next := decodeNest (s.drop l)
      if next == default then cand else {cand with ps := cand.ps.push next}
    | some (_, l) => Id.run do
      let mut (q, t) := (#[], s)
      for _ in [0:l] do
        let (np, ns) := decodeOne s
        t := ns
        q := q.push np
      let cand := {p with ps := q}
      let next := decodeNest t
      if next == default then return cand else return {cand with ps := cand.ps.push next}

partial
def decodeAll (s : String) : Array Packet :=
  if s.all (· == '0') then #[]
  else
    let (p, s) := decodeOne s
    #[p] ++ decodeAll s

def printDecs (s : String) (nest? : Bool := true) : IO Unit := do
  let ds := if nest? then #[decodeNest s] else decodeAll s
  let mut vers := 0
  for d in ds do
    vers := vers + d.version
    IO.println s!"\n{d}"
  IO.println s!"\n---\n\nTotal version: {vers}"



#eval do
  let inp := "00111000000000000110111101000101001010010001001000000000"
  let inp := "11101110000000001101010000001100100000100011000001100000"
  let inp := hexToString "8A004A801A8002F478"
  let inp := hexToString "620080001611562C8802118E34"
  let inp := hexToString "A0016C880162017C3686B18A3D4780"
  let inp := hexToString "C0015000016115A2E0802F182340"
  let inp := hexToString (← IO.FS.readFile input)
  --let dn := decodeNest inp
  --let dn := dn[1]!
  --IO.println s!"dn: {dn}"
  printDecs inp
  printDecs inp false

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let ds := decodeAll <| hexToString dat
  let mut vers := 0
  for d in ds do
    vers := vers + d.version
  vers

#assert part1 test1 == 16
#assert part1 test2 == 12
#assert part1 test3 == 23
#assert part1 test4 == 31

solve 1 891 file

/-!
#  Question 2
-/

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := sorry
--def part2 (dat : String) : Nat :=

--#assert part2 atest == ???

--solve 2

end Day16
