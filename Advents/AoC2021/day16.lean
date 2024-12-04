import Advents.Utils
open Lean

namespace Day16

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2021/day16.input"

/-!
#  Question 1
-/

--#eval do IO.println (← IO.FS.readFile input)

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
  --ps : Array Packet
  deriving Inhabited

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
  ({version := ver, ID := ID, lth := lth, lit := lit}, s)

partial
def decodeMany (s : String) : Array Packet :=
  if s == "" then #[] else
  let (p, s) := decodeOne s
  #[p] ++ decodeMany s


instance : ToString Packet where
  toString p :=
    let lID := match p.lth with
      | none => s!"literal: '{p.lit.getD 0}'"
      | some (0, lth) => s!"length ID: subpackets of total length {lth}"
      | some (_, lth) => s!"length ID: {lth} subpackets"
    s!"v:  {p.version}\nID: {p.ID}\n{lID}" --\npackets: {p.ps.size}"

#eval do
  let ds := decodeMany "00111000000000000110111101000101001010010001001000000000"
  let ds := decodeMany "11101110000000001101010000001100100000100011000001100000"
  let ds := decodeMany <| hexToString "8A004A801A8002F478"
  let ds := decodeMany <| hexToString "620080001611562C8802118E34"
  let ds := decodeMany <| hexToString "C0015000016115A2E0802F182340"
  let ds := decodeMany <| hexToString "A0016C880162017C3686B18A3D4780"
  let ds := decodeMany <| hexToString (← IO.FS.readFile input)
  let mut vers := 0
  for d in ds do
    vers := vers + d.version
    IO.println s!"\n{d}"
  IO.println s!"\n---\n\nTotal version: {vers}"



#eval do
  let ss := #["110100101111111000101000", "00111000000000000110111101000101001010010001001000000000", "1101000101001010010001001000000000", "01010010001001000000000"]
  let ssl := #[hexToString "8A004A801A8002F478", "101010000000000000101111010001111000"]
  for s in ss do
    let (p, s1) := decodeOne s
    dbg_trace "\n{p}"
    if p.ID == 4 then
      dbg_trace "Decoding {s1} as a literal: {decodeLiteral s1}"
    else
      dbg_trace "Not a literal: sub-decoding {s1}"
      dbg_trace decodeOne s1

#eval do
  let ss := #[hexToString "620080001611562C8802118E34"
  --, "101010000000000000101111010001111000"
  ]
  for s in ss do
    let (p, s1) := decodeOne s
    dbg_trace "\n{p}"
    if p.ID == 4 then
      dbg_trace "Decoding {s1} as a literal: {decodeLiteral s1}"
    else
      dbg_trace "Not a literal: sub-decoding {s1}"
      dbg_trace decodeOne s1


#eval do
  let s := hexToString (← IO.FS.readFile input)
  let (p, s1) := decodeOne s
  dbg_trace "\n{p}\n"
  if p.ID == 4 then
    dbg_trace "Decoding {s1} as a literal: {decodeLiteral s1}"
  else
    dbg_trace "Not a literal: sub-decoding {s1}\n"
    let (p, s1) := decodeOne s1
    dbg_trace "\n{p}"
    dbg_trace "\nleftover: {s1}\n"


--def toBin (s : String) : Nat :=
--  match s.toList with

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat := Id.run do
  let ds := decodeMany <| hexToString dat
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
