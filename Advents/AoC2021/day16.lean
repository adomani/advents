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
  src : String
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
    s!"v:  {p.version}\nID: {p.ID} '{typeID p.ID}' \n{lID}\npackets: {p.ps.map toString}\nsrc: {p.src.length} '{p.src}'"

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
  s := s.drop 5
  --dbg_trace "lit left: '{s.drop 5}'"
  return (bitToNat digits, s) --if s.all (· == '0') then "" else s)

def decodeOne (s : String) : Packet × String :=
  let s' := s

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
  ({version := ver, ID := ID, lth := lth, lit := lit, ps := #[], src := s'.dropRight s.length}, s)

partial
def decodeNest (s : String) : Packet :=
  --dbg_trace "Out: '{s}' -->\n{decodeOne s}"
  if s.all (· == '0') then
    --dbg_trace "Dis: '{s}'"
    default
  else
  --dbg_trace "Pro: '{s}' -->\n{decodeOne s}\n"
  let (p, s) := decodeOne s
  match p.lth with
    | none =>
      --dbg_trace "nesting in literal: {s}"
      {p with ps := #[decodeNest s]}
    | some (0, l) =>
      --Id.run do
      --let tgt := s.length - l
      --let mut sleft := s
      --let mut acc : Array Packet := #[]
      --while sleft.length != tgt do
      --  let (newP, sl) := decodeNest sleft
      --return default

      let nest := decodeNest (s.take l)
      let (p1, s1) := decodeOne (s.take l)
      dbg_trace "{s.take l}\n\n{decodeOne (s.take l)} "
      dbg_trace "there {l}\n\n{nest.ps.size}\nnest[0]:\n{nest.ps[0]!}\n\nnest[1]:\n{nest.ps[1]!}\n\nnest[2]:\n{nest.ps[2]!}\n---"
      let cand := {p with ps := #[nest]}
      dbg_trace "cand: {cand}\n\n"
      let next := decodeNest (s.drop l)
      if next == default then cand else {cand with ps := cand.ps.push next}
    | some (_, l) => Id.run do
      dbg_trace "here {l}"
      let mut (q, t) := (#[], s)
      for _ in [0:l] do
        let (np, ns) := decodeOne t
        t := ns
        q := q.push np
      let cand := {p with ps := q}
      let next := decodeNest t
      if next == default then return cand else return {cand with ps := cand.ps.push next}



partial
def decodeNestArr (s : String) : Array Packet :=
  --dbg_trace "Out: '{s}' -->\n{decodeOne s}"
  if s.all (· == '0') then
    --dbg_trace "Dis: '{s}'"
    default
  else
  --dbg_trace "Pro: '{s}' -->\n{decodeOne s}\n"
  let (p, s) := decodeOne s
  match p.lth with
    | none =>
      --dbg_trace "nesting in literal: {s}"
      #[p] -- with ps := #[decodeNestArr s]}]
    | some (0, l) =>
      --Id.run do
      --let tgt := s.length - l
      --let mut sleft := s
      --let mut acc : Array Packet := #[]
      --while sleft.length != tgt do
      --  let (newP, sl) := decodeNest sleft
      --return default

      let nest := decodeNest (s.take l)
      --let nestRight := decodeNest (s.drop l)
      let next := decodeNest (s.drop l)
      --let (p1, s1) := decodeOne (s.take l)
      --dbg_trace "{s.take l}\n\n{decodeOne (s.take l)} "
      --dbg_trace "there {l}\n\n{nest.ps.size}\nnest[0]:\n{nest.ps[0]!}\n\nnest[1]:\n{nest.ps[1]!}\n\nnest[2]:\n{nest.ps[2]!}\n---"
      #[{p with ps := #[nest]}]--, next]
      --let cand := {p with ps := nest}
      --dbg_trace "cand: {cand}\n\n"
      --if next == default then cand else #[{cand with ps := cand.ps.push next}]
    | some (_, l) =>
      let fin := decodeNestArr s
      #[{p with ps := fin.take l}] ++ fin.toList.drop l
      --default
      --Id.run do
      --dbg_trace "here {l}"
      --let mut (q, t) := (#[], s)
      --for _ in [0:l] do
      --  let (np, ns) := decodeOne t
      --  t := ns
      --  q := q.push np
      --let cand := {p with ps := q}
      --let next := decodeNest t
      --if next == default then return #[cand] else return #[{cand with ps := cand.ps.push next}]






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

partial
def toOps (p : Packet) (indent : String) : String :=
  if p.ID == 4 then indent ++ s!"{p.lit.get!} ({p.ps.map (toOps · (indent ++ "  "))})"
  else
    indent ++ typeID p.ID ++ s!"({p.ps.map (toOps · (indent ++ "  "))})"

partial
def toNat (p : Packet) : Nat :=
  if (p.ps.filter (· != default)).isEmpty then p.lit.getD 0 else
  let rest := (p.ps.map toNat)
  dbg_trace "rest: {rest}"
  match p.ID with
  | 0 =>
    dbg_trace "+ {rest}"
    rest.sum
  | 1 =>
    dbg_trace "* {rest}"
    rest.prod
  | 2 =>
    dbg_trace "⊓ {rest}"
    rest.foldl min rest[0]!
  | 3 =>
    dbg_trace "⊔ {rest}"
    rest.foldl max rest[0]!
  | 4 =>
    dbg_trace "λ {rest}"
    p.lit.get!
  | 5 =>
    dbg_trace "> {rest}"
    if rest[0]! > rest[1]! then 1 else 0
  | 6 =>
    dbg_trace "< {rest}"
    if rest.getD 0 1000 < rest.getD 1 1000 then 1 else 0
  | 7 =>
    dbg_trace "= {rest}"
    if rest[0]! = rest.getD 1 10000 then 1 else 0
  | _ =>
    dbg_trace "∃ {rest}"
    10 ^ 9

-- 15843889 -- too low

#eval do
  let _inp := "00111000000000000110111101000101001010010001001000000000"
  let _inp := "11101110000000001101010000001100100000100011000001100000"
  let _inp := hexToString "8A004A801A8002F478"
  let _inp := hexToString "620080001611562C8802118E34"
  let _inp := hexToString "A0016C880162017C3686B18A3D4780"
  let _inp := hexToString "C0015000016115A2E0802F182340"

  let inp := hexToString (← IO.FS.readFile input)

  let inp := hexToString "C200B40A82"
  let inp := hexToString "04005AC33890"
  let inp := hexToString "880086C3E88112"
  let inp := hexToString "CE00C43D881120"
  let inp := hexToString "D8005AC2A8F0"
  let inp := hexToString "9C0141080250320F1802104A08"
  IO.println s!"MANY SUMS: {((decodeNestArr inp).map toNat)}"
  IO.println (toNat (decodeNest inp))
  IO.println (toOps (decodeNest inp) "\n")

#exit
  --let dn := decodeNest inp
  --let dn := dn[1]!
  --IO.println s!"dn: {dn}"
  printDecs inp
  printDecs inp false

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : String) : Nat :=
  let ds := decodeAll <| hexToString dat
  ds.foldl (init := 0) (· + ·.version)

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
