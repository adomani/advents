import Advents.Utils

namespace Day03

/-- `input` is the location of the file with the data for the problem. -/
def input : System.FilePath := "Advents/AoC2024/day03.input"

/-!
#  Question 1
-/

/-- `test` is the test string for the problem. -/
def test := "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

/-- `atest` is the test string for the problem, split into rows. -/
def atest := (test.splitOn "\n").toArray

/--
Checks whether the input string `s` starts with `mul(d1,d2)`,
where `d1` and `d2` are only allowed to consist of `1`, `2` or `3` digits.

If that is the case, then return `s` with the leading `mul(d1,d2)` removed and
`d1 * d2` -- the result of multiplying the natural numbers `d1` and `d2`.
Otherwise, return `(s', none)`, where `s'` is at least one character shorter than `s`.
-/
def startsWithMul (s : String) : String × (Option Nat) :=
  if ! s.startsWith "mul(" then (s.drop 1, none)
  else
    let s := s.drop "mul(".length
    let d1 := s.takeWhile (·.isDigit)
    if !#[1, 2, 3].contains d1.length then  (s, none)
    else
      let s := s.drop d1.length
      let d1 := d1.toNat!
      if s.take 1 != "," then  (s, none)
      else
        let s := s.drop 1
        let d2 := s.takeWhile (·.isDigit)
        if !#[1, 2, 3].contains d2.length then  (s, none)
        else
          let s := s.drop d2.length
          let d2 := d2.toNat!
          if s.take 1 != ")" then  (s, none)
          else
            let s := s.drop 1
            (s, d1 * d2)

/--
`consume s` repeatedly applies `startsWithMul` on `s`, accumulating the resulting natural numbers
until `s` becomes empty.
-/
partial
def consume (s : String) : Array Nat :=
  if s == "" then #[]
  else
    let (s, new) := startsWithMul s
    let fin := consume s
    if let some n := new then (fin.push n).reverse else fin.reverse

/-- `part1 dat` takes as input the input of the problem and returns the solution to part 1. -/
def part1 (dat : Array String) : Nat :=
  let vals := dat.map consume
  vals.flatten.sum

#assert part1 atest == 161

solve 1 185797128

/-!
#  Question 2
-/

/-- `MulState` is the state for the second part of the puzzle.
* `s` is the still-to-be-parsed string;
* `muls` is the array of accumulated multiplications;
* `do?` is the currently active `do()`/`don't()` command, `true` for `do()` and `false` for `don't()`.
-/
structure MulState where
  /-- `s` is the still-to-be-parsed string. -/
  s : String
  /-- `muls` is the array of accumulated multiplications. -/
  muls : Array Nat := #[]
  /-- `do?` is the currently active `do()`/`don't()` command, `true` for `do()` and `false` for `don't()`. -/
  do? : Bool := true
  deriving Inhabited

/--
Checks whether the input string `s` starts with `do()` or `don't()`.
If that is the case, then return `s` with the leading `do()`/`don't()` removed and
the appropriate `true`/`false`.
Otherwise, return `(s.drop 1, none)`.
-/
def startsWithDo (s : String) : String × (Option Bool) :=
  if s.startsWith "do()" then (s.drop "do()".length, true)
  else
    if s.startsWith "don't()" then (s.drop "don't()".length, false)
    else (s.drop 1, none)

/-- `updateMul m` updates the state `m` by performing one check:
* if the string `m.s` starts with `d`, then use `startsWithDo`,
* if the string `m.s` starts with `m`, then use `startsWithMul`,
* otherwise, drop the first character from `s`.
-/
def updateMul (m : MulState) : MulState :=
  if m.s == "" then m
  else
    match m.s.take 1 with
      | "d" =>
        let (s, b) := startsWithDo m.s
        {m with s := s, do? := b.getD m.do?}
      | "m" =>
        let (s, new) := startsWithMul m.s
        if let some n := new then
          let newMuls := if m.do? then m.muls.push n else m.muls
          {m with s := s, muls := newMuls} else {m with s := s}
      | _ => {m with s := m.s.drop 1}

/--
`consumeAll m` repeatedly applies `updateMul` on `m`, until the string `m.s` becomes empty.
Eventually, it reports the array of accumulated products and the value of the `do?` state.
-/
partial
def consumeAll (m : MulState) : Array Nat × Bool :=
  if m.s == "" then (m.muls.reverse, m.do?)
  else
    consumeAll (updateMul m)

/-- `test2` is the test string for the problem. -/
def test2 := "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

/-- `atest2` is the test string for the problem, split into rows. -/
def atest2 := (test2.splitOn "\n").toArray

/-- `part2 dat` takes as input the input of the problem and returns the solution to part 2. -/
def part2 (dat : Array String) : Nat := Id.run do
  let mut do? := true
  let mut mulMuls := #[]
  for d in dat do
    let (muls, newDo?) := consumeAll {s := d, do? := do?}
    mulMuls := mulMuls.push muls
    do? := newDo?
  mulMuls.flatten.sum

#assert part2 atest2 == 48

solve 2 89798695

end Day03
