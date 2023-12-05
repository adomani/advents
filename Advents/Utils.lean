import Std

section sums
variable {α} [Add α] [Inhabited α]

/--  Sum the elements of a `List`. -/
def List.sum : List α → α
  | []    => default
  | [m]   => m
  | m::ms => m + ms.sum

/--  Sum the elements of an `Array`. -/
def Array.sum (l : Array α) : α :=
  l.toList.sum

end sums

/-- `List.getNumbers l` takes as input a list of characters and returns the list of
`Nat` where each entry is the natural number corresponding to each consecutive
sequence of digits in `l`, in their order. -/
partial
def List.getNumbers (l : List Char) : List Nat :=
  let l1 := l.dropWhile (!Char.isDigit ·)
  if l1.length == 0 then [] else
    let d1 := String.toNat! ⟨l1.takeWhile (Char.isDigit ·)⟩
    let fin := getNumbers (l1.dropWhile (Char.isDigit ·))
  d1 :: fin


section meta
open Lean Elab
/-- `#assert x` takes a `Bool`ean `x` and fails if `x` is `false`.
It runs `#eval show MetaM _ from do guard x`-/
macro (name := cmdAssert) "#assert" cmd:term : command =>
  `(command| #eval show MetaM _ from do guard $cmd)

end meta
