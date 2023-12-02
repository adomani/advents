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

section meta
open Lean Elab
/-- `#assert x` takes a `Bool`ean `x` and fails if `x` is `false`.
It runs `#eval show MetaM _ from do guard x`-/
macro (name := cmdAssert) "#assert" cmd:term : command =>
  `(command| #eval show MetaM _ from do guard $cmd)

end meta
