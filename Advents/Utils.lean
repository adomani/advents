import Std

section sums
variable {α} [Inhabited α]

/--  Sum the elements of a `List`. -/
def List.sum [Add α] : List α → α
  | []    => default
  | [m]   => m
  | m::ms => m + ms.sum

/--  Sum the elements of an `Array`. -/
def Array.sum [Add α] (l : Array α) : α :=
  l.toList.sum

/--  Multiply the elements of a `List`. -/
def List.prod [Mul α] : List α → α
  | []    => default
  | [m]   => m
  | m::ms => m * ms.prod

/--  Multiply the elements of an `Array`. -/
def Array.prod [Mul α] (l : Array α) : α :=
  l.toList.prod

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
open Lean Elab Command

/-- `#assert x` takes a `Bool`ean `x` and fails if `x` is `false`.
It runs `#eval show MetaM _ from do guard x`-/
macro (name := cmdAssert) "#assert" cmd:term : command =>
  `(command| run_cmd Elab.Command.liftTermElabM do guard $cmd)

/-- `solve pt answer` runs function `part1` if `pt = 1` and function `part2` if `pt = 2`
on declaration `input`, expecting that it evaluates to `answer`.
If it does, then it prints a summary, otherwise it fails.

The variant `solve pt answer file` assumes that the code should be run on the whole string input,
rather than on its lines.

Finally, the `answer` argument is optional: if it is not provided, `solve` will not guard
for the computed value.

Example usage:
```lean
solve 1 15    -- parses the input as an array of strings, errors if answer does not match `15`
solve 2 629   -- parses the input as an array of strings, errors if answer does not match `629`

solve 1 15  file  -- parses the input as a string, errors if answer does not match `15`
solve 2 file      -- parses the input as a string, no error
```
-/
elab "solve" part:num n:(num)? f:("file")?: command => do
  let nn ← match n with
    | some stx =>  `((some $stx))
    | none =>  `((none))
  let p1 := mkIdent <| match part with
    | `(1) => `part1
    | `(2) => `part2
    | _ => default
  let inp := mkIdent `input
  let rf := mkIdent <| if f.isSome then `IO.FS.readFile else `IO.FS.lines
  elabCommand (← `(command|
    #eval show MetaM _ from do
      let day := ((System.FilePath.toString $inp).toList.getNumbers)[0]!
      let answer := $p1 <| ← $rf $inp
      IO.println <| f!"Day {day}, part {$part}: {answer}"
      let ans := ($nn).getD answer
      guard (answer == ans) <|> throwError "Computed {answer}\nExpected {ans}"))

end meta
