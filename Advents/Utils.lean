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
