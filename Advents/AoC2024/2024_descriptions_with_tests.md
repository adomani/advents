#  [Day 1](https://adventofcode.com/2024/day/1)

The input is a pair of lists of natural numbers out of which a location should be produced.

####  Test

<pre>
3   4
4   3
2   5
1   3
3   9
3   3
</pre>

### Description

#### Part 1

Sort each of the two lists and compute the distances of the corresponding entries in the
sorted lists

#### Part 2

For each entry `l` of the first list, find how many entries of the second list are equal to
`l` and multiply `l` by this number.
Then, add everything up.

[Solution in Lean](day01.lean)

---

#  [Day 2](https://adventofcode.com/2024/day/2)

The input is a list of lists of levels -- each level is a natural number.

####  Test

<pre>
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
</pre>

### Description

#### Part 1

Find all the rows of the input whose entries are either strictly increasing or strictly decreasing and that never
do they increase or decrease by more than 3.

#### Part 2

Same as part 1, except that you are allowed to drop one level from each entry before checking the condition.

[Solution in Lean](day02.lean)

---

#  [Day 3](https://adventofcode.com/2024/day/3)

Parsing a string with errors.

####  Test

<pre>
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
</pre>

####  Test 2

<pre>
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
</pre>

### Description

#### Part 1

Scan the input string for substrings of the form `mul(d1,d2)`, where `d1` and `d2` are only allowed to be `1`, `2` or `3` digit numbers.
Accumulate the products of such numbers, ignoring every malformed substring.

#### Part 2

Same as part 1, except that now we should be aware of the fact that interspered in the string there are also some `do()` and `don't()` substring.
We should only count a multiplication if the closest `do()`/`don't()` substring is a `do()`, ignoring the other multiplications.
If you parse the input by lines, remember to persist the `do()`/`don't()` information across lines!

[Solution in Lean](day03.lean)

---

#  [Day 4](https://adventofcode.com/2024/day/4)

Look for the word `XMAS` and `X-MAS` in a grid of letters.

####  Test

<pre>
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
</pre>

### Description

#### Part 1

For part 1, we scan for the word `XMAS` starting anywhere on the grid and continuing in each of the 8 directions:
horizontal, vertical or diagonal and left/right or up/down.

#### Part 2

For part 2, we should find instead an `X` of `MAS`.
This means that we are looking for a rotation of the pattern
```
M M
 A
S S
```
anywhere on the grid.

[Solution in Lean](day04.lean)

---

#  [Day 5](https://adventofcode.com/2024/day/5)

Checking whether some pages are correctly sorted.

####  Test

<pre>
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
</pre>

### Description

The input begins with a list of pairs of natural numbers, in the form `n1|n2`, representing the correct ordering that the pages should have.
It then continues with a list of actual orders of pages, in the form `p₁,p₂,...,pₙ`.

#### Part 1

Add the number of pages that appear in the middle of each *correctly ordered* list of pages.

#### Part 2

Add the number of pages that appear in the middle of each *incorrectly ordered* list of pages.

[Solution in Lean](day05.lean)

---

#  [Day 6](https://adventofcode.com/2024/day/6)

The input is a map with locations of obstacles and the location of a guard.

####  Test

<pre>
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
</pre>

### Description

#### Part 1

The guard moves in a fixed direction, until it reaches an obstacle.
At that point, it rotates by 90⁰ clockwise and continues.
The first question is to determine how many locations on the map the guard occupies, before exiting the map.

#### Part 2

For the second part, we should find all positions on the grid such that placing one further an obstacle in that position, makes the guard enter into a loop.

[Solution in Lean](day06.lean)

---
