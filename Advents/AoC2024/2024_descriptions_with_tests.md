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
