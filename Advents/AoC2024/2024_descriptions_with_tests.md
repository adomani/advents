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
