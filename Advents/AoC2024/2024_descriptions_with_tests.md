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
