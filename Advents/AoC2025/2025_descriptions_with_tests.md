#  [Day 1](https://adventofcode.com/2025/day/1)

The input is a lists strings, starting with either `L` or `R` and continuing with a natural number.

####  Test

<pre>
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
</pre>

The first character represents whether we rotate left or right, the following natural number
represents by how much.

### Description

#### Part 1

Count how many times, following the rotations, we reach the position `0` at the end of some
rotation.

#### Part 2

Count how many times, following the rotations, we cross through the position `0`, possibly multiple
times with a single rotation.

[Solution in Lean](day01.lean)

---

#  [Day 2](https://adventofcode.com/2025/day/2)

The input is a sequence of ranges of IDs that are all natural numbers.

####  Test

<pre>
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
</pre>

### Description

#### Part 1

Find the IDs in the given ranges that consist of the concatenation of twice the same natural number,
when written to base `10` and report their sum.

For instance, `123123`, `1010` and `22` are examples of natural numbers that we are seeking.

#### Part 2

For the second part, we should find all numbers in the given ranges that are a concatenation of
*at least* two identical numbers and again report the sum of all such IDs.

[Solution in Lean](day02.lean)

---

#  [Day 3](https://adventofcode.com/2025/day/3)

The input is a list of sequences of joltages, each of which is a natural number from `1` to `9`.

####  Test

<pre>
987654321111111
811111111111119
234234234234278
818181911112111
</pre>

### Description

#### Part 1

Find the pairs of joltages in each sequence that are the digits of the largest natural number
that can be extracted in order.
Report the sum of such two-digit numbers.

For instance, from `321` extract `32` and from `123` extract `23`.
Their sum would be `55`.

#### Part 2

For the second part, we should do the same as in part 1, except that we want to sum the largest
12-digit numbers that can be extracted.

[Solution in Lean](day03.lean)

---
