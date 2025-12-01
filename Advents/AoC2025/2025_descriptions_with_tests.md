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
