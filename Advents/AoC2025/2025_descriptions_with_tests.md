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

#  [Day 4](https://adventofcode.com/2025/day/4)

The input is a grid with the positions of rolls of paper.

####  Test

<pre>
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
</pre>

### Description

#### Part 1

We should find the number of rolls of papers that have fewer than `4` nearby rolls of paper.

#### Part 2

For the second part, we should recursively remove all rolls of paper that have fewer than `4` nearby
rolls of paper, until no more rolls can be removed.
We should report how rolls we removed in the process.

[Solution in Lean](day04.lean)

---

#  [Day 5](https://adventofcode.com/2025/day/5)

Finding fresh ingredients for the cafeteria

####  Test

<pre>
3-5
10-14
16-20
12-18

1
5
8
11
17
32
</pre>

### Description

#### Part 1

The input consists of ingredient ID ranges and individual IDs.

First, we should determine how many of the individual IDs are contained in at least one of the
ranges.

#### Part 2

Next, we should figure out how many distinct ingredient IDs are contained in the union of all the
IDs in all of the ranges.

[Solution in Lean](day05.lean)

---

#  [Day 6](https://adventofcode.com/2025/day/6)

Helping cephalopods with their maths homework

####  Test

<pre>
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
</pre>

### Description

#### Part 1

Read the input one way to add and multiply various numbers.

#### Part 2

Read the input in a different way to add and multiply various numbers.

[Solution in Lean](day06.lean)

---

#  [Day 7](https://adventofcode.com/2025/day/7)

Splitting tachyon manifolds

####  Test

<pre>
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
</pre>

### Description

#### Part 1

The input is a map with the positions of the splitters and the starting tachyon.
We should determine how many times the tachyon splits, while travelling in a
*classical* tachyon manifold.

#### Part 2

For part 2, we should determine how many times the tachyon splits, while travelling in a
*quantum* tachyon manifold.

[Solution in Lean](day07.lean)

---

#  [Day 8](https://adventofcode.com/2025/day/8)

Powering up Christmas decorations

####  Test

<pre>
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
</pre>

### Description

#### Part 1

The input is a list of positions in 3-space of junction boxes.
The first part, asks to connect the `1000` pairs positions that are closest to one another,
find the sizes of the `3` largest resulting connected components and report the product of these
sizes.

#### Part 2

Now, we should add all edges, from the shortest to the largest, until the graph has a single
connected component.
Once that happens, we should report the product of the `x`-coordinates of the last edge that was
added.

[Solution in Lean](day08.lean)

---

#  [Day 9](https://adventofcode.com/2025/day/9)

The input are the coordinates of the red tiles.

####  Test

<pre>
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
</pre>

### Description

#### Part 1

In part 1, we should determine the largest area of a rectangle with two opposite corners
on the red tiles.

#### Part 2

In part 2, we should still find the largest rectangle with opposite vertices on the red tiles,
but the rectangle must be entirely contained in the interior of the path.

[Solution in Lean](day09.lean)

---
