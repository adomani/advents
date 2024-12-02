#  [Day 1](https://adventofcode.com/2021/day/1)

The input is a list of natural numbers, representing depths.

####  Test

<pre>
199
200
208
210
200
207
240
269
260
263
</pre>

### Description

#### Part 1

We should determine how often is one of the depths strictly smaller than the following depth.

#### Part 2

We should determine how often is one of the depths strictly smaller than the depth that is 3 entries later than itself.

[Solution in Lean](day01.lean)

---

#  [Day 2](https://adventofcode.com/2021/day/2)

A list of `forward`, `up` and `down`, each followed by a number.

####  Test

<pre>
forward 5
down 5
forward 8
up 3
down 8
forward 2
</pre>

### Description

#### Part 1

Each one of `forward`, `up` and `down` represents a movement of a vector with two integer coordinates.
The puzzle consists of finding where you end up, if you start at `(0, 0)`.

#### Part 2

Now, each one of `forward`, `up` and `down` represents a movement of a vector with *three* integer coordinates.
The puzzle again consists of finding where you end up, if you start at `(0, 0, 0)`.

[Solution in Lean](day02.lean)

---

#  [Day 3](https://adventofcode.com/2021/day/3)

The input is a list of binary digits, all having the same length (and possibly starting with `0` digits).

####  Test

<pre>
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
</pre>

### Description

#### Part 1

For each position, find the most common and the least common digit in the given input.
Multiply these numbers and convert to decimal

#### Part 2

For each position, find which digit is most common or least common and only keep those numbers that have that digit in that position.
When only one number remains, keep it.
As before, multiply the two numbers obtained by following the two strategies.

[Solution in Lean](day03.lean)

---

#  [Day 4](https://adventofcode.com/2021/day/4)

Playing Bingo against the squid!

####  Test

<pre>
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
</pre>

### Description

#### Part 1

The input consists of the numbers called and bingo cards.
First, we find the card that wins the earliest, and multiply the last value called (the one that made the card a winning card) by the sum of all the numbers of the card that were not called yet.

#### Part 2

For part 2, we apply the same procedure, but to the bingo card that wins last.

[Solution in Lean](day04.lean)

---

#  [Day 5](https://adventofcode.com/2021/day/5)

The inputs are lists of pairs of integer positions in the plane, representing the endpoints of line segments

####  Test

<pre>
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
</pre>

### Description

#### Part 1

Considering only the line segments that are parallel to a coordinate axis, compute how many overlaps there are between the line segments, discounting multiplicities.

#### Part 2

For part 2, the question is the same, except that we include all line segments, not just the ones that are parallel to the coordinate axes.
There is the extra implicit assumption that all line segments are either parallel to the coordinate axes or form a 45⁰ degree angle with the axes.

[Solution in Lean](day05.lean)

---

#  [Day 6](https://adventofcode.com/2021/day/6)

The input is a list of counters of lanternfish: they represent the days before they reproduce.

####  Test

<pre>
3,4,3,1,2
</pre>

### Description

#### Part 1

Using the reproduction rules for the lanternfish, figure out how many lanternfish there would be after 80 days.

#### Part 2

Using the same reproduction rules, figure out how many lanternfish there would be after 256 days.

[Solution in Lean](day06.lean)

---

#  [Day 7](https://adventofcode.com/2021/day/7)

The input is a list of positions of crab submarines.

####  Test

<pre>
16,1,2,0,4,2,7,1,2,14
</pre>

### Description

#### Part 1

The goal is to move each crab, so that they occupy all the same position.
The cost of moving a crab is its distance to the final position.
We need to determine the minimum cost.

#### Part 2

The goal is to move each crab, so that they occupy all the same position.
The cost of moving a crab is $\binom{d}{n}$, were $d$ is the distance to the final position.
We need to determine the minimum cost.

[Solution in Lean](day07.lean)

---
