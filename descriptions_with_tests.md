#  Day 1

Finding the first and last digit (or the English name of a digit) in each row of the input.

####  Test 1

<pre>
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
</pre>

####  Test 2

<pre>
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
</pre>

### Description

#### Part 1

I simply scanned the characters in each line,
stopping at the first digit.

Next, I recycled the function, scanning the reversed list of characters.

#### Part 2

Very similar to part 1:
scan the characters as before, but also try
to match English spelling of digits,
before moving on to the next character.

I also recycled the previous function for the reverse scanning.

[Solution in Lean](Advents/day01.lean)

---

#  Day 2

RGB-coloured cubes in a bag, finding sups and products.

####  Test

<pre>
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
</pre>

[Solution in Lean](Advents/day02.lean)

---

#  Day 3

Operations and numbers scattered on a rectangular grid: compute a value from the natural numbers adjacent to certain symbols.

####  Test

<pre>
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
</pre>

[Solution in Lean](Advents/day03.lean)

---

#  Day 4

A game with cards: computing the number of matches between two lists of natural numbers, some recursion.

####  Test

<pre>
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
</pre>

[Solution in Lean](Advents/day04.lean)

---

#  Day 5

Seeds growing into locations as a series of permutations.

####  Test

<pre>
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
</pre>

[Solution in Lean](Advents/day05.lean)

---

#  Day 6

Toy boat race, with times and record distances.

####  Test

<pre>
Time:      7  15   30
Distance:  9  40  200
</pre>

[Solution in Lean](Advents/day06.lean)

---

#  Day 7

Camel cards, a simplified version of poker.

####  Test

<pre>
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
</pre>

[Solution in Lean](Advents/day07.lean)

---

#  Day 8

Traveling through the desert with ghosts.

####  Test 1

<pre>
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
</pre>

####  Test 2

<pre>
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
</pre>

####  Test 3

<pre>
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
</pre>

[Solution in Lean](Advents/day08.lean)

---

#  Day 9

Oasis measurements: compute iterated differences of sequences of integers.

####  Test

<pre>
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
</pre>

[Solution in Lean](Advents/day09.lean)

---

#  Day 10

Paths along pipes.

####  Test 1

<pre>
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
</pre>

####  Test 2

<pre>
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
</pre>

[Solution in Lean](Advents/day10.lean)

---

#  Day 11

Distances between galaxies in an expanding universe.

####  Test

<pre>
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
</pre>

### Description

#### Part 1

Extracted the coordinates of the positions of the galaxies.
Increased the `x` and `y` coordinates according to how
many skipped values there were before each one of them,
to reproduce the *expansion*.
After that, simply looped through all pairs,
accumulated the distances and divided by two.

#### Part 2

Same setup as for part 1, except that I increased the introduced
spacing in the *expansion* step.

[Solution in Lean](Advents/day11.lean)

---

