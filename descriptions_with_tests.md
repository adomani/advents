#  [Day 1](https://adventofcode.com/2023/day/1)

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

#  [Day 2](https://adventofcode.com/2023/day/2)

RGB-coloured cubes in a bag, finding sups and products.

####  Test

<pre>
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
</pre>

### Comments

I wrote up two solutions for this problem.

One, more traditional, extracting the numbers from the input
and then performing the various operations on the extracted numbers.

The other, exploits defining new syntax for a Lean command.
This allows to make the input file itself a Lean command whose
evaluation is the solution to the problem that has it as its input.

The conversion is a little clunky, but this is just due to my limited
experience with parsing `Lean.Syntax`.

[Solution in Lean](Advents/day02.lean)

---

#  [Day 3](https://adventofcode.com/2023/day/3)

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

### Description

This mostly consisted of extracting positions of digits,
symbols and chaining consecutive digits to get numbers.

The biggest speed-up that I obtained was by extracting the
two rows surrounding a symbol, before looking for digit
neighbours of a given symbol!

[Solution in Lean](Advents/day03.lean)

---

#  [Day 4](https://adventofcode.com/2023/day/4)

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

### Description

#### Part 1

This is a relative straightforward parsing of the given numbers.
After that, what is left is finding common elements and
evaluating powers and sums.

#### Part 2

For the second part, I encoded the newly-generated cards in a
list of numbers.
Since the loop is over the initial length, I did not have to
take care that the list finished at the right place:
the algorithm does not reach any of the excess entries.

[Solution in Lean](Advents/day04.lean)

---

#  [Day 5](https://adventofcode.com/2023/day/5)

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

### Description

#### Part 1

The given data encodes a permutation of some range of natural
numbers.
To answer the first question, all that is needed is to pass
the initial 'seed' through the permutation and compute minima.

#### Part 2

It turns out that now the 'seed' represents a *very long*
list of seeds and they all need to go through the permutation
before the minimum is computed.

The trick that I used to compute the minimum is that the
permutations are *piece-wise increasing*.
Thus, the minimum is achieved on the lower end of an
increasing range, or at a place where there is a break.
Reverse-engineering where the breaks are and selecting the
ones contained in the 'seed ranges' turns out to be
sufficient to solve the question.

[Solution in Lean](Advents/day05.lean)

---

#  [Day 6](https://adventofcode.com/2023/day/6)

Toy boat race, with times and record distances.

####  Test

<pre>
Time:      7  15   30
Distance:  9  40  200
</pre>

### Description

Both questions revolve around computing the number
of integers on which a second-degree polynomial is
positive.
While the length of the real interval where
the polynomial is positive is simply the
square root of the discriminant of the quadratic,
the number of integer points inside it depends
on the exact location of the interval.
I could only solve part 1 by brute-force enumeration,
while for part 2 the discriminant approach worked well.

The issue with part 1 is particularly frustrating,
since the correct answer is at most 1 away from the
calculation involving the discriminant, but fixing
the 'off-by-one' error involved too much fiddling
around for my taste!

[Solution in Lean](Advents/day06.lean)

---

#  [Day 7](https://adventofcode.com/2023/day/7)

Camel cards, a simplified version of poker.

####  Test

<pre>
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
</pre>

### Description

Both parts involve sorting 5-card hands of cards
following an order naturally described as
* first a lexicographic ordering of the
  frequency of each card in each hand;
* second a lexicographic ordering of the
  cards, in the dealt order.

By abstracting the sorting rules out of the main
functions, a good part of the code can be used
for both parts.

#### Part 1

Using rules very similar to poker, the first part
involves sorting a list of 5-card hands and doing
operations with the sorted rank of each hand (plus
also using a number given next to each hand
in the given input).

#### Part 2

The second part, changes the sorting function.
In the first sorting (the *frequency* phase), the card
labeled by `J` should be interpreted as the most
beneficial that it could be.
Given the rules for sorting, the revised `Joker`
card is always best assigned to the most-frequent
value.
The second, tie-breaking sorting is lexicographic
as before, except that the relative order of the
card `J` with all the others is different.

[Solution in Lean](Advents/day07.lean)

---

#  [Day 8](https://adventofcode.com/2023/day/8)

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

### Description

#### Part 1

In part 1, simply 'going through the moves' is fast enough.

#### Part 2

For the second part, the various starting points are
really independent processes each running with its own
period.
They all finish in their end-positions simultaneously
at time intervals that are proportional to the `lcm` of
the individual periods.
Thus, the answer is the least common multiple of the periods.

*Note*.
The periods for my input are all themselves multiples of 269.

[Solution in Lean](Advents/day08.lean)

---

#  [Day 9](https://adventofcode.com/2023/day/9)

Oasis measurements: compute iterated differences of sequences of integers.

####  Test

<pre>
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
</pre>

### Description

For the two parts, the goal is to compute iterated first
differences of lists of numbers until all differences
are zero.
After that, you should extend either on the right or on
the left the initial sequence so that the final pattern
of zeros persists.

Equivalently, you could compute the polynomial of smallest
degree, whose values on `{1, 2, ..., n}` are the initial
sequence.
The two extensions are then simply the evaluation of the
polynomial at `n + 1` and at `0`.

[Solution in Lean](Advents/day09.lean)

---

#  [Day 10](https://adventofcode.com/2023/day/10)

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

### Description

#### Part 1

Pretty straightforward:
* locate where `S` is on the map;
* look for which neighbours of `S` you can reach `S`;
* start from one of the neighbours and loop around
  following the rules and storing the visited positions.

Thus, you end up with a path starting from `S` and ending
just before it reaches `S` again.

Half the length of this path is the answer to part 1.

#### Part 2

I found this part very cute!

I oriented the path found in part 1, by attaching to each
position the counter-clockwise rotated step that you can take
from there (and to there, in case they yield different answers).

Now, from any location not on the path, start moving right.
* If you reach the path, look at whether the arrow is pointing
  towards you or away: in one case you are in, in the other out!
* If you reach the boundary of the grid, then you are out.

[Solution in Lean](Advents/day10.lean)

---

#  [Day 11](https://adventofcode.com/2023/day/11)

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

#  [Day 12](https://adventofcode.com/2023/day/12)

Counting ways of filling in `#`s and `.`s.  (Missing part 2)

####  Test

<pre>
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
</pre>

### Description

I solved part 1, but part 2 is still in progress!

[Solution in Lean](Advents/day12.lean)

---

#  [Day 13](https://adventofcode.com/2023/day/13)

Finding axes of symmetry among rock and ash.

####  Test

<pre>
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
</pre>

### Description

The input data consists of several maps with locations of ash and rocks.
The goals revolve around finding horizontal or vertical axes of symmetry
in the maps.

#### Part 1

Each map has exactly one axis of symmetry.
From the positions of the axes, you can compute the answer.

#### Part 2

It turns out that, by changing exactly one map point
from ash to rock or viceversa, that the resulting map
acquires an axis of symmetry *different* from the
original one.
Of course, after the switch, the old axis of symmetry
may no longer be an axis of symmetry.
All that matters is that, after the switch, there are
at most two axes of symmetry:
* the new one that must be present,
* the old one, in case it stays.

In some maps, there may be several location switches that
comply with these rules, but they all happen to produce
the *same new* axis of symmetry.
Processing the locations of the new axes of symmetry as before
yields the answer to part 2.

[Solution in Lean](Advents/day13.lean)

---

#  [Day 14](https://adventofcode.com/2023/day/14)

Rolling rocks in a maze. (Missing part 2)

####  Test

<pre>
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
</pre>

### Description

The data is a map with locations of
* round (moving) rocks, denoted by `O`;
* non-moving rocks, denoted by `#`;
* empty spaces, denoted by `·`.

In both parts, the surface described map can be tilted
so that the moving rocks roll in direction of the tilt
until they reach either the boundary of the surface or
a fixed rock.

#### Part 1

The goal was to perform a single tilt, figure out the
final positions of the moving rocks and compute the
*total load* -- a weight determined by the final positions.

#### Part 2

Instead of tilting only once, now we successively tilt
in succession in each of the four directions
north, west, south, east, doing each such cycle of four
tilts 1000000000 times.
After that, we still need to compute the *total load*
of the final configuration.

[Solution in Lean](Advents/day14.lean)

---

#  [Day 15](https://adventofcode.com/2023/day/15)

Fitting lenses.

####  Test

<pre>
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
</pre>

### Description

#### Part 1

The first part gives the instructions for converting strings
to natural numbers, using the ASCII values of their characters.

#### Part 2

The second part parses a little more the input and uses it
as instructions to create a list of boxes containing lens
arrangements.

From the final arrangement, you get the answer to part 2.

[Solution in Lean](Advents/day15.lean)

---

