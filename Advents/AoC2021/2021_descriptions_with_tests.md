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
The cost of moving a crab is $\binom{d}{2}$, were $d$ is the distance to the final position.
We need to determine the minimum cost.

[Solution in Lean](day07.lean)

---

#  [Day 8](https://adventofcode.com/2021/day/8)

The input contains 10 signal/wire permutations and 4 digits obtained from them.

####  Test

<pre>
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
</pre>

### Description

#### Part 1

Some seven-segment displays are uniquely determined by the number of lit segments.
Count how many such
</pre>easy" displays appear in the second halves of the lists .

#### Part 2

Now, fully decrypt the digits using all the information from the first half of each input and sum the 4-digit numbers thus determined on the second halves of the inputs.

[Solution in Lean](day08.lean)

---

#  [Day 9](https://adventofcode.com/2021/day/9)

The input is the elevation grid of the lava caves.

####  Test

<pre>
2199943210
3987894921
9856789892
8767896789
9899965678
</pre>

### Description

#### Part 1

We should determine the heights of the locations of the
</pre>low points":
these are the points on the elevation grid that have height less than or equal to all of their neighbours *and* actually strictly smaller than some neighbour.
The sum of the risk levels of these low points answers part 1.

#### Part 2

For the second part, we determine the product of the sizes of the three larges basins.
Each basin is obtained from a low point by growing in a connected manner until the boundary consists only of vertices of height 9 (that are not part of the basin) and external walls of the grid.

[Solution in Lean](day09.lean)

---

#  [Day 10](https://adventofcode.com/2021/day/10)

A list of sequences of open and closed parentheses, some malformed, others incomplete.

####  Test

<pre>
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
</pre>

### Description

#### Part 1

Identify the malformed sequences of parentheses and compute the total penalties for the first incorrectly matched
parentheses.

#### Part 2

For the second part, compute the score of each incomplete set of parentheses and report the middle value of the scores.

[Solution in Lean](day10.lean)

---

#  [Day 11](https://adventofcode.com/2021/day/11)

A list energy levels of bioluminescent dumbo octopuses.

####  Test

<pre>
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
</pre>

### Description

#### Part 1

Figure out how many flashes the octopuses emit in 100 steps.

#### Part 2

Figure out when the flashes of the octopuses synchronize for the first time.

[Solution in Lean](day11.lean)

---

#  [Day 12](https://adventofcode.com/2021/day/12)

The passageways through the subterranean caves, both small and big.

####  Test

<pre>
start-A
start-b
A-c
A-b
b-d
A-end
b-end
</pre>

####  Test 2

<pre>
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
</pre>

####  Test 3

<pre>
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
</pre>

### Description

#### Part 1

Count the number of paths from `start` to `end` that visit each
</pre>small" cave at most once.

#### Part 2

Count the number of paths from `start` to `end` that visit at most one
</pre>small" cave, other than `start` or `end`, twice and all remaining "small" caves at most once.

[Solution in Lean](day12.lean)

---

#  [Day 13](https://adventofcode.com/2021/day/13)

Folding a grid with dots to create a code word.

####  Test

<pre>
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
</pre>

### Description

#### Part 1

The input consists of the coordinates of some points on a sheet of transparent paper.
The instructions explain how to fold it.
To answer part 1, we just have to fold once.

#### Part 2

To answer part 2, we follow all the folding instructions.
Once that is done, the dots align to produce the code word below, that is the answer to the puzzle.
```
#### #### #    ####   ##  ##  ###  ####
#    #    #    #       # #  # #  # #
###  ###  #    ###     # #    #  # ###
#    #    #    #       # # ## ###  #
#    #    #    #    #  # #  # # #  #
#### #    #### #     ##   ### #  # #
```

[Solution in Lean](day13.lean)

---

#  [Day 14](https://adventofcode.com/2021/day/14)

Inserting elements to form long polymers.

####  Test

<pre>
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
</pre>

### Description

#### Part 1

The input consists of a starting polymer and the insertion rules for the polymer.
The puzzle consists in iteratively applying the insertion rules and reporting a statistic of the final distribution of elements.

#### Part 2

Same as part 2, except performing a larger number of iterations.
I initially coded part 1 following the most direct implementation of what was asked.
However, the current implementation is the more efficient one that I used in order to get the result for part 2.
The main observation is that the interleaved insertion hides the possibility of counting *multiplicities*.
Separating out the string in all of its pairs of adjacent elements makes counting with multiplicities possible and extends the reach of the algorithm.

[Solution in Lean](day14.lean)

---

#  [Day 15](https://adventofcode.com/2021/day/15)

Navigating the ocean floor, minimizing the risk of hitting a chiton.

####  Test

<pre>
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
</pre>

### Description

#### Part 1

The input consists of $100 	imes 100$ grid of digits.
Find the smallest total sum of a path that starts from the top left corner and exists at the bottom right corner.

#### Part 2

Same as part 2, except that the grid is now 5 times larger in each direction.

My main realization for this puzzle was that finding the minimum weighted distances to *all* the points of the grid was more effective that just figuring out the value for one path.
Also, finding the minimum sum, but not also a minimizing path at the same time helped!

[Solution in Lean](day15.lean)

---

#  [Day 17](https://adventofcode.com/2021/day/17)

####  Test

<pre>
target area: x=20..30, y=-10..-5
</pre>

[Solution in Lean](day17.lean)

---
