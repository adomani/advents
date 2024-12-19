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

#  [Day 4](https://adventofcode.com/2024/day/4)

Look for the word `XMAS` and `X-MAS` in a grid of letters.

####  Test

<pre>
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
</pre>

### Description

#### Part 1

For part 1, we scan for the word `XMAS` starting anywhere on the grid and continuing in each of the 8 directions:
horizontal, vertical or diagonal and left/right or up/down.

#### Part 2

For part 2, we should find instead an `X` of `MAS`.
This means that we are looking for a rotation of the pattern
```
M M
 A
S S
```
anywhere on the grid.

[Solution in Lean](day04.lean)

---

#  [Day 5](https://adventofcode.com/2024/day/5)

Checking whether some pages are correctly sorted.

####  Test

<pre>
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
</pre>

### Description

The input begins with a list of pairs of natural numbers, in the form `n1|n2`, representing the correct ordering that the pages should have.
It then continues with a list of actual orders of pages, in the form `p₁,p₂,...,pₙ`.

#### Part 1

Add the number of pages that appear in the middle of each *correctly ordered* list of pages.

#### Part 2

Add the number of pages that appear in the middle of each *incorrectly ordered* list of pages.

[Solution in Lean](day05.lean)

---

#  [Day 6](https://adventofcode.com/2024/day/6)

The input is a map with locations of obstacles and the location of a guard.

####  Test

<pre>
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
</pre>

### Description

#### Part 1

The guard moves in a fixed direction, until it reaches an obstacle.
At that point, it rotates by 90⁰ clockwise and continues.
The first question is to determine how many locations on the map the guard occupies, before exiting the map.

#### Part 2

For the second part, we should find all positions on the grid such that placing one further an obstacle in that position, makes the guard enter into a loop.

[Solution in Lean](day06.lean)

---

#  [Day 7](https://adventofcode.com/2024/day/7)

Lists of total and possible operands, but missing operations!

####  Test

<pre>
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
</pre>

### Description

#### Part 1

Each row starts with a
</pre>target" value and a list of possible summands.
Inserting `*` and `+` in all possible ways (and *always* associate to the left),
sum the values of the rows that *can* represent a correct operation, with an appropriate
choice of `*` and `+`.

#### Part 2

Similar to part 1, except that there is a third operation that is allowed:
concatenation of natural numbers, written in decimal.
This extra operation maps `12` and `345` to `12345`.

[Solution in Lean](day07.lean)

---

#  [Day 8](https://adventofcode.com/2024/day/8)

Find the resonant harmonics in a grid of antenna locations.

####  Test

<pre>
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
</pre>

####  Test 2

<pre>
..........
...#......
..........
....a.....
..........
.....a....
..........
......#...
..........
..........
</pre>

####  Test 3

<pre>
T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
..........
</pre>

### Description

#### Part 1

The input consists of a map with the positions of antennas.
For each pair `(p, q)` of locations of distinct antennas of the same kind,
we determine the location `p + (p - q)` of a resonant antinode.
We count how many such resonant antinodes there are that are contained in the initial grid.

#### Part 2

Similar to part 1, except that instead of just the location `p + (p - q)`, we want to count all
locations `p + n * (p - q)`, for all `n ≠ 0`, again only counting the positions that end up in the
initial grid.

[Solution in Lean](day08.lean)

---

#  [Day 9](https://adventofcode.com/2024/day/9)

Fragment and defragment a disk, based on blocks that are free or occupied by files.

####  Test

<pre>
2333133121414131402
</pre>

### Description

#### Part 1

The input is a long number in decimal notation.
The digit encode the location of consecutive free blocks and file blocks of a file-system.
The goal of the first part is to compress as much as possible the files,
by moving individual occupied blocks from the right to the left-most available free space.

#### Part 2

Similar to part 1, except that now we move all the blocks of a file together, rather than fragmenting.
If there is no consecutive sequence of free blocks at least as long as the current file to the left of the file,
then we skip the file and move to the next one to its left.

[Solution in Lean](day09.lean)

---

#  [Day 10](https://adventofcode.com/2024/day/10)

The input is a height map.

####  Test

<pre>
0123
1234
8765
9876
</pre>

####  Test 2

<pre>
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
</pre>

### Description

#### Part 1

A valid path starts at any `0`, ends at any `9` and increases by exactly one at each step.
Steps are only allowed in the four
</pre>grid" directions: up, down, left, right.
Determine the number of pairs `(start, end)`, where
* `start` is the position of a `0`;
* `end` is the position of a `9`;
* there is a path paths from `start` to `end`.

#### Part 2

Similar to part 1, except that now we should determine the total number of valid paths,
not just their existence.

[Solution in Lean](day10.lean)

---

#  [Day 11](https://adventofcode.com/2024/day/11)

The input is a sequence of numbers written on stones.

####  Test

<pre>
0 1 10 99 999
</pre>

####  Test 2

<pre>
125 17
</pre>

### Description

#### Part 1

Each time you blink, each stones changes, by either modifying its value or splitting into two stones.
Determine the total sum of the values of the configuration of the stone after you blink 25 times.

#### Part 2

Similar to part 1, except that now you blink 75 times.

[Solution in Lean](day11.lean)

---

#  [Day 12](https://adventofcode.com/2024/day/12)

The input is a grid with locations of where different kinds of plants have been planted.

####  Test 1

<pre>
AAAA
BBCD
BBCC
EEEC
</pre>

####  Test 2

<pre>
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
</pre>

####  Test 3

<pre>
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
</pre>

####  Test

<pre>
EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
</pre>

####  Test

<pre>
AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
</pre>

### Description

#### Part 1

We should find the sum of `area * perimeter` for each connected component of the garden containing
plants of a single variety.

#### Part 2

Similar to part one, except that we should find the sum of `area * corners` for each connected
component of the garden containing plants of a single variety.

[Solution in Lean](day12.lean)

---

#  [Day 13](https://adventofcode.com/2024/day/13)

The input is a description of various games with the claw.

####  Test

<pre>
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
</pre>

### Description

#### Part 1

We should determine the intersection points between two lines.
All the lines are non-parallel, the intersection points are required to be integers and
non-negative.

Once we found these intersection points, we tally them using `3` and `1` as weights
for their coordinates.

#### Part 2

Similar to part one, except that the target positions are now increased by `10000000000000`.

[Solution in Lean](day13.lean)

---

#  [Day 14](https://adventofcode.com/2024/day/14)

The input is a list of positions and velocities of robots in a periodic grid.

####  Test

<pre>
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
</pre>

### Description

#### Part 1

We should determine the product of the numbers of robots in each of the four quadrants of the grid,
after the robots moved 100 times.

#### Part 2

There is an Easter Egg: once in every period, a subset of the robots draws a Christmas tree in one of the quadrats.
We should determine the first time that this happens.
```
············#······································
·#···················#·····························
···················································
···················································
·#········###############################··········
··········#·····························#··········
··········#·····························#··········
··········#·····························#··········
··········#·····························#··········
··········#··············#··············#··········
··········#·············###·············#··········
··········#············#####············#··········
····#·····#···········#######···········#··········
··········#··········#########··········#····#·····
#·········#············#####············#··········
··········#···········#######···········#··········
··········#··········#########··········#··········
#·········#·········###########·········#··········
··········#········#############········#··········
··········#··········#########··········#··········
··········#·········###########·········#··········
··········#········#############········#··········
··········#·······###############·······#·····#····
··········#······#################······#··········
··········#········#############········#··········
··········#·······###############·······#····#·····
··········#······#################······#··········
··········#·····###################·····#····#·····
··········#····#####################····#··········
··········#·············###·············#··········
·#········#·············###·············#··········
··········#·············###·············#··········
··········#·····························#··········
··········#·····························#·····#····
··········#·····························#··········
··········#·····························#·#········
··········###############################··········
···················································
··················································#
·····················#···#·························
```

[Solution in Lean](day14.lean)

---

#  [Day 15](https://adventofcode.com/2024/day/15)

The lanternfish maze: a robot pushing boxes around.

####  Test

<pre>
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
</pre>

####  Test 2

<pre>
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
</pre>

####  Test 3

<pre>
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^>
</pre>

### Description

#### Part 1

As the robot moves around the grid, it pushes the boxes around.
We should determine the GPS value of the final configuration,
adding the appropriately weighted `x` and `y` coordinates.

#### Part 2

It turns out that the grid and everything inside except for the bot should be stretched in the
horizontal direction by a factor of 2.
The boxes, in particular, can now be stacked and pushed also when they overlap in a single position.

[Solution in Lean](day15.lean)

---

#  [Day 16](https://adventofcode.com/2024/day/16)

The Reindeer maze: most steps are forced, sometimes you have to rotate.

####  Test 1

<pre>
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
</pre>

####  Test 2

<pre>
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
</pre>

### Description

#### Part 1

We should determine the smallest score to go from `S` to `E`:
continuing in the current direction costs 1 point, rotating costs 1000.

#### Part 2

Now we should determine the number of the tiles that are contained in some path of minimum score
between `S` and `E`.

[Solution in Lean](day16.lean)

---

#  [Day 17](https://adventofcode.com/2024/day/17)

The state machine and its list of instructions.

####  Test

<pre>
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
</pre>

####  Test 2

<pre>
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
</pre>

### Description

#### Part 1

Run the machine until it halts and report the answer.
Note that the answer is a *comma-separated* list of digits, not necessarily a single number!

#### Part 2

For the second part we should determine the smallest initial state of the machine that reproduces
a copy of its own program.

[Solution in Lean](day17.lean)

---

#  [Day 18](https://adventofcode.com/2024/day/18)

Historians making their way in memory, avoiding corrupted bytes.

####  Test

<pre>
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
</pre>

### Description

#### Part 1

Find the smallest number of steps in the memory space going between start and finish.

#### Part 2

For the second part, we should determine the position of the first corrupted byte that prevents
a path for the historians.
Effectively, I started two processes:
* one looking for a path for the historians and
* one looking for a blocking path for the corrupted memory blocks.

The first one to finish decides who wins.
Bisecting on the line at which each corrupted byte appears, we find the location of the first byte
that seals off the start from the finish.

[Solution in Lean](day18.lean)

---

#  [Day 19](https://adventofcode.com/2024/day/19)

Creating designs by concatenating towels.

####  Test

<pre>
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
</pre>

### Description

#### Part 1

The input contains some towels (a few strings) and designs (more strings, but longer!).
We should determine how many of the designs can be written as concatenations of towels, with
repetitions of towels allowed.

#### Part 2

Similar to part 1, but now we should determine in how many ways each design can be written as a
concatenation of towels.

I ended up storing previous value to compute later ones: this produced a speed up by a factor of
approximately 6.

[Solution in Lean](day19.lean)

---
