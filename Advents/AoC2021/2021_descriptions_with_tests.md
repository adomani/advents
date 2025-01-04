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

#  [Day 16](https://adventofcode.com/2021/day/16)

A single hexadecimal number, providing instructions to construct a
</pre>packet".

####  Test

<pre>

####  Test 1

<pre>
8A004A801A8002F478
</pre>

####  Test 2

<pre>
620080001611562C8802118E34
</pre>

####  Test 3

<pre>
C0015000016115A2E0802F182340
</pre>

####  Test 4

<pre>
A0016C880162017C3686B18A3D4780
</pre>

### Description

#### Part 1

Decode the
</pre>packet" and report the sum of the "versions" of the packet itself and
all its sub-packets.

#### Part 2

Decode the
</pre>packet" fully, interpret the operations that it encodes and report the result.

[Solution in Lean](day16.lean)

---

#  [Day 17](https://adventofcode.com/2021/day/17)

The input is a rectangular range: we should aim for it!

####  Test

<pre>
target area: x=20..30, y=-10..-5
</pre>

### Description

#### Part 1

Find the highest `y`-coordinate that a probe the passes through the given rectangular range can achieve.

#### Part 2

Now find all the possible initial velocities that allow a probe to reach the rectangular range.

[Solution in Lean](day17.lean)

---

#  [Day 19](https://adventofcode.com/2021/day/19)

Aligning beacons and scanners in space.

####  Test

<pre>
--- scanner 0 ---
0,2
4,1
3,3

--- scanner 1 ---
-1,-1
-5,0
-2,1
</pre>

####  Test 2

<pre>
--- scanner 0 ---
-1,-1,1
-2,-2,2
-3,-3,3
-2,-3,1
5,6,-4
8,0,7

--- scanner 0 ---
1,-1,1
2,-2,2
3,-3,3
2,-1,3
-5,4,-6
-8,-7,0

--- scanner 0 ---
-1,-1,-1
-2,-2,-2
-3,-3,-3
-1,-3,-2
4,6,5
-7,0,8

--- scanner 0 ---
1,1,-1
2,2,-2
3,3,-3
1,3,-2
-4,-6,5
7,0,8

--- scanner 0 ---
1,1,1
2,2,2
3,3,3
3,1,2
-6,-4,-5
0,7,-8
</pre>

####  Test 3

<pre>
--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14
</pre>

### Description

#### Part 1

The input contains the coordinates of the beacons that each scanner detects.
In part 1, we should find how many beacons there are, taking into account that beacons seen by
each scanner may overlap with the other beacons.

#### Part 2

Now we should determine the largest Manhattan distance between any two *scanners*
(not the beacons!).

[Solution in Lean](day19.lean)

---

#  [Day 20](https://adventofcode.com/2021/day/20)

Repeatedly enhancing an image.

####  Test

<pre>
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
</pre>

### Description

#### Part 1

The input contains a string of length 512, enconding an `i`mage `e`nhancing `a`lgorithm, and
an initial image (a grid of `#` and `.`).
In part 1, we should run the `iea` twice and find out how many pixels are lit as a result.

#### Part 2

In part 2, we should run the `iea` 50 times and find out how many pixels are lit as a result.

[Solution in Lean](day20.lean)

---

#  [Day 22](https://adventofcode.com/2021/day/22)

Switching on and off overlapping cuboids.

####  Test 1

<pre>
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
</pre>

####  Test 2

<pre>
on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682
</pre>

####  Test 3

<pre>
on x=-5..47,y=-31..22,z=-19..33
on x=-44..5,y=-27..21,z=-14..35
on x=-49..-1,y=-11..42,z=-10..38
on x=-20..34,y=-40..6,z=-44..1
off x=26..39,y=40..50,z=-2..11
on x=-41..5,y=-41..6,z=-36..8
off x=-43..-33,y=-45..-28,z=7..25
on x=-33..15,y=-32..19,z=-34..11
off x=35..47,y=-46..-34,z=-11..5
on x=-14..36,y=-6..44,z=-16..29
on x=-57795..-6158,y=29564..72030,z=20435..90618
on x=36731..105352,y=-21140..28532,z=16094..90401
on x=30999..107136,y=-53464..15513,z=8553..71215
on x=13528..83982,y=-99403..-27377,z=-24141..23996
on x=-72682..-12347,y=18159..111354,z=7391..80950
on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
on x=-52752..22273,y=-49450..9096,z=54442..119054
on x=-29982..40483,y=-108474..-28371,z=-24328..38471
on x=-4958..62750,y=40422..118853,z=-7672..65583
on x=55694..108686,y=-43367..46958,z=-26781..48729
on x=-98497..-18186,y=-63569..3412,z=1232..88485
on x=-726..56291,y=-62629..13224,z=18033..85226
on x=-110886..-34664,y=-81338..-8658,z=8914..63723
on x=-55829..24974,y=-16897..54165,z=-121762..-28058
on x=-65152..-11147,y=22489..91432,z=-58782..1780
on x=-120100..-32970,y=-46592..27473,z=-11695..61039
on x=-18631..37533,y=-124565..-50804,z=-35667..28308
on x=-57817..18248,y=49321..117703,z=5745..55881
on x=14781..98692,y=-1341..70827,z=15753..70151
on x=-34419..55919,y=-19626..40991,z=39015..114138
on x=-60785..11593,y=-56135..2999,z=-95368..-26915
on x=-32178..58085,y=17647..101866,z=-91405..-8878
on x=-53655..12091,y=50097..105568,z=-75335..-4862
on x=-111166..-40997,y=-71714..2688,z=5609..50954
on x=-16602..70118,y=-98693..-44401,z=5197..76897
on x=16383..101554,y=4615..83635,z=-44907..18747
off x=-95822..-15171,y=-19987..48940,z=10804..104439
on x=-89813..-14614,y=16069..88491,z=-3297..45228
on x=41075..99376,y=-20427..49978,z=-52012..13762
on x=-21330..50085,y=-17944..62733,z=-112280..-30197
on x=-16478..35915,y=36008..118594,z=-7885..47086
off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
off x=2032..69770,y=-71013..4824,z=7471..94418
on x=43670..120875,y=-42068..12382,z=-24787..38892
off x=37514..111226,y=-45862..25743,z=-16714..54663
off x=25699..97951,y=-30668..59918,z=-15349..69697
off x=-44271..17935,y=-9516..60759,z=49131..112598
on x=-61695..-5813,y=40978..94975,z=8655..80240
off x=-101086..-9439,y=-7088..67543,z=33935..83858
off x=18020..114017,y=-48931..32606,z=21474..89843
off x=-77139..10506,y=-89994..-18797,z=-80..59318
off x=8476..79288,y=-75520..11602,z=-96624..-24783
on x=-47488..-1262,y=24338..100707,z=16292..72967
off x=-84341..13987,y=2429..92914,z=-90671..-1318
off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
off x=-27365..46395,y=31009..98017,z=15428..76570
off x=-70369..-16548,y=22648..78696,z=-1892..86821
on x=-53470..21291,y=-120233..-33476,z=-44150..38147
off x=-93533..-4276,y=-16170..68771,z=-104985..-24507
</pre>

### Description

#### Part 1

The input is a list of ranges of cuboids and whether they are `on` or `off`.
Part 1 asks how many are on in the cube `[-50..50] ^ 3`.

#### Part 2

In part 2, we should perform the computation on *all* the cubes.

[Solution in Lean](day22.lean)

---

#  [Day 25](https://adventofcode.com/2021/day/25)

Seacucumbers moving right and down.

####  Test

<pre>
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
</pre>

### Description

#### Part 1

The input is a configuration of two herds of seacucumbers: some move right, other move down.
Part 1 asks after how many steps, the seacucumbers run out of moves.

#### Part 2

Obtain all the stars!

[Solution in Lean](day25.lean)

---
