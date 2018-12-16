all:
	stack build
	stack install
day01: all
	cat inputs/day1.1.in | aoc2018 1 1
	cat inputs/day1.2.in | aoc2018 1 2

day02: all
	cat inputs/day2.1.sample.in | aoc2018 2 1
	cat inputs/day2.1.in | aoc2018 2 1
	cat inputs/day2.1.sample.in | aoc2018 2 2
	cat inputs/day2.2.sample.in | aoc2018 2 2
	cat inputs/day2.1.in | aoc2018 2 2

day03: all
	cat inputs/day3.sample.in | aoc2018 3 1
	cat inputs/day3.in | aoc2018 3 1
	cat inputs/day3.in | aoc2018 3 2

day04: all
	cat inputs/day4.sample.in | aoc2018 4 1
	cat inputs/day4.in | aoc2018 4 1
	cat inputs/day4.in | aoc2018 4 2

day05: all
	cat inputs/day5.sample.in | aoc2018 5 1
	cat inputs/day5.in | aoc2018 5 1
	cat inputs/day5.in | aoc2018 5 2

day06: all
	cat inputs/day6.sample.in | aoc2018 6 1
	cat inputs/day6.in | aoc2018 6 1
	cat inputs/day6.in | aoc2018 6 2

day07: all
	cat inputs/day7.sample.in | aoc2018 7 1
	cat inputs/day7.in | aoc2018 7 1
	cat inputs/day7.in | aoc2018 7 2
	cat inputs/day7.sample.in | aoc2018 7 2

day08: all
	cat inputs/day8.sample.in | aoc2018 8 1
	cat inputs/day8.in | aoc2018 8 1
	cat inputs/day8.sample.in | aoc2018 8 2
	cat inputs/day8.in | aoc2018 8 2

day09: all
	aoc2018 9 1 9 25 32
	aoc2018 9 1 10 1618 8317
	aoc2018 9 1 13 7999 146373
	aoc2018 9 1 17 1104 2764
	aoc2018 9 1 21 6111 54718
	aoc2018 9 1 30 5807 37305
	aoc2018 9 1 428 70825 -1
	aoc2018 9 1 428 7082500 -1

day10: all
	cat inputs/day10.sample.in | aoc2018 10 2
	cat inputs/day10.in | aoc2018 10 2

day11: all
	aoc2018 11 7315
	aoc2018 11 18


day12: all
	cat inputs/day12.sample.in | aoc2018 12 1


day13: all
	cat inputs/day13.sample.in | aoc2018 13 1

day14: all
	aoc2018 14 1 9 5158916779
	aoc2018 14 1 5 0124515891
	aoc2018 14 1 18 9251071085
	aoc2018 14 1 2018 5941429882
	aoc2018 14 1 702831 1132413111
	aoc2018 14 1 20340233 0

day15: all
	cat inputs/day.15.1 | aoc2018 15 1 27730
	cat inputs/day.15.2 | aoc2018 15 1 36334
	cat inputs/day.15.3 | aoc2018 15 1 39514
	cat inputs/day.15.4 | aoc2018 15 1 27755
	cat inputs/day.15.5 | aoc2018 15 1 27755
	cat inputs/day.15   | aoc2018 15 1 1889910
