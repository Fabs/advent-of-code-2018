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
