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
