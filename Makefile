all:
	stack build
	stack install
day1: all
	cat inputs/day1.1.in | aoc2018 1 1
	cat inputs/day1.2.in | aoc2018 1 2

day2: all
	cat inputs/day2.1.sample.in | aoc2018 2 1
	cat inputs/day2.1.in | aoc2018 2 1
	cat inputs/day2.1.sample.in | aoc2018 2 2
	cat inputs/day2.2.sample.in | aoc2018 2 2
	cat inputs/day2.1.in | aoc2018 2 2
