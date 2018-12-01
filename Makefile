all:
	stack build
	stack install
day1: all
	cat inputs/day1.1.in | aoc2018 1 1
	cat star2 | aoc2018 1 2
	cat star22 | aoc2018 1 2
	cat inputs/day1.2.in | aoc2018 1 2
