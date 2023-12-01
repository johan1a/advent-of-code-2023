package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day01Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day01.part1(getInput("day01/test.txt")), 142)
  }

  test("Part 1") {
    assertEquals(Day01.part1(getInput("day01/input.txt")), 142)
  }

  test("Part 2") {
    assertEquals(Day01.part2(getInput("day01/input.txt")), -1)
  }

}
