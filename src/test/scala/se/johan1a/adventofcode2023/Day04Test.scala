package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day04Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day04.part1(getInput("day04/input.txt")), 21138)
  }

  test("Part 1 test") {
    assertEquals(Day04.part1(getInput("day04/test.txt")), 13)
  }

  test("Part 2") {
    assertEquals(Day04.part2(getInput("day04/input.txt")), -1)
  }

}
