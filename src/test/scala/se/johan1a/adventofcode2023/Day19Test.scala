package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day19Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day19.part1(getInput("day19/test.txt")),19114)
  }

  test("Part 1") {
    assertEquals(Day19.part1(getInput("day19/input.txt")), 397061)
  }

  test("Part 2") {
    assertEquals(Day19.part2(getInput("day19/input.txt")), -1)
  }

}
