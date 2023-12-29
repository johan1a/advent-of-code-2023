package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day25Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day25.part1(getInput("day25/test.txt")), 54)
  }

  test("Part 1") {
    assertEquals(Day25.part1(getInput("day25/input.txt")), 543564)
  }

}
