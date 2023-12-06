package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day06Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day06.part1(getInput("day06/test.txt")), 288)
  }

  test("Part 1") {
    assertEquals(Day06.part1(getInput("day06/input.txt")), 252000)
  }

  test("Part 2 test") {
    assertEquals(Day06.part2(getInput("day06/test.txt")), 71503)
  }

  test("Part 2") {
    assertEquals(Day06.part2(getInput("day06/input.txt")), 36992486)
  }

}
