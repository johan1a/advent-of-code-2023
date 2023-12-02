package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day02Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day02.part1(getInput("day02/test.txt")), 8)
  }

  test("Part 1") {
    assertEquals(Day02.part1(getInput("day02/input.txt")), 2149)
  }

  test("Part 2 test") {
    assertEquals(Day02.part2(getInput("day02/test.txt")), 2286)
  }

  test("Part 2") {
    assertEquals(Day02.part2(getInput("day02/input.txt")), 71274)
  }

}
