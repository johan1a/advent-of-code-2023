package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day03Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day03.part1(getInput("day03/test.txt")), 4361)
  }

  test("Part 1") {
    assertEquals(Day03.part1(getInput("day03/input.txt")), 528799)
  }

  // test("Part 2") {
  //   assertEquals(Day03.part2(getInput("day03/input.txt")), -1)
  // }

}
