package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day13Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day13.part1(getInput("day13/test.txt")), 405)
  }

  test("Part 1") {
    assertEquals(Day13.part1(getInput("day13/input.txt")), 30535)
  }

  // test("Part 2") {
  //   assertEquals(Day13.part2(getInput("day13/input.txt")), -1)
  // }

}
