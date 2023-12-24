package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day24Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day24.part1(getInput("day24/test.txt"), 7, 27), 2L)
  }

  test("Part 1") {
    assertEquals(Day24.part1(getInput("day24/input.txt")), 21679L)
  }

  // test("Part 2") {
  //   assertEquals(Day24.part2(getInput("day24/input.txt")), -1L)
  // }

}
