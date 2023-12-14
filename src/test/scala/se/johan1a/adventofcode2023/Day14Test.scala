package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day14Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day14.part1(getInput("day14/test.txt")), 136)
  }

  test("Part 1") {
    assertEquals(Day14.part1(getInput("day14/input.txt")), 106648)
  }

  // test("Part 2 test") {
  //   assertEquals(Day14.part2(getInput("day14/test.txt")), -1)
  // }

  // test("Part 2") {
  //   assertEquals(Day14.part2(getInput("day14/input.txt")), -1)
  // }

}
