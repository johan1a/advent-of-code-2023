package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day23Test extends munit.FunSuite {

  test("Part 1 test0") {
    assertEquals(Day23.part1(getInput("day23/test0.txt")), 12L)
  }

  test("Part 1 test") {
    assertEquals(Day23.part1(getInput("day23/test.txt")), 94L)
  }

  test("Part 1") {
    assertEquals(Day23.part1(getInput("day23/input.txt")), 2094L)
  }

  test("Part 2 test") {
    assertEquals(Day23.part2(getInput("day23/test.txt")), 154L)
  }

  // test("Part 2") {
  //   assertEquals(Day23.part2(getInput("day23/input.txt")), -1L)
  // }

}
