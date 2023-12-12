package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day12Test extends munit.FunSuite {

  test("Part 1 test0") {
    assertEquals(Day12.part1(getInput("day12/test0.txt")), 1L)
  }

  test("Part 1 test1") {
    assertEquals(Day12.part1(getInput("day12/test1.txt")), 4L)
  }

  test("Part 1 test2") {
    assertEquals(Day12.part1(getInput("day12/test2.txt")), 10L)
  }

  test("Part 1 test") {
    assertEquals(Day12.part1(getInput("day12/test.txt")), 21L)
  }

  test("Part 1") {
    assertEquals(Day12.part1(getInput("day12/input.txt")), 8270L)
  }

  test("Part 2 test0") {
    assertEquals(Day12.part2(getInput("day12/test0.txt")), 1L)
  }

  test("Part 2 test3") {
    assertEquals(Day12.part2(getInput("day12/test3.txt")), 16L)
  }

  test("Part 2 test") {
    assertEquals(Day12.part2(getInput("day12/test.txt")), 525152L)
  }

  test("Part 2") {
    assertEquals(Day12.part2(getInput("day12/input.txt")), 204640299929836L)
  }

}
