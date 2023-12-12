package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day12Test extends munit.FunSuite {

  test("Part 1 test0") {
    assertEquals(Day12.part1(getInput("day12/test0.txt")), 1)
  }

  test("Part 1 test1") {
    assertEquals(Day12.part1(getInput("day12/test1.txt")), 4)
  }

  test("Part 1 test2") {
    assertEquals(Day12.part1(getInput("day12/test2.txt")), 10)
  }

  test("Part 1 test") {
    assertEquals(Day12.part1(getInput("day12/test.txt")), 21)
  }

  test("Part 1") {
    assertEquals(Day12.part1(getInput("day12/input.txt")), 8270)
  }

  // test("Part 2 test") {
  //   assertEquals(Day12.part2(getInput("day12/test.txt")), -1)
  // }
  //
  // test("Part 2") {
  //   assertEquals(Day12.part2(getInput("day12/input.txt")), -1)
  // }

}
