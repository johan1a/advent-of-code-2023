package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day11Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day11.part1(getInput("day11/test.txt")), 374L)
  }

  test("Part 1") {
    assertEquals(Day11.part1(getInput("day11/input.txt")), 9681886L)
  }

  test("Part 2 test0") {
    assertEquals(Day11.part2(getInput("day11/test.txt"), 10), 1030L)
  }

  test("Part 2 test") {
    assertEquals(Day11.part2(getInput("day11/test.txt"), 100), 8410L)
  }

  test("Part 2") {
    assertEquals(Day11.part2(getInput("day11/input.txt"), 1000000), 791134099634L)
  }

}
