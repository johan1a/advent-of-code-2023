package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day05Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day05.part1(getInput("day05/test.txt")), 35)
  }

  test("Part 1") {
    assertEquals(Day05.part1(getInput("day05/input.txt")), 174137457)
  }

  test("Part 2 test") {
    assertEquals(Day05.part2(getInput("day05/test.txt")), 46)
  }

  // test("Part 2") {
  //   assertEquals(Day05.part2(getInput("day05/input.txt")), 104054)
  // }

}
