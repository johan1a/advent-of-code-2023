package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day07Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day07.part1(getInput("day07/test.txt")), 6440)
  }

  test("Part 1") {
    assertEquals(Day07.part1(getInput("day07/input.txt")), 248836197)
  }

  // test("Part 2") {
  //   assertEquals(Day07.part2(getInput("day07/input.txt")), -1)
  // }

}
