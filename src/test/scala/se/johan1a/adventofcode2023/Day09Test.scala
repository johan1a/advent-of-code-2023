package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day09Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day09.part1(getInput("day09/test.txt")), 114L)
  }

  test("Part 1") {
    assertEquals(Day09.part1(getInput("day09/input.txt")), 1584748274L)
  }

  test("Part 2 test") {
    assertEquals(Day09.part2(getInput("day09/input.txt")), 2)
  }

  // test("Part 2") {
  //   assertEquals(Day09.part2(getInput("day09/input.txt")), -1)
  // }

}
