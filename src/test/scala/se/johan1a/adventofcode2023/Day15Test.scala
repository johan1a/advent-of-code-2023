package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day15Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day15.part1(Seq("HASH")), 52)
  }

  test("Part 1") {
    assertEquals(Day15.part1(getInput("day15/input.txt")), 5077691)
  }

  // test("Part 2") {
  //   assertEquals(Day15.part2(getInput("day15/input.txt")), -1)
  // }

}
