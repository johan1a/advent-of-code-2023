package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day20Test extends munit.FunSuite {

  test("Part 1 test 0") {
    assertEquals(Day20.part1(getInput("day20/test0.txt")), 32000000L)
  }

  test("Part 1 test 1") {
    assertEquals(Day20.part1(getInput("day20/test1.txt")), 11687500L)
  }

  test("Part 1") {
    assertEquals(Day20.part1(getInput("day20/input.txt")), 980457412L)
  }

  test("Part 2") {
    assertEquals(Day20.part2(getInput("day20/input.txt")), -1L)
  }

}
