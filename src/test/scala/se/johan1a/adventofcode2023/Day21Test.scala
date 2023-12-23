package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput
import scala.concurrent.duration.Duration

class Day21Test extends munit.FunSuite {

  override val munitTimeout = Duration(5, "min")

  test("Part 1 test") {
    assertEquals(Day21.part1(getInput("day21/test.txt"), 6), 16L)
  }

  test("Part 1") {
    assertEquals(Day21.part1(getInput("day21/input.txt"), 64), 3809L)
  }

  test("Part 2") {
    assertEquals(Day21.part2(getInput("day21/input.txt"), 26501365), 629720570456311L)
  }

}
