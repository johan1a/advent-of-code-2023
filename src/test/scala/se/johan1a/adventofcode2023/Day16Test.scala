package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput
import scala.concurrent.duration.Duration

class Day16Test extends munit.FunSuite {

  override val munitTimeout = Duration(30000, "min")

  test("Part 1 test") {
    assertEquals(Day16.part1(getInput("day16/test.txt")), 46)
  }

  test("Part 1") {
    assertEquals(Day16.part1(getInput("day16/input.txt")), 7939)
  }

  test("Part 2 test") {
    assertEquals(Day16.part2(getInput("day16/test.txt")), 51)
  }

  test("Part 2") {
    assertEquals(Day16.part2(getInput("day16/input.txt")), 125657431183201L)
  }

}
