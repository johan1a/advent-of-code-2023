package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput
import scala.concurrent.duration.Duration

class Day25Test extends munit.FunSuite {

  override val munitTimeout = Duration(5, "min")

  test("Part 1 test") {
    assertEquals(Day25.part1(getInput("day25/test.txt")), 54)
  }

  test("Part 1") {
    assertEquals(Day25.part1(getInput("day25/input.txt")), 543564)
  }

}
