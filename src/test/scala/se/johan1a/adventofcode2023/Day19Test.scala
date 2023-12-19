package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput
import se.johan1a.adventofcode2023.Day19._
import scala.concurrent.duration.Duration

class Day19Test extends munit.FunSuite {

  override val munitTimeout = Duration(30000, "min")

  test("Part 1 test") {
    assertEquals(Day19.part1(getInput("day19/test.txt")), 19114L)
  }

  test("Part 1") {
    assertEquals(Day19.part1(getInput("day19/input.txt")), 397061L)
  }

  test("Part 2 test0") {
    assertEquals(Day19.part2(getInput("day19/test0.txt"), 4), 80L)
  }

  test("Part 2 test") {
    assertEquals(Day19.part2(getInput("day19/test.txt")), 167409079868000L)
  }

  test("Part 2") {
    assertEquals(Day19.part2(getInput("day19/input.txt")), 125657431183201L)
  }

}
