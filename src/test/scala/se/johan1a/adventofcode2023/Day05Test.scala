package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput
import se.johan1a.adventofcode2023.Day05.Range

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

  test("Part 2 test merge") {
    val input = Seq(Range(1, 14, 0), Range(3, 5, 0), Range(16, 18, 0), Range(18, 24, 0))
    val expected = Seq(Range(1, 14, 0), Range(16, 24, 0))
    assertEquals(Day05.mergeRanges(input), expected)
  }

  test("Part 2") {
    assertEquals(Day05.part2(getInput("day05/input.txt")), 1493866)
  }

}
