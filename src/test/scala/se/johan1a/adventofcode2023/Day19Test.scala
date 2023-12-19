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

  test("sort") {
    val a = Part2(
      Map(
        "x" -> (2001L, 4000),
        "m" -> (1L, 4000),
        "a" -> (1L, 4000),
        "s" -> (1L, 4000)
      )
    )
    val b = Part2(
      Map(
        "x" -> (1L, 2000),
        "m" -> (1L, 4000),
        "a" -> (1L, 4000),
        "s" -> (1L, 4000)
      )
    )
    assertEquals(Day19.sort(Seq(a, b)), Seq(b, a))
  }

  test("Combine") {
    val a = Part2(
      Map(
        "x" -> (1L, 4000),
        "m" -> (1L, 4000),
        "a" -> (1L, 4000),
        "s" -> (1L, 4000)
      )
    )
    val b = Part2(
      Map(
        "x" -> (1L, 4000),
        "m" -> (1L, 4000),
        "a" -> (1L, 4000),
        "s" -> (1L, 4000)
      )
    )
    assertEquals(Day19.combine(Seq(a, b)), Set(a))
  }

  test("Combine 2") {
    val a = Part2(
      Map(
        "x" -> (1L, 2000),
        "m" -> (1L, 4000),
        "a" -> (1L, 4000),
        "s" -> (1L, 4000)
      )
    )
    val b = Part2(
      Map(
        "x" -> (1L, 4000),
        "m" -> (1L, 4000),
        "a" -> (1L, 4000),
        "s" -> (1L, 4000)
      )
    )
    val expected = Set(
      Part2(
        Map(
          "x" -> (1L, 2000),
          "m" -> (1L, 4000),
          "a" -> (1L, 4000),
          "s" -> (1L, 4000)
        )
      ),
      Part2(
        Map(
          "x" -> (2001L, 4000),
          "m" -> (1L, 4000),
          "a" -> (1L, 4000),
          "s" -> (1L, 4000)
        )
      )
    )
    assertEquals(Day19.combine(Seq(a, b)), expected)
  }

  test("Combine 2") {
    val a = Part2(
      Map(
        "x" -> (1L, 2000),
        "m" -> (1L, 4000),
        "a" -> (1L, 4000),
        "s" -> (1L, 4000)
      )
    )
    val b = Part2(
      Map(
        "x" -> (1L, 4000),
        "m" -> (2000L, 4000),
        "a" -> (1L, 4000),
        "s" -> (1L, 4000)
      )
    )
    val expected = Set(
      Part2(
        Map(
          "x" -> (1L, 2000),
          "m" -> (1L, 1999),
          "a" -> (1L, 4000),
          "s" -> (1L, 4000)
        )
      ),
      Part2(
        Map(
          "x" -> (1L, 2000),
          "m" -> (2000L, 4000),
          "a" -> (1L, 4000),
          "s" -> (1L, 4000)
        )
      ),
      Part2(
        Map(
          "x" -> (2001L, 4000),
          "m" -> (2000L, 4000),
          "a" -> (1L, 4000),
          "s" -> (1L, 4000)
        )
      )
    )
    assertEquals(Day19.combine(Seq(a, b)), expected)
  }

  test("Part 2 test0") {
    assertEquals(Day19.part2(getInput("day19/test0.txt"), 4), 80L)
  }

  test("Part 2 test") {
    assertEquals(Day19.part2(getInput("day19/test.txt")), 167409079868000L)
  }

  test("Part 2") {
    assertEquals(Day19.part2(getInput("day19/input.txt")), -1L)
  }

}
