package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput
import scala.concurrent.duration.Duration

class Day24Test extends munit.FunSuite {

  override val munitTimeout = Duration(5, "min")

  test("Part 1 test") {
    assertEquals(Day24.part1(getInput("day24/test.txt"), 7, 27), 2L)
  }

  test("Part 1") {
    assertEquals(Day24.part1(getInput("day24/input.txt")), 21679L)
  }

   test("Part 2 test") {
     assertEquals(Day24.part2(getInput("day24/test.txt")), 47L)
   }

  test("Part 2") {
    assertEquals(Day24.part2(getInput("day24/input.txt"), 300), 566914635762564L)
  }

}
