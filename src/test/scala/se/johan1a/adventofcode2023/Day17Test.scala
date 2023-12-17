package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day17Test extends munit.FunSuite {

  test("Part 1 test0") {
    assertEquals(Day17.part1(getInput("day17/test0.txt")), 9)
  }

  test("Part 1 test") {
    assertEquals(Day17.part1(getInput("day17/test.txt")), 102)
  }

  test("Part 1") {
    assertEquals(Day17.part1(getInput("day17/input.txt")), 843)
  }

   test("Part 2 test") {
     assertEquals(Day17.part2(getInput("day17/test1.txt")), 71)
   }

  test("Part 2") {
    assertEquals(Day17.part2(getInput("day17/input.txt")), 1017)
  }

}
