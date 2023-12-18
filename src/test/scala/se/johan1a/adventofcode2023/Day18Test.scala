package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day18Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day18.part1(getInput("day18/test.txt")), 62)
  }

  test("Part 1") {
    assertEquals(Day18.part1(getInput("day18/input.txt")), 47139)
  }

  test("Part 2 test") {
    assertEquals(Day18.part2(getInput("day18/test.txt")), 952408144115L)
  }

//   test("Part 2") {
//     assertEquals(Day18.part2(getInput("day18/input.txt")), -1L)
//   }

}
