package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput

class Day10Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day10.part1(getInput("day10/test.txt")), 4)
  }

  test("Part 1 test1") {
    assertEquals(Day10.part1(getInput("day10/test1.txt")), 4)
  }

  test("Part 1") {
    assertEquals(Day10.part1(getInput("day10/input.txt")), 7145)
  }

  test("Part 2 test") {
    assertEquals(Day10.part2(getInput("day10/test.txt")), 1)
  }

  test("Part 2 test2") {
    assertEquals(Day10.part2(getInput("day10/test2.txt")), 4)
  }

  test("Part 2 test3") {
    assertEquals(Day10.part2(getInput("day10/test3.txt")), 8)
  }

  test("Part 2") {
    assertEquals(Day10.part2(getInput("day10/input.txt")), 445)
  }

}
