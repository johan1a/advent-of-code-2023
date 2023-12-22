package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.TestInputUtil.getInput
import se.johan1a.adventofcode2023.Day22._
import se.johan1a.adventofcode2023.Utils._

class Day22Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day22.part1(getInput("day22/test.txt")), 5)
  }

  test("Part 1 test0") {
    assertEquals(Day22.part1(getInput("day22/test0.txt")), 3)
  }

  test("Part 1 test1") {
    assertEquals(Day22.part1(getInput("day22/test1.txt")), 2)
  }

  test("Part 1 test") {
    assertEquals(Day22.part1(getInput("day22/test2.txt")), 34)
  }

  test("Part 1") {
    assertEquals(Day22.part1(getInput("day22/input.txt")), 461)
  }

  test("intersects") {
    val a = Brick(Vec3(0, 0, 0), Vec3(1, 1, 1))
    val b = Brick(Vec3(0, 0, 0), Vec3(1, 1, 1))
    assertEquals(Day22.intersectsXY(a, b), true)
  }

  test("intersects") {
    val a = Brick(Vec3(0, 0, 0), Vec3(1, 1, 1))
    val b = Brick(Vec3(1, 0, 0), Vec3(2, 1, 1))
    assertEquals(Day22.intersectsXY(a, b), true)
  }

  test("intersects") {
    val a = Brick(Vec3(0, 0, 0), Vec3(1, 1, 1))
    val b = Brick(Vec3(0, 1, 0), Vec3(1, 2, 1))
    assertEquals(Day22.intersectsXY(a, b), true)
  }

  test("intersects") {
    val a = Brick(Vec3(0, 0, 0), Vec3(1, 1, 1))
    val b = Brick(Vec3(0, 0, 2), Vec3(1, 1, 4))
    assertEquals(Day22.intersectsXY(a, b), true)
  }

  test("fall") {
    val a = Brick(Vec3(0, 0, 1), Vec3(1, 1, 2))
    val b = Brick(Vec3(0, 0, 3), Vec3(1, 1, 5))

    val expected = Seq(
      Brick(Vec3(0, 0, 3), Vec3(1, 1, 5)), a
    )

    assertEquals(Day22.fall(Seq(a, b)), expected)
  }

  test("Part 2 test") {
    assertEquals(Day22.part2(getInput("day22/test.txt")), 7)
  }

  test("Part 2") {
    assertEquals(Day22.part2(getInput("day22/input.txt")), 74074)
  }

}
