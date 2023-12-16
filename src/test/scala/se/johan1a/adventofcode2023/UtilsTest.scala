package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

class UtilsTest extends munit.FunSuite {

  test("Vec2 + Vec2") {
    assertEquals(Vec2(2, 3) + Vec2(1, 5), Vec2(3, 8))
  }

  test("Vec2 - Vec2") {
    assertEquals(Vec2(2, 3) - Vec2(1, 5), Vec2(1, -2))
  }

}
