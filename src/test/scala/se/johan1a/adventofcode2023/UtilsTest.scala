package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.collection.mutable.ArrayBuffer

class UtilsTest extends munit.FunSuite {

  test("Vec2 + Vec2") {
    assertEquals(Vec2(2, 3) + Vec2(1, 5), Vec2(3, 8))
  }

  test("Vec2 - Vec2") {
    assertEquals(Vec2(2, 3) - Vec2(1, 5), Vec2(1, -2))
  }

  test("rotate grid") {
    assertEquals(
      rotate(
        ArrayBuffer(
          ArrayBuffer('0', '1', '2', '3'),
          ArrayBuffer('4', '5', '6', '7'),
          ArrayBuffer('8', '9', 'a', 'b'),
          ArrayBuffer('c', 'd', 'e', 'f')
        )
      ),
      ArrayBuffer(
        ArrayBuffer('c', '8', '4', '0'),
        ArrayBuffer('d', '9', '5', '1'),
        ArrayBuffer('e', 'a', '6', '2'),
        ArrayBuffer('f', 'b', '7', '3')
      )
    )
  }

  test("flip grid") {
    assertEquals(
      flip(
        ArrayBuffer(
          ArrayBuffer('0', '1', '2', '3'),
          ArrayBuffer('4', '5', '6', '7'),
          ArrayBuffer('8', '9', 'a', 'b'),
          ArrayBuffer('c', 'd', 'e', 'f')
        )
      ),
      ArrayBuffer(
        ArrayBuffer('3', '2', '1', '0'),
        ArrayBuffer('7', '6', '5', '4'),
        ArrayBuffer('b', 'a', '9', '8'),
        ArrayBuffer('f', 'e', 'd', 'c')
      )
    )
  }

}
