package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day18 {

  def part1(input: Seq[String]): Int = {
    var pos = Vec2(0, 0)
    var edge = Set(pos)
    var sideA: Option[Vec2] = None
    var sideB: Option[Vec2] = None
    input.map { case s"$dir $n ($color)" => (dir, n.toInt) }.foreach { case (dir, n) =>
      0.until(n).foreach { _ =>
        pos = move(pos, dir)
        edge = edge + pos

        if (n > 4 && sideA.isEmpty) {
          val left = leftOf(dir)
          val right = rightOf(dir)
          sideA = Some(move(pos, left))
          sideB = Some(move(pos, right))
        }
      }
    }

    // val minY = edge.minBy(_.y)
    // val maxY = edge.maxBy(_.y)
    // val minX = edge.minBy(_.x)
    // val maxX = edge.maxBy(_.x)

    // minY.y.to(maxY.y).foreach { y =>
    //   minX.x.to(maxX.x).foreach { x=>
    //     if(edge.contains(Vec2(x,y))) {
    //       print("#")
    //     } else {
    //       print(" ")
    //     }
    //   }
    //   println()
    // }

    println(edge.size)
    println(sideA)
    println(sideB)
    val inside = flood(edge, sideA.get, sideB.get)

    inside.size + edge.size
  }

  def part2(input: Seq[String]): Long = {
    var pos = Vec2(0, 0)
    var edge = Set(pos)
    var sideA: Option[Vec2] = None
    var sideB: Option[Vec2] = None
    input
      .map { case s"$colorA $colorB ($instruction)" =>
      (Integer.parseInt(instruction.drop(1).take(5), 16), toDir(instruction.takeRight(1)))
      }
      .foreach { case (n, dir) =>
        0.until(n).foreach { _ =>
          pos = move(pos, dir)
          edge = edge + pos

          if (n > 4 && sideA.isEmpty) {
            val left = leftOf(dir)
            val right = rightOf(dir)
            sideA = Some(move(pos, left))
            sideB = Some(move(pos, right))
          }
        }
      }

    println(edge.size)
    println(sideA)
    println(sideB)
    val inside = flood(edge, sideA.get, sideB.get)

    inside.size + edge.size
    edge.size
  }

  def flood(edge: Set[Vec2], aStart: Vec2, bStart: Vec2) = {
    var setA = Set[Vec2]()
    var setB = Set[Vec2]()

    var qA = Set(aStart)
    var qB = Set(bStart)
    val maxIter = 10000000
    var i = 0
    while (i < maxIter && qA.nonEmpty && qB.nonEmpty) {
      val a = qA.head
      qA = qA.tail
      val b = qB.head
      qB = qB.tail
      setA = setA + a
      setB = setB + b
      // println(s"setA ${setA.size} qA ${qA.size} setB ${setB.size} qB ${qB.size}")

      qA = qA ++ neighbors(a, includeDiagonals = false).filterNot(p =>
        edge.contains(p) || setA.contains(p) || setB.contains(p) || qA.contains(p)
      )
      qB = qB ++ neighbors(b, includeDiagonals = false).filterNot(p =>
        edge.contains(p) || setA.contains(p) || setB.contains(p) || qB.contains(p)
      )
      i += 1
    }

    if(i==maxIter) {
      println("ERROR maxIter reached")
    }

    if (qA.isEmpty) {
      setA
    } else {
      setB
    }

  }

  def leftOf(dir: String) = {
    dir match {
      case "U" => "L"
      case "D" => "R"
      case "L" => "D"
      case "R" => "U"
    }
  }

  def rightOf(dir: String) = {
    dir match {
      case "U" => "R"
      case "D" => "L"
      case "L" => "U"
      case "R" => "D"
    }
  }

  def toDir(str: String) = {
    str match {
      case "0" => "R"
      case "1" => "D"
      case "2" => "L"
      case "3" => "U"
    }
  }

}
