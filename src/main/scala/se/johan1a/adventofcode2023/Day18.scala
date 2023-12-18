package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day18 {

  def part1(input: Seq[String]): Long = {
    val instructions = input.map { case s"$dir $n ($color)" => (dir, n.toInt) }
    calculateArea(instructions)
  }

  def part2(input: Seq[String]): Long = {
    val instructions = input
      .map { case s"$colorA $colorB ($instruction)" =>
        (toDir(instruction.takeRight(1)), Integer.parseInt(instruction.drop(1).take(5), 16))
      }
    calculateArea(instructions)
  }

  def calculateArea(instructions: Seq[(String, Int)]): Long = {
    var pos = Vec2(0, 0)
    var vertices = Seq(pos)
    var edgeSize = 0L
    instructions
      .foreach { case (dir, n) =>
        val b = move(pos, dir, n)
        vertices = vertices :+ b
        pos = b
        edgeSize += n
      }

    // Shoelace formula to calculate area
    var sum = 0L
    var j = vertices.size - 1
    0.until(vertices.size).foreach { i =>
      val a = vertices(j)
      val b = vertices(i)
      sum = sum + (a.x + b.x) * (a.y - b.y)
      j = i
    }
    val area = Math.abs(Math.abs(sum) / 2)

    // Pick's theorem:
    // A = I + E/2 - 1
    // I = A - E/2 + 1

    // ans = E + I
    // ans = E + A - E/2 + 1
    // ans = A + E/2 + 1
    area + edgeSize / 2 + 1
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
