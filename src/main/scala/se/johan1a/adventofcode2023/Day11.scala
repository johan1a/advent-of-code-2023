package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day11 {

  def part1(input: Seq[String], bonus: Int = 2): Long = {
    val galaxies = parse(input, bonus)
    pairs(galaxies).map { case (a, b) =>
      manhattan(a, b)
    }.sum
  }

  def part2(input: Seq[String], bonus: Int): Long = {
    part1(input, bonus)
  }

  def parse(input: Seq[String], bonus: Int) = {
    val emptyYs = input.indices.filter { y =>
      input.head.indices.forall { x =>
        input(y)(x) == '.'
      }
    }
    val emptyXs = input.head.indices.filter { x =>
      input.indices.forall { y =>
        input(y)(x) == '.'
      }
    }

    var bonusY = 0
    input.zipWithIndex.flatMap { case (row, y) =>
      if (emptyYs.contains(y)) {
        bonusY += (bonus - 1)
      }
      var bonusX = 0
      val result = row.zipWithIndex
        .map { case (char, x) =>
          if (emptyXs.contains(x)) {
            bonusX += (bonus - 1)
          }
          if (char != '.') {
            Some(Vec2(x + bonusX, y + bonusY))
          } else {
            None
          }
        }
        .collect { case Some(p) => p }
      result
    }.toSeq
  }
}
