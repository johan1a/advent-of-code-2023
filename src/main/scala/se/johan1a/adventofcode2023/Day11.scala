package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day11 {

  def part1(input: Seq[String], bonus: Int = 2): Long = {
    val galaxies: Seq[Vec2] = parse(input, bonus)
    val pairs = 0.until(galaxies.size).flatMap { i =>
      (i + 1).until(galaxies.size).map { j =>
        val a = galaxies(i)
        val b = galaxies(j)
        (a, b)
      }
    }
    val d = pairs.map { case (a, b) =>
      (a, b, Utils.manhattan((a._1, a._2), (b._1, b._2)))
    }
    d.map(_._3).sum
  }

  def part2(input: Seq[String], bonus: Int): Long = {
    part1(input, bonus)
  }

  def parse(input: Seq[String], bonus: Int) = {
    var emptyYs = Seq[Int]()
    input.zipWithIndex.foreach { case (row, y) =>
      var allEmpty = true
      row.zipWithIndex.foreach { case (char, x) =>
        if (char != '.') {
          allEmpty = false
        }
      }
      if (allEmpty) {
        emptyYs = emptyYs :+ y
      }
    }

    var emptyXs = Seq[Int]()
    input.head.indices.map { case x =>
      var allEmpty = true
      input.indices.map { case y =>
        val char = input(y)(x)
        if (char != '.') {
          allEmpty = false
        }
      }
      if (allEmpty) {
        emptyXs = emptyXs :+ x
      }
    }

    var bonusY = 0

    input.zipWithIndex.flatMap { case (row, y) =>
      if (emptyYs.contains(y)) {
        bonusY += (bonus -1)
      }
      var bonusX = 0
      val result = row.zipWithIndex
        .map { case (char, x) =>
          if (emptyXs.contains(x)) {
            bonusX += (bonus-1)
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
