package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.collection.mutable.ArrayBuffer

object Day13 {

  def part1(input: Seq[String]): Int = {
    split(input).map(makeGrid).map(findMirror).sum
  }

  def findMirror(grid: ArrayBuffer[ArrayBuffer[Char]]): Int = {
    val fromTop: Option[Int] = 0.until(grid.size - 1).find { row =>
      mirrorAtRow(grid, row)
    }
    fromTop match {
      case Some(row) =>
        println(s"found row $row")
        100 * (row + 1)
      case None =>
        0.until(grid.head.size - 1).find { col =>
          mirrorAtCol(grid, col)
        } match {
          case Some(col) =>
            println(s"found col $col")
            col + 1
          case None => throw new Exception("none found")
        }
    }
  }

  def mirrorAtCol(grid: ArrayBuffer[ArrayBuffer[Char]], col: Int): Boolean = {
    var col0 = col
    var col1 = col + 1
    while (col0 >= 0 && col1 < grid.head.size) {
      val equal = 0.until(grid.size).forall { row =>
        grid(row)(col0) == grid(row)(col1)
      }
      if (!equal) {
        return false
      }
      col0 -= 1
      col1 += 1
    }
    true
  }

  def mirrorAtRow(grid: ArrayBuffer[ArrayBuffer[Char]], row: Int): Boolean = {
    var row0 = row
    var row1 = row + 1
    while (row0 >= 0 && row1 < grid.size) {
      val equal = 0.until(grid.head.size).forall { col =>
        grid(row0)(col) == grid(row1)(col)
      }
      if (!equal) {
        return false
      }
      row0 -= 1
      row1 += 1
    }
    true
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
