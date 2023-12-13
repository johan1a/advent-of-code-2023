package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.collection.mutable.ArrayBuffer

object Day13 {

  sealed trait Result {
    def score: Int
  }
  case class RowMatch(row: Int) extends Result {
    override def score = 100 * (row + 1)
  }
  case class ColMatch(col: Int) extends Result {
    override def score = col + 1
  }

  def part1(input: Seq[String]): Int = {
    split(input).map(makeGrid).map(grid => findMirror(grid)).map(_.score).sum
  }

  def part2(input: Seq[String]): Int = {
    split(input).map(makeGrid).map(findMirror2).map(_.score).sum
  }

  def findMirror(grid: ArrayBuffer[ArrayBuffer[Char]], tolerance: Int = 0): Result = {
    val fromTop: Option[Int] = 0.until(grid.size - 1).find { row =>
      mirrorAtRow(grid, row, tolerance)
    }
    fromTop match {
      case Some(row) =>
        RowMatch(row)
      case None =>
        0.until(grid.head.size - 1).find { col =>
          mirrorAtCol(grid, col, tolerance)
        } match {
          case Some(col) =>
            ColMatch(col)
          case None => throw new Exception("none found")
        }
    }
  }

  def findMirror2(grid: ArrayBuffer[ArrayBuffer[Char]]): Result = {
    val tolerance = 1
    val originalMirror = findMirror(grid)

    val fromTop: Option[Int] = 0.until(grid.size - 1).find { row =>
      originalMirror != RowMatch(row) && mirrorAtRow(grid, row, tolerance)
    }
    fromTop match {
      case Some(row) =>
        RowMatch(row)
      case None =>
        0.until(grid.head.size - 1).find { col =>
          originalMirror != ColMatch(col) && mirrorAtCol(grid, col, tolerance)
        } match {
          case Some(col) =>
            ColMatch(col)
          case None => throw new Exception("none found")
        }
    }
  }

  def mirrorAtCol(grid: ArrayBuffer[ArrayBuffer[Char]], col: Int, tolerance: Int): Boolean = {
    var col0 = col
    var col1 = col + 1
    var diffs = 0
    while (col0 >= 0 && col1 < grid.head.size) {
      diffs += 0
        .until(grid.size)
        .filter { row =>
          grid(row)(col0) != grid(row)(col1)
        }
        .size
      if (diffs > tolerance) {
        return false
      }
      col0 -= 1
      col1 += 1
    }
    true
  }

  def mirrorAtRow(grid: ArrayBuffer[ArrayBuffer[Char]], row: Int, tolerance: Int): Boolean = {
    var row0 = row
    var row1 = row + 1
    var diffs = 0
    while (row0 >= 0 && row1 < grid.size) {
      diffs += 0
        .until(grid.head.size)
        .filter { col =>
          grid(row0)(col) != grid(row1)(col)
        }
        .size
      if (diffs > tolerance) {
        return false
      }
      row0 -= 1
      row1 += 1
    }
    true
  }

}
