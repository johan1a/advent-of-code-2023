package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils
import scala.collection.mutable.ArrayBuffer

object Day03 {

  def part1(input: Seq[String]): Int = {
    val grid = Utils.makeGrid(input)

    var sum = 0
    var i = 0
    var j = 0
    while (i < grid.size) {
      while (j < grid.head.size) {
        if (!grid(i)(j).isDigit) {
          j += 1
        } else {
          var end = j
          while (end < grid.size && grid(i)(end).isDigit) {
            end += 1
          }
          // number is [j, end] inclusive
          end -= 1
          sum += checkNumber(grid, i, j, end)
          j = end + 1
        }
      }
      i += 1
      j = 0
    }
    sum
  }

  def checkNumber(grid: ArrayBuffer[ArrayBuffer[Char]], numberRow: Int, numberColStart: Int, numberColEnd: Int): Int = {

    val adjacent = (numberColStart - 1).to(numberColEnd + 1).exists { col =>
      if (col >= 0 && col < grid.size) {
        val rowAbove = numberRow - 1
        val rowBelow = numberRow + 1
        (rowAbove >= 0 && !grid(rowAbove)(col).isDigit && grid(rowAbove)(col) != '.') ||
        (rowBelow < grid.head.size && !grid(rowBelow)(col).isDigit && grid(rowBelow)(col) != '.')
      } else {false}
    }
    if (adjacent) {
      return getNumber(grid, numberRow, numberColStart, numberColEnd)
    }

    val colBefore = numberColStart - 1
    if (colBefore >= 0 && !grid(numberRow)(colBefore).isDigit && grid(numberRow)(colBefore) != '.') {
      return getNumber(grid, numberRow, numberColStart, numberColEnd)
    }

    val colAfter = numberColEnd + 1
    if (colAfter < grid.head.size && !grid(numberRow)(colAfter).isDigit && grid(numberRow)(colAfter) != '.') {
      return getNumber(grid, numberRow, numberColStart, numberColEnd)
    }

    0
  }

  def getNumber(grid: ArrayBuffer[ArrayBuffer[Char]], numberRow: Int, numberColStart: Int, numberColEnd: Int): Int = {
    var result = ""
    numberColStart.to(numberColEnd).foreach { col =>
      result = result + grid(numberRow)(col).toString
    }
    result.toInt
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
