package se.johan1a.adventofcode2023

import scala.collection.mutable.ArrayBuffer

object Day03 {

  def part1(input: Seq[String]): Int = {
    val grid = Utils.makeGrid(input)

    var sum = 0
    var row = 0
    var col = 0
    while (row < grid.size) {
      while (col < grid.head.size) {
        if (!grid(row)(col).isDigit) {
          col += 1
        } else {
          var end = col
          while (end < grid.size && grid(row)(end).isDigit) {
            end += 1
          }
          // number is columns [col, end] inclusive
          end -= 1
          sum += checkNumber(grid, row, col, end)
          col = end + 1
        }
      }
      row += 1
      col = 0
    }
    sum
  }

  def part2(input: Seq[String]): Int = {
    val grid = Utils.makeGrid(input)

    var numbersAdjacentToGears = Map[(Int, Int), Seq[Int]]()

    var row = 0
    var col = 0
    while (row < grid.size) {
      while (col < grid.head.size) {
        if (!grid(row)(col).isDigit) {
          col += 1
        } else {
          var end = col
          while (end < grid.size && grid(row)(end).isDigit) {
            end += 1
          }
          // number is columns [col, end] inclusive
          end -= 1

          val adjacentGearPositions = getAdjacentGears(grid, row, col, end)

          val number = getNumber(grid, row, col, end)
          adjacentGearPositions.foreach { gearPos =>
            val currentAdjacentNumbers = numbersAdjacentToGears.getOrElse(gearPos, Seq.empty)
            numbersAdjacentToGears = numbersAdjacentToGears + (gearPos -> (currentAdjacentNumbers :+ number))
          }

          col = end + 1
        }
      }
      row += 1
      col = 0
    }

    numbersAdjacentToGears.map { case (_, numbers) =>
      if (numbers.size == 2) {
        numbers.product
      } else {
        0
      }
    }.sum
  }

  def checkNumber(grid: ArrayBuffer[ArrayBuffer[Char]], numberRow: Int, numberColStart: Int, numberColEnd: Int): Int = {
    val number = getNumber(grid, numberRow, numberColStart, numberColEnd)

    def isSymbol = (row: Int, col: Int) =>
      Utils.inRange(grid, row, col) && !grid(row)(col).isDigit && grid(row)(col) != '.'

    val hasSymbolAboveOrBelow = (numberColStart - 1).to(numberColEnd + 1).exists { col =>
      val rowAbove = numberRow - 1
      val rowBelow = numberRow + 1
      isSymbol(rowAbove, col) || isSymbol(rowBelow, col)
    }

    val colBefore = numberColStart - 1
    val colAfter = numberColEnd + 1
    if (hasSymbolAboveOrBelow || isSymbol(numberRow, colBefore) || isSymbol(numberRow, colAfter)) {
      number
    } else {
      0
    }
  }

  def getAdjacentGears(
      grid: ArrayBuffer[ArrayBuffer[Char]],
      numberRow: Int,
      numberColStart: Int,
      numberColEnd: Int
  ): Seq[(Int, Int)] = {

    def isGear = (row: Int, col: Int) => Utils.inRange(grid, row, col) && grid(row)(col) == '*'

    val gearsAboveAndBelow: Seq[(Int, Int)] = (numberColStart - 1)
      .to(numberColEnd + 1)
      .map { col =>
        val rowAbove = numberRow - 1
        val rowBelow = numberRow + 1
        if (isGear(rowAbove, col)) {
          Some((rowAbove, col))
        } else if (isGear(rowBelow, col)) {
          Some((rowBelow, col))
        } else {
          None
        }
      }
      .collect { case Some(pos) => pos }

    var allGears = gearsAboveAndBelow

    val colBefore = numberColStart - 1
    if (isGear(numberRow, colBefore)) {
      allGears = allGears :+ (numberRow, colBefore)
    }

    val colAfter = numberColEnd + 1
    if (isGear(numberRow, colAfter)) {
      allGears = allGears :+ (numberRow, colAfter)
    }

    allGears
  }

  def getNumber(grid: ArrayBuffer[ArrayBuffer[Char]], numberRow: Int, numberColStart: Int, numberColEnd: Int): Int = {
    var result = ""
    numberColStart.to(numberColEnd).foreach { col =>
      result = result + grid(numberRow)(col).toString
    }
    result.toInt
  }

}
