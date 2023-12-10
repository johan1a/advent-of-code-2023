package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

import scala.collection.mutable.ArrayBuffer

object Day10 {

  def part1(input: Seq[String]): Int = {
    val grid = Utils.makeGrid(input)
    val start = findStart(grid)
    maxDist(grid, start)._2
  }

  def part2(input: Seq[String]): Int = {
    val grid = Utils.makeGrid(input)
    val doubled = double(grid)
    val start = findStart(doubled)
    val loop = maxDist(doubled, start)._1
    if (grid.size < 40) {
      printGrid(grid)
      printGrid(doubled)
    }
    val result = countContained(doubled, loop)
    if (grid.size < 40) {
      printGrid(doubled)
    }
    result
  }

  def printGrid(grid: ArrayBuffer[ArrayBuffer[Char]]) = {
    grid.foreach { line =>
      println(line.mkString(""))
    }
  }

  def countContained(grid: ArrayBuffer[ArrayBuffer[Char]], loop: Set[Vec2]): Int = {
    var outside = Set[Vec2]()
    var inside = Set[Vec2]()
    0.until(grid.size).foreach { y =>
      0.until(grid.head.size).foreach { x =>
        val pos = Vec2(x, y)
        if (!outside.contains(pos) && !inside.contains(pos) && !loop.contains(pos)) {

          var isInside = true
          var pending = Set[Vec2]()
          var queue = Seq[Vec2](pos)
          while (queue.nonEmpty) {
            val curr = queue.head
            queue = queue.tail
            if (!pending.contains(curr)) {
              if (!loop.contains(curr)) {
                pending += curr
                grid(curr.y.toInt)(curr.x.toInt) = 'P'
                neighbors(curr, includeDiagonals = true).foreach { case neighbor =>
                  if (
                    outside.contains(
                      neighbor
                    ) || neighbor.y < 0 || neighbor.x < 0 || neighbor.y >= grid.size || neighbor.x >= grid.head.size
                  ) {
                    isInside = false
                  } else if (!pending.contains(neighbor) && !loop.contains(neighbor)) {
                    queue = queue :+ neighbor
                  }
                }
              }
            }
          }
          if (isInside) {
            inside ++= pending
            pending.foreach { pos =>
              grid(pos.y.toInt)(pos.x.toInt) = 'I'
            }
          } else {
            pending.foreach { pos =>
              grid(pos.y.toInt)(pos.x.toInt) = ' '
            }
            outside ++= pending
          }
        }
      }
    }
    inside.filter(pos => pos.x % 2 == 0 && pos.y % 2 == 0).size
  }

  def isInside(grid: ArrayBuffer[ArrayBuffer[Char]], loop: Set[Vec2], outside: Set[Vec2], pos: Vec2) = {}

  def double(grid: ArrayBuffer[ArrayBuffer[Char]]): ArrayBuffer[ArrayBuffer[Char]] = {
    val result = ArrayBuffer.fill(grid.size * 2)(ArrayBuffer.fill(grid.head.size * 2)('?'))

    0.until(grid.size).foreach { y =>
      0.until(grid.head.size).foreach { x =>
        result(2 * y)(2 * x) = get(grid, Vec2(x, y))
        result(2 * y + 1)(2 * x + 1) = '.'

        if (x < grid.head.size - 1) {
          result(2 * y)(2 * x + 2) = get(grid, Vec2(x + 1, y))
        }
        if (x < grid.head.size - 1 && compatible(grid, Seq(Vec2(x, y), Vec2(x + 1, y)))) {
          result(2 * y)(2 * x + 1) = '-'
        } else {
          result(2 * y)(2 * x + 1) = '.'
        }

        if (y < grid.size - 1) {
          result(2 * y + 2)(2 * x) = get(grid, Vec2(x, y + 1))
        }
        if (y < grid.size - 1 && compatible(grid, Seq(Vec2(x, y), Vec2(x, y + 1)))) {
          result(2 * y + 1)(2 * x) = '|'
        } else {
          result(2 * y + 1)(2 * x) = '.'
        }

      }
    }

    result
  }

  def maxDist(grid: ArrayBuffer[ArrayBuffer[Char]], start: Vec2): (Set[Vec2], Int) = {
    var seen = Set[Vec2]()
    var queue = Seq((start, 0))

    var maxDist = 0

    while (queue.nonEmpty) {
      val (curr, dist) = queue.head
      seen = seen + curr
      if (dist > maxDist) {
        maxDist = dist
      }
      queue = queue.tail

      getNeighbors(grid, curr).foreach { neighbor =>
        if (!seen.contains(neighbor)) {
          queue = queue :+ (neighbor, dist + 1)
        }
      }
    }

    (seen, maxDist)
  }

  def getNeighbors(grid: ArrayBuffer[ArrayBuffer[Char]], pos: Vec2): Seq[Vec2] = {

    val min = Vec2(0, 0)
    val max = getMax(grid)
    val diagonals = false
    neighbors(pos, min, max, diagonals).filter { case neighbor =>
      compatible(grid, Seq(pos, neighbor).sortBy(p => (p.x, p.y)))
    }
  }

  def compatible(grid: ArrayBuffer[ArrayBuffer[Char]], positions: Seq[Vec2]): Boolean = {

    val a = positions.head
    val b = positions.last
    if (a.y == b.y) {
      // a left of b
      val x = Set(
        'S',
        '-',
        'L',
        'F'
      ).contains(get(grid, a)) &&
        Set(
          'S',
          '-',
          'J',
          '7'
        ).contains(get(grid, b))

      x
    } else if (a.x == b.x) {
      // a above b
      val x = Set(
        'S',
        '|',
        '7',
        'F'
      ).contains(get(grid, a)) &&
        Set(
          'S',
          '|',
          'L',
          'J'
        ).contains(get(grid, b))
      x
    } else {
      ???
    }
  }

  def get(grid: ArrayBuffer[ArrayBuffer[Char]], pos: Vec2) = {
    grid(pos.y.toInt)(pos.x.toInt)
  }

  def findStart(grid: ArrayBuffer[ArrayBuffer[Char]]): Vec2 = {
    grid.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex
        .map { case (char, j) =>
          if (char == 'S') {
            Some(Vec2(j, i))
          } else {
            None
          }
        }
        .collect { case Some(value) => value }
    }.head
  }

}
