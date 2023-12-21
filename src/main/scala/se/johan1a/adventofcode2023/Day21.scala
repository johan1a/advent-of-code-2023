package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day21 {

  def part1(input: Seq[String], n: Int): Long = {
    val grid = makeGrid(input)
    val start: Vec2 = positions(grid).find(p => get(grid, p) == 'S').get
    count(grid, start, n)
  }

  def count(grid: Grid, start: Vec2, n: Int) = {
    var queue = Set(start)
    var nextQueue = Set[Vec2]()
    var nbrReached = 0
    var nbrSteps = 0
    while (queue.nonEmpty && nbrSteps < n) {
      val curr = queue.head
      queue = queue.tail

      nextQueue = nextQueue ++ neighbors(curr, includeDiagonals = false)
        .filter(p => inRange(grid, p) && get(grid, p) != '#')

      if (queue.isEmpty) {
        nbrSteps += 1
        nbrReached = nextQueue.size
        queue = nextQueue
        nextQueue = Set.empty
      }
    }
    nbrReached
  }

  def part2(input: Seq[String], n: Int): Long = {
    val grid = makeGrid(input)
    val start: Vec2 = positions(grid).find(p => get(grid, p) == 'S').get
    count2(grid, start, n)
  }

  def count2(grid: Grid, start: Vec2, n: Int) = {
    var queue = Set(start)
    var nextQueue = Set[Vec2]()
    var nbrAtEnd = 0
    var nbrSteps = 0
    var nbrReached = 1

    // Start at m steps, then solve the quadratic equation:
    // f(x) for x : (m + x * width)
    val m = n % grid.size

    // it repeats every width, starting from m
    // f(m + k * width) = f(x)
    // f(x) = a*x^2 + b*x + c
    // calculate:
    // f(0), f(1), f(2)
    // then solve the quadratic equation
    var values = Seq[Long]()
    while (queue.nonEmpty && values.size < 3) {
      val curr = queue.head
      queue = queue.tail

      if (nbrSteps == n) {
        nbrAtEnd += 1
      } else {
        nextQueue = nextQueue ++ neighbors(curr, includeDiagonals = false)
          .filter { p =>
            var x = p.x
            var y = p.y
            if (x < 0 && x % grid.head.size == 0) {
              x = 0
            } else if (x < 0) {
              x = grid.head.size + (x % grid.head.size)
            } else {
              x = x % grid.head.size
            }
            if (y < 0 && y % grid.size == 0) {
              y = 0
            } else if (y < 0) {
              y = grid.size + (y % grid.size)
            } else {
              y = y % grid.size
            }
            val adjustedPos = Vec2(x, y)
            get(grid, adjustedPos) != '#'
          }
      }

      if (queue.isEmpty) {
        nbrSteps += 1
        nbrReached = nextQueue.size
        if ((nbrSteps - m) % (grid.size) == 0) {
          values = values :+ nbrReached
        }
        queue = nextQueue
        nextQueue = Set()
      }
    }

    val y0 = values(0)
    val y1 = values(1)
    val y2 = values(2)

    // f(x) = a*x^2 + b*x + c
    // f(0) = a*0^2 + b*0 + c
    // f(0) = c
    val c = y0

    // f(1) = a*1^2 + b*1 + c
    // y1 = a + b + c
    // a = y1 - b - c

    // f(2) = a*2^2 + b*2 + c
    // y2 = a*4 + b*2 + c
    // y2 - 4a - c = b*2
    // b = (y2 - 4a - c)/2
    // b = (y2 - 4(y1 - b - c) - c)/2
    // b = (y2 - 4y1 + 4b + 4c - c)/2
    // b = (y2 - 4y1 + 4b + 3c)/2
    // b = 1/2*y2 - 2*y1 + 2*b + 3*c/2
    // 0 = 1/2*y2 - 2y1 + b + 3*c/2
    // -b = (y2)/2 - 2y1 + 3*c/2
    // b = 2*y1-(y2)/2-3*c/2

    val b = (2 * y1 - y2 / 2.0 - 3 / 2.0 * c).toLong
    val a = y1 - b - c

    val x = n / grid.size
    a * x * x + b * x + c
  }
}
// a = 15387, b = 15488, c = 3911
