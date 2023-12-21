package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day21 {

  def part1(input: Seq[String], n: Int): Long = {
    val grid = makeGrid(input)
    val start: Vec2 = positions(grid).find(p => get(grid, p) == 'S').get
    count(grid, start, n)
  }

  def count(grid: Grid, start: Vec2, n: Int) = {
    var queue = Seq((start, 0))
    var nbrAtEnd = 0
    var seen = Set[(Vec2, Int)]()
    while (queue.nonEmpty) {
      val (curr, stepsTaken) = queue.head
      queue = queue.tail
      if (!seen.contains((curr, stepsTaken))) {
        seen = seen + ((curr, stepsTaken))

        if (stepsTaken == n) {
          nbrAtEnd += 1
        } else {
          queue = queue ++ neighbors(curr, includeDiagonals = false)
            .filter { p =>
              inRange(grid, p) && get(grid, p) != '#'
            }
            .map(p => (p, stepsTaken + 1))
        }
      }
    }
    nbrAtEnd
  }

  def part2(input: Seq[String]): Long = {
    -1
  }
}
