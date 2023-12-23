package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day23 {

  def part1(input: Seq[String]): Long = {
    val grid = makeGrid(input)
    val start = Vec2(1, 0)
    val target = Vec2(grid.head.size - 2, grid.size - 1)
    longest(grid, Set(start), start, target) - 1
  }

  def longest(grid: Grid, seen: Set[Vec2], pos: Vec2, target: Vec2): Long = {
    if (pos == target) {
      println(s"at target after ${seen.size} steps")
      seen.size
    } else {
      val results = getNeighbors(grid, seen, pos).map { neighbor =>
        longest(grid, seen + neighbor, neighbor, target)
      }
      if(results.nonEmpty) {
        results.max
      } else {
        -1
      }
    }
  }
  def getNeighbors(grid: Grid, seen: Set[Vec2], pos: Vec2): Seq[Vec2] = {
    (get(grid, pos) match {
      case '>' => Seq(pos + Vec2(1, 0))
      case '<' => Seq(pos + Vec2(-1, 0))
      case '^' => Seq(pos + Vec2(0, -1))
      case 'v' => Seq(pos + Vec2(0, 1))
      case _ => neighbors(pos, includeDiagonals = false)
    }).filter(p => inRange(grid, p) && !(get(grid, p) == '#') && !seen.contains(p))
  }

  def part2(input: Seq[String]): Long = {
    -1
  }
}
