package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day23 {

  def part1(input: Seq[String]): Long = {
    val grid = makeGrid(input)
    val start = Vec2(1, 0)
    val target = Vec2(grid.head.size - 2, grid.size - 1)
    longest(grid, Set(start), start, target) - 1
  }

  def part2(input: Seq[String]): Long = {
    println("Start part2")
    val grid = makeGrid(input)
    val start = Vec2(1, 0)
    val target = Vec2(grid.head.size - 2, grid.size - 1)
    longest(grid, Set(start), start, target, slippery = false) - 1
  }

  def longest(grid: Grid, seen: Set[Vec2], pos: Vec2, target: Vec2, slippery: Boolean = true): Long = {
    if (pos == target) {
      println(s"at target after ${seen.size} steps")
      seen.size
    } else {
      val results = getNeighbors(grid, seen, pos, slippery).map { neighbor =>
        longest(grid, seen + neighbor, neighbor, target, slippery)
      }
      if (results.nonEmpty) {
        results.max
      } else {
        -1
      }
    }
  }

  def getNeighbors(grid: Grid, seen: Set[Vec2], pos: Vec2, slippery: Boolean): Seq[Vec2] = {
    (get(grid, pos) match {
      case '>' if slippery => Seq(pos + Vec2(1, 0))
      case '<' if slippery => Seq(pos + Vec2(-1, 0))
      case '^' if slippery => Seq(pos + Vec2(0, -1))
      case 'v' if slippery => Seq(pos + Vec2(0, 1))
      case _               => neighbors(pos, includeDiagonals = false)
    }).filter(p => inRange(grid, p) && !(get(grid, p) == '#') && !seen.contains(p))
  }

}
