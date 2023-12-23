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
    val grid = makeGrid(input)
    val graph = makeGraph(grid)
    val start = Vec2(1, 0)
    val target = Vec2(grid.head.size - 2, grid.size - 1)
    longest2(graph, Set(start), 0, start, target)
  }

  def longest(grid: Grid, seen: Set[Vec2], pos: Vec2, target: Vec2, slippery: Boolean = true): Long = {
    if (pos == target) {
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

  def longest2(
      allNeighbors: Map[Vec2, Set[(Vec2, Int)]],
      seen: Set[Vec2],
      distance: Int,
      pos: Vec2,
      target: Vec2
  ): Long = {
    if (pos == target) {
      distance
    } else {
      val results = allNeighbors(pos).filter { case (k, v) => !seen.contains(k) }.map { case (neighbor, d) =>
        longest2(allNeighbors, seen + neighbor, distance + d, neighbor, target)
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

  def makeGraph(grid: Grid): Map[Vec2, Set[(Vec2, Int)]] = {
    var allNeighbors = Map[Vec2, Set[(Vec2, Int)]]()
    positions(grid).filter(p => get(grid, p) != '#').map { pos =>
      val posNeighbors = neighbors(pos, includeDiagonals = false)
        .filter(p => inRange(grid, p) && !(get(grid, p) == '#'))

      allNeighbors = allNeighbors.updated(pos, posNeighbors.map(p => (p, 1)).toSet)
    }

    reduce(allNeighbors)
  }

  def reduce(allNeighbors: Map[Vec2, Set[(Vec2, Int)]]): Map[Vec2, Set[(Vec2, Int)]] = {
    allNeighbors.find { case (pos, posNeighbors) =>
      posNeighbors.size == 2
    } match {
      case Some((pos, (posNeighbors: Set[(Vec2, Int)]))) =>
        var newNeighbors = allNeighbors - pos
        val (a: Vec2, aDist: Int) = posNeighbors.toSeq.head
        val (b: Vec2, bDist: Int) = posNeighbors.toSeq.last

        newNeighbors = newNeighbors.updated(a, (newNeighbors(a) + ((b, aDist + bDist))).filterNot(e => e._1 == pos))
        newNeighbors = newNeighbors.updated(b, (newNeighbors(b) + ((a, aDist + bDist))).filterNot(e => e._1 == pos))

        reduce(newNeighbors)
      case None => allNeighbors
    }
  }

}
