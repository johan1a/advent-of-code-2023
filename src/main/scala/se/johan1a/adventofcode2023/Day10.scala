package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

object Day10 {

  type Grid = ArrayBuffer[ArrayBuffer[Char]]

  def part1(input: Seq[String]): Int = {
    val grid = Utils.makeGrid(input)
    val start = findStart(grid)
    makeLoop(grid, Seq((start, 0)))._2
  }

  def part2(input: Seq[String]): Int = {
    val grid = Utils.makeGrid(input)
    val doubled = double(grid)
    val start = findStart(doubled)
    val loop = makeLoop(doubled, Seq((start, 0)))._1
    countContained(doubled, loop, positions(grid))
  }

  @tailrec
  def makeLoop(
      grid: Grid,
      queue: Seq[(Vec2, Int)],
      seen: Set[Vec2] = Set.empty,
      maxDist: Int = 0
  ): (Set[Vec2], Int) = {
    queue match {
      case Nil => (seen, maxDist)
      case (curr, dist) +: tail =>
        val newNeighbors = compatibleNeighbors(grid, curr)
          .filterNot(seen.contains)
          .map(neighbor => (neighbor, dist + 1))
        makeLoop(grid, tail ++ newNeighbors, seen + curr, Math.max(dist, maxDist))
    }
  }

  def compatibleNeighbors(grid: Grid, pos: Vec2): Seq[Vec2] = {
    neighbors(pos, min = Vec2(0, 0), getMax(grid), includeDiagonals = false).filter { case neighbor =>
      compatible(grid, Seq(pos, neighbor).sortBy(p => (p.x, p.y)))
    }
  }

  @tailrec
  def countContained(
      grid: Grid,
      loop: Set[Vec2],
      queue: Seq[Vec2],
      outside: Set[Vec2] = Set.empty,
      inside: Set[Vec2] = Set.empty
  ): Int = {
    queue match {
      case Nil => inside.filter(pos => pos.x % 2 == 0 && pos.y % 2 == 0).size
      case pos +: tail =>
        if (!outside.contains(pos) && !inside.contains(pos) && !loop.contains(pos)) {

          flood(grid, outside, loop, Seq(pos)) match {
            case Inside(positions)  => countContained(grid, loop, tail, outside, inside ++ positions)
            case Outside(positions) => countContained(grid, loop, tail, outside ++ positions, inside)
          }
        } else {
          countContained(grid, loop, tail, outside, inside)
        }
    }
  }

  sealed trait FloodResult
  case class Inside(positions: Set[Vec2]) extends FloodResult
  case class Outside(positions: Set[Vec2]) extends FloodResult

  @tailrec
  def flood(
      grid: Grid,
      outside: Set[Vec2],
      loop: Set[Vec2],
      queue: Seq[Vec2],
      pending: Set[Vec2] = Set.empty,
      isInside: Boolean = true
  ): FloodResult =
    queue match {
      case Nil =>
        if (isInside) {
          Inside(pending)
        } else {
          Outside(pending)
        }
      case curr +: tail =>
        if (pending.contains(curr) || loop.contains(curr)) {
          flood(grid, outside, loop, tail, pending, isInside)
        } else {
          val allNeighbors = neighbors(curr)
          val inside =
            isInside && allNeighbors.forall(neighbor => !outside.contains(neighbor) && inRange(grid, neighbor))
          val newNeighbors = allNeighbors.filter(neighbor =>
            !outside.contains(neighbor)
              && inRange(grid, neighbor)
              && !pending.contains(neighbor)
              && !loop.contains(neighbor)
          )

          flood(grid, outside, loop, tail ++ newNeighbors, pending + curr, inside)
        }
    }

  def double(grid: Grid): Grid = {
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

  def compatible(grid: Grid, positions: Seq[Vec2]): Boolean = {
    val aPos = positions.head
    val bPos = positions.last
    val a = get(grid, aPos)
    val b = get(grid, bPos)
    if (aPos.y == bPos.y) {
      // a left of b
      Set('S', '-', 'L', 'F').contains(a) &&
      Set('S', '-', 'J', '7').contains(b)
    } else if (aPos.x == bPos.x) {
      // a above b
      Set('S', '|', '7', 'F').contains(a) &&
      Set('S', '|', 'L', 'J').contains(b)
    } else {
      ???
    }
  }

  def findStart(grid: Grid): Vec2 = {
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
