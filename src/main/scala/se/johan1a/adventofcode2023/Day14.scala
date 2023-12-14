package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.collection.mutable.ArrayBuffer

object Day14 {

  def part1(input: Seq[String]): Int = {
    val (rocks, statics) = parse(input)
    val grid = makeGrid(input)

    val changedRocks = spin(grid, rocks, statics, Vec2(0, 1), rr => rr.sortBy(r => (r.x, r.y)))

    score(grid, changedRocks)
  }

  def part2(input: Seq[String]): Int = {
    val (rocks, statics) = parse(input)
    val grid = makeGrid(input)

    val n = 1000000000
    val (rocks0, scores, repStart, period) = spinN(grid, rocks, statics, n)

    val finalIndex = repStart + (n - repStart) % period

    scores(finalIndex)
  }

  def score(
      grid: ArrayBuffer[ArrayBuffer[Char]],
      rocks: Seq[Vec2]
  ) = rocks.map(r => grid.head.size - r.y).sum.toInt

  def spinN(
      grid: ArrayBuffer[ArrayBuffer[Char]],
      rocks0: Seq[Vec2],
      statics: Seq[Vec2],
      n: Int
  ): (Seq[Vec2], Map[Int, Int], Int, Int) = {
    var seen = Map[Seq[Vec2], Int]()
    var scores = Map[Int, Int]()
    var rocks = rocks0
    var i = 0
    while (i < n && !seen.contains(rocks)) {
      seen = seen + (rocks -> i)

      // north
      rocks = spin(grid, rocks, statics, Vec2(0, 1), rr => rr.sortBy(r => (r.x, r.y)))
      // west
      rocks = spin(grid, rocks, statics, Vec2(1, 0), rr => rr.sortBy(r => (-r.y, r.x)))
      // south
      rocks = spin(grid, rocks, statics, Vec2(0, -1), rr => rr.sortBy(r => (r.x, -r.y)))
      // east
      rocks = spin(grid, rocks, statics, Vec2(-1, 0), rr => rr.sortBy(r => (r.y, -r.x)))

      i += 1
      scores = scores + (i -> score(grid, rocks))
    }

    val seenAt = seen(rocks)
    (rocks, scores, seenAt, i - seenAt)
  }

  def spin(
      grid: ArrayBuffer[ArrayBuffer[Char]],
      rocks0: Seq[Vec2],
      statics0: Seq[Vec2],
      downDir: Vec2,
      sort: (Seq[Vec2] => Seq[Vec2])
  ) = {
    val rocks = sort(rocks0)
    val statics = sort(statics0)
    var newRocks = Seq[Vec2]()
    var s = 0
    var nextStatic = statics(s)
    var prevPos: Option[Vec2] = None

    rocks.foreach { r =>
      val top = getTop(grid, r, downDir)

      while (s < statics.size - 1 && sort(Seq(nextStatic, top)).head == nextStatic) {
        s += 1
        nextStatic = statics(s)
      }
      while (s < statics.size - 1 && sort(Seq(statics(s + 1), r)).last == r) {
        s += 1
        nextStatic = statics(s)
      }

      // get lowest of prev pos, top and next static
      // that still is above r
      val blockingList = sort(Seq(Some(top), prevPos, Some(nextStatic)).collect { case Some(p) => p })
        .filter(p => sort(Seq(p, r)).head == p)

      if (blockingList.nonEmpty) {

        val nextFreeSpot = add(blockingList.last, downDir)
        if (inRange(grid, nextFreeSpot)) {
          grid(r.y.toInt)(r.x.toInt) = '.'
          grid(nextFreeSpot.y.toInt)(nextFreeSpot.x.toInt) = 'O'

          newRocks = newRocks :+ nextFreeSpot
          prevPos = Some(nextFreeSpot)

          while (s < statics.size - 1 && sort(Seq(nextStatic, nextFreeSpot)).head == nextStatic) {
            s += 1
            nextStatic = statics(s)
          }
        }
      } else {
        newRocks = newRocks :+ r
        prevPos = Some(r)
      }
    }

    newRocks
  }

  def getTop(grid: ArrayBuffer[ArrayBuffer[Char]], pos: Vec2, downDir: Vec2) = {
    downDir match {
      // north
      case Vec2(0, 1) => Vec2(pos.x, -1)
      // west
      case Vec2(1, 0) => Vec2(-1, pos.y)
      // south
      case Vec2(0, -1) => Vec2(pos.x, grid.size)
      // east
      case Vec2(-1, 0) => Vec2(grid.head.size, pos.y)
    }
  }

  def parse(input: Seq[String]) = {
    var rocks = Seq[Vec2]()
    var statics = Seq[Vec2]()
    input.indices.foreach { y =>
      input.head.indices.foreach { x =>
        if (input(y)(x) == 'O') {
          rocks = rocks :+ Vec2(x, y)
        } else if (input(y)(x) == '#') {
          statics = statics :+ Vec2(x, y)
        }
      }
    }
    (rocks, statics)
  }
}
