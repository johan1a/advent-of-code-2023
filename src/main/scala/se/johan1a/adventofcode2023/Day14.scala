package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.collection.mutable.ArrayBuffer

object Day14 {

  def part1(input: Seq[String]): Int = {
    val (rocks0, statics) = parse(input)
    val grid = makeGrid(input)
    var rocks = rocks0

    rocks = spin(grid, rocks, statics, Vec2(0, 1), rr => rr.sortBy(r => (r.x, r.y)))

    rocks.map(r => input.size - r.y).sum.toInt
  }

  def part2(input: Seq[String]): Int = {
    val (rocks, statics) = parse(input)
    val grid = makeGrid(input)

    val n = 1000000000
    val (rocks0, repStart, duplicate) = spinN(grid, rocks, statics, n)
    val period = duplicate - repStart.get
    // println(s"repStart $repStart.get duplicate: $duplicate period $period")

    val grid1 = makeGrid(input)
    val (rocks1, _, i) = spinN(grid1, rocks, statics, repStart.get + (n - repStart.get) % period)
    // println(s"i after: $i")

    // println("\ngrid")
    // printGrid(grid)
    // println("\ngrid1")
    // printGrid(grid1)

    rocks1.map(r => input.size - r.y).sum.toInt
  }

  def spinN(
      grid: ArrayBuffer[ArrayBuffer[Char]],
      rocks0: Seq[Vec2],
      statics: Seq[Vec2],
      n: Int
  ): (Seq[Vec2], Option[Int], Int) = {
    var seen = Map[Seq[Vec2], Int]()
    var rocks = rocks0
    val nbrRocks = rocks.size

    // println(s"\n0")
    // printGrid(grid)

    var i = 0
    while (i < n && !seen.contains(rocks)) {
      // if (i % 100 == 0) {
      println(i)
      // }

      seen = seen + (rocks -> i)

      // north
      rocks = spin(grid, rocks, statics, Vec2(0, 1), rr => rr.sortBy(r => (r.x, r.y)))
      // println(s"\nafter north")
      // printGrid(grid)

      assert(rocks.size == nbrRocks)

      // println("start west")
      // west
      rocks = spin(grid, rocks, statics, Vec2(1, 0), rr => rr.sortBy(r => (-r.y, r.x)))
      // println(s"\nafter west")
      // printGrid(grid)

      // south
      rocks = spin(grid, rocks, statics, Vec2(0, -1), rr => rr.sortBy(r => (r.x, -r.y)))
      // println(s"\nafter south")
      // printGrid(grid)

      // east
      rocks = spin(grid, rocks, statics, Vec2(-1, 0), rr => rr.sortBy(r => (r.y, -r.x)))
      // println(s"\nafter east")
      // printGrid(grid)

      i += 1
      // println(s"\n$i")
      // printGrid(grid)
      // val score = rocks.map(r => grid.size - r.y).sum.toInt
      // println(s"current score: $score i: $i")
    }

    val seenAt = seen.get(rocks)

    // println(s"\nseen size : ${seen.size} seen at: $seenAt")
    (rocks, seenAt, i)
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

    // west
    val debug = false && downDir == Vec2(1, 0)
    if (debug) {
      // println(s"rocks: $rocks")
    }

    var newRocks = Seq[Vec2]()

    var s = 0
    var nextStatic = statics(s)
    var prevPos: Option[Vec2] = None

    // if (debug) {
    //   // println("")
    //   // printGrid(grid)
    // }

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
        if (debug) {
          // println(s"pos: $r, static: $nextStatic, blocking: ${blockingList.last}, nextFreeSpot: $nextFreeSpot")
        }

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
        if (debug) {
          // println(s"empty blocklist pos: $r top: $top")
        }
        newRocks = newRocks :+ r
        prevPos = Some(r)
      }
      // if (debug) {
      //   // println("")
      //   // printGrid(grid)
      //   // scala.io.StdIn.readLine()
      // }
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
