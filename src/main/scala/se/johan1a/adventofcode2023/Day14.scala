package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day14 {

  def part1(input: Seq[String]): Int = {
    val (rocks0, statics) = parse(input)
    val grid = makeGrid(input)
    var rocks = rocks0
    var moving = true
    val maxIter = 100000
    var i = 0
    // printGrid(grid)
    // println()
    while (moving && i < maxIter) {
      moving = false
      val newRocks = rocks.sortBy(_.y).map { r =>
        val still = r.y == 0 || Seq('#', 'O').contains(grid(r.y.toInt-1)(r.x.toInt))
        if (still){
          r
        } else {
          moving = true
          grid(r.y.toInt-1)(r.x.toInt) = 'O'
          grid(r.y.toInt)(r.x.toInt) = '.'
          Vec2(r.x,r.y-1)
        }
      }
      i += 1
      rocks = newRocks
      // printGrid(grid)
      // println()
    }
    rocks.map(r =>  input.size-r.y).sum.toInt
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  def parse(input: Seq[String]) = {
    var rocks = Seq[Vec2]()
    var statics = Seq[Vec2]()
    input.indices.foreach { y =>
      input.head.indices.foreach { x =>
        if(input(y)(x)=='O') {
          rocks = rocks :+ Vec2(x,y)
        } else if (input(y)(x)=='#') {
          statics = statics :+ Vec2(x,y)
        }
      }
    }
    (rocks, statics)
  }
}
