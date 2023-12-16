package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.collection.mutable.ArrayBuffer

object Day16 {

  case class Arrow(pos: Vec2, dir: String)

  def part1(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    simulate(grid, Arrow(Vec2(-1, 0), "R"))
  }

  def part2(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    val startPositions = 0.until(grid.size).flatMap { y =>
      Seq(Arrow(Vec2(-1, y), "R"), Arrow(Vec2(grid.head.size, y), "L"))
    } ++ 0.until(grid.head.size).flatMap { x =>
      Seq(Arrow(Vec2(x, -1), "D"), Arrow(Vec2(x, grid.size), "U"))
    }
    var i = 0
    var max = -1
    startPositions
      .map(p => {
        i += 1
        val result = simulate(grid, p)
        if (result > max) {
          max = result
        }
        result
      })
    max
  }

  def simulate(grid: ArrayBuffer[ArrayBuffer[Char]], startArrow: Arrow): Int = {
    var nbrEnergized = Set[Vec2]()
    var arrows = Set[Arrow](startArrow)
    var seen = Set[Arrow]()

    var i = 0
    while (arrows.nonEmpty) {
      seen = seen ++ arrows
      arrows = arrows.map(a => moveArrow(grid, a)).flatMap { arrows =>
        arrows.map { arrow =>
          nbrEnergized = nbrEnergized + arrow.pos
          arrow
        }
      }.filterNot(seen.contains)
      i += 1
    }

    nbrEnergized.size
  }

  implicit class ComparableDirection(a: Vec2) {
    def leftOf(b: Vec2): Boolean = a.y == b.y && a.x < b.x
    def rightOf(b: Vec2): Boolean = a.y == b.y && a.x > b.x
    def above(b: Vec2): Boolean = a.x == b.x && a.y < b.y
    def below(b: Vec2): Boolean = a.x == b.x && a.y > b.y
  }

  def moveArrow(grid: ArrayBuffer[ArrayBuffer[Char]], arrow: Arrow): Seq[Arrow] = {
    val pos = arrow.pos
    val nextPos = move(pos, arrow.dir)
    if (inRange(grid, nextPos)) {
      val nextTile = get(grid, nextPos)
      val nextArrows: Seq[Arrow] = nextTile match {
        case '.'                          => Seq(Arrow(nextPos, arrow.dir))
        case '\\' if pos.leftOf(nextPos)  => Seq(Arrow(nextPos, "D"))
        case '\\' if pos.rightOf(nextPos) => Seq(Arrow(nextPos, "U"))
        case '\\' if pos.above(nextPos)   => Seq(Arrow(nextPos, "R"))
        case '\\' if pos.below(nextPos)   => Seq(Arrow(nextPos, "L"))
        case '/' if pos.leftOf(nextPos)   => Seq(Arrow(nextPos, "U"))
        case '/' if pos.rightOf(nextPos)  => Seq(Arrow(nextPos, "D"))
        case '/' if pos.above(nextPos)    => Seq(Arrow(nextPos, "L"))
        case '/' if pos.below(nextPos)    => Seq(Arrow(nextPos, "R"))
        case '|' if pos.leftOf(nextPos)   => Seq(Arrow(nextPos, "U"), Arrow(nextPos, "D"))
        case '|' if pos.rightOf(nextPos)  => Seq(Arrow(nextPos, "U"), Arrow(nextPos, "D"))
        case '|' if pos.above(nextPos)    => Seq(Arrow(nextPos, "D"))
        case '|' if pos.below(nextPos)    => Seq(Arrow(nextPos, "U"))
        case '-' if pos.leftOf(nextPos)   => Seq(Arrow(nextPos, "R"))
        case '-' if pos.rightOf(nextPos)  => Seq(Arrow(nextPos, "L"))
        case '-' if pos.above(nextPos)    => Seq(Arrow(nextPos, "L"), Arrow(nextPos, "R"))
        case '-' if pos.below(nextPos)    => Seq(Arrow(nextPos, "L"), Arrow(nextPos, "R"))
      }
      nextArrows.filter(a => inRange(grid, a.pos))
    } else {
      Seq.empty
    }

  }
}
