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
    val startPositions = grid.indices.flatMap { y =>
      Seq(Arrow(Vec2(-1, y), "R"), Arrow(Vec2(grid.head.size, y), "L"))
    } ++ grid.head.indices.flatMap { x =>
      Seq(Arrow(Vec2(x, -1), "D"), Arrow(Vec2(x, grid.size), "U"))
    }
    startPositions.map(simulate(grid, _)).max
  }

  def simulate(grid: ArrayBuffer[ArrayBuffer[Char]], startArrow: Arrow): Int = {
    var energized = Set[Vec2]()
    var arrows = Set[Arrow](startArrow)
    var seen = Set[Arrow]()

    while (arrows.nonEmpty) {
      seen = seen ++ arrows
      arrows = arrows
        .map(moveArrow(grid, _))
        .flatMap { arrows =>
          arrows.map { arrow =>
            energized = energized + arrow.pos
            arrow
          }
        }
        .filterNot(seen.contains)
    }
    energized.size
  }

  def moveArrow(grid: ArrayBuffer[ArrayBuffer[Char]], arrow: Arrow): Seq[Arrow] = {
    val pos = arrow.pos
    val nextPos = move(pos, arrow.dir)
    if (inRange(grid, nextPos)) {
      val nextTile = get(grid, nextPos)
      val nextArrows = nextTile match {
        case '.'                         => Seq(Arrow(nextPos, arrow.dir))
        case '\\' if pos leftOf nextPos  => Seq(Arrow(nextPos, "D"))
        case '\\' if pos rightOf nextPos => Seq(Arrow(nextPos, "U"))
        case '\\' if pos above nextPos   => Seq(Arrow(nextPos, "R"))
        case '\\' if pos below nextPos   => Seq(Arrow(nextPos, "L"))
        case '/' if pos leftOf nextPos   => Seq(Arrow(nextPos, "U"))
        case '/' if pos rightOf nextPos  => Seq(Arrow(nextPos, "D"))
        case '/' if pos above nextPos    => Seq(Arrow(nextPos, "L"))
        case '/' if pos below nextPos    => Seq(Arrow(nextPos, "R"))
        case '|' if pos leftOf nextPos   => Seq(Arrow(nextPos, "U"), Arrow(nextPos, "D"))
        case '|' if pos rightOf nextPos  => Seq(Arrow(nextPos, "U"), Arrow(nextPos, "D"))
        case '|' if pos above nextPos    => Seq(Arrow(nextPos, "D"))
        case '|' if pos below nextPos    => Seq(Arrow(nextPos, "U"))
        case '-' if pos leftOf nextPos   => Seq(Arrow(nextPos, "R"))
        case '-' if pos rightOf nextPos  => Seq(Arrow(nextPos, "L"))
        case '-' if pos above nextPos    => Seq(Arrow(nextPos, "L"), Arrow(nextPos, "R"))
        case '-' if pos below nextPos    => Seq(Arrow(nextPos, "L"), Arrow(nextPos, "R"))
      }
      nextArrows.filter(a => inRange(grid, a.pos))
    } else {
      Seq.empty
    }
  }
}
