package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.concurrent.Await

object Day16 {

  implicit val ec: ExecutionContext = ExecutionContext.global

  case class Arrow(pos: Vec2, dir: String)

  def part1(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    getEnergizedTiles(grid, Set(Arrow(Vec2(-1, 0), "R")), mutable.Set(), Set.empty).size
  }

  def part2(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    val startPositions = grid.indices.flatMap { y =>
      Seq(Arrow(Vec2(-1, y), "R"), Arrow(Vec2(grid.head.size, y), "L"))
    } ++ grid.head.indices.flatMap { x =>
      Seq(Arrow(Vec2(x, -1), "D"), Arrow(Vec2(x, grid.size), "U"))
    }
    Await
      .result(
        Future.sequence(startPositions.map(a => {
          Future {
            getEnergizedTiles(grid, Set(a), mutable.Set(), Set.empty).size
          }
        })),
        Duration.Inf
      )
      .max
  }

  @tailrec
  def getEnergizedTiles(
      grid: ArrayBuffer[ArrayBuffer[Char]],
      arrows: Set[Arrow],
      seen: mutable.Set[Arrow],
      energized: Set[Vec2]
  ): Set[Vec2] = {
    if (arrows.isEmpty) {
      energized
    } else {
      seen.addAll(arrows)
      var newEnergized = energized

      val newArrows = arrows
        .map(moveArrow(grid, _))
        .flatMap { arrows =>
          arrows.map { arrow =>
            newEnergized = newEnergized + arrow.pos
            arrow
          }
        }
        .filterNot(seen.contains)
      getEnergizedTiles(grid, newArrows, seen, newEnergized)
    }
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
