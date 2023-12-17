package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ArrayBuffer

object Day17 {

  def part1(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    search(grid, Vec2(0, 0), Vec2(grid.size - 1, grid.head.size - 1))
  }

  def search(grid: Grid, start: Vec2, end: Vec2): Int = {
    var prev = Map[Vec2, (Vec2, String)]()
    val pq = new PriorityQueue[(Vec2, String, Int, Int)]()(Ordering.by(x => (-x._4)))
    pq.addOne((start, "?", 0, 0))
    var seen = Set[(Vec2, String, Int)]()
    var best = Int.MaxValue
    while (pq.nonEmpty) {
      val (curr, currDir, nbrStraight, dist) = pq.dequeue()

      val state = (curr, currDir, nbrStraight)

      if (curr == end) {
        if (dist < best) {
          best = dist
        }
      }

      if (!seen.contains(state)) {
        seen = seen + state

        getNeighbors(grid, currDir, nbrStraight, curr).foreach { case (neighbor, dir, nbrStraight) =>
          val dx = grid(neighbor.y.toInt)(neighbor.x.toInt).toString.toInt
          val d = dist + dx

          prev = prev.updated(neighbor, (curr, dir))
          pq.addOne((neighbor, dir, nbrStraight, d))
        }
      }
    }

    best
  }

  def printPath(grid: Grid, prev: Map[Vec2, (Vec2, String)], pos: Vec2) = {
    val gridCopy = ArrayBuffer.fill(grid.size)(
      ArrayBuffer.fill(grid.head.size)('?')
    )
    grid.indices.foreach { y =>
      grid.indices.foreach { x =>
        gridCopy(y)(x) = grid(y)(x)
      }
    }

    var path = Seq[(Vec2, String)]()
    var curr = pos
    var dir = "?"
    while (prev.contains(curr)) {
      val p = prev(curr)
      dir = p._2
      path = path :+ (curr, dir)
      curr = p._1
    }
    path.foreach { case (pos, dir) =>
      gridCopy(pos.y.toInt)(pos.x.toInt) = dir.charAt(0)
    }
    printGrid(gridCopy)

  }

  def getNeighbors(grid: Grid, dir: String, nbrStraight: Int, pos: Vec2) = {
    val possibleDirs = dir match {
      case ">" =>
        if (nbrStraight == 3 && dir == ">") {
          Seq("^", "v")
        } else {
          Seq("^", "v", ">")
        }
      case "<" =>
        if (nbrStraight == 3 && dir == "<") {
          Seq("^", "v")
        } else {
          Seq("^", "v", "<")
        }
      case "^" =>
        if (nbrStraight == 3 && dir == "^") {
          Seq("<", ">")
        } else {
          Seq("<", ">", "^")
        }
      case "v" =>
        if (nbrStraight == 3 && dir == "v") {
          Seq("<", ">")
        } else {
          Seq("<", ">", "v")
        }
      case "?" =>
        Seq(">", "v")
    }

    possibleDirs
      .map { dir =>
        walk(pos, dir)
      }
      .filter { case (p, d) => inRange(grid, p) }
      .map { case (p, d) =>
        val newNbrStraight = if (d == dir) {
          nbrStraight + 1
        } else {
          1
        }

        (p, d, newNbrStraight)
      }
  }

  def walk(pos: Vec2, dir: String): (Vec2, String) = {
    dir match {
      case "^" =>
        (pos + Vec2(0, -1), "^")
      case "v" =>
        (pos + Vec2(0, 1), "v")
      case ">" =>
        (pos + Vec2(1, 0), ">")
      case "<" =>
        (pos + Vec2(-1, 0), "<")
    }
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
