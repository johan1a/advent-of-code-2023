package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.collection.mutable.PriorityQueue

object Day17 {

  def part1(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    val target = Vec2(grid.head.size - 1, grid.size - 1)
    search(grid, Vec2(0, 0), target, 0, 3)
  }
  def part2(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    val target = Vec2(grid.head.size - 1, grid.size - 1)
    search(grid, Vec2(0, 0), target, 4, 10)
  }

  def search(grid: Grid, start: Vec2, target: Vec2, minNbrStraight: Int, maxNbrStraight: Int): Int = {
    val pq = new PriorityQueue[(Vec2, String, Int, Int)]()(Ordering.by(x => (-x._4)))
    pq.addOne((start, "?", 0, 0))
    var seen = Set[(Vec2, String, Int)]()
    var best: Option[Int] = None

    while (pq.nonEmpty) {
      val (curr, currDir, nbrStraight, dist) = pq.dequeue()

      if (curr == target && nbrStraight >= minNbrStraight) {
        if (best.map(b => dist < b).getOrElse(true)) {
          best = Some(dist)
        }
      }

      val state = (curr, currDir, nbrStraight)

      if (!seen.contains(state)) {
        seen = seen + state

        getNeighbors(grid, currDir, nbrStraight, curr, minNbrStraight, maxNbrStraight).foreach {
          case (neighbor, dir, nbrStraight) =>
            val dx = grid(neighbor.y.toInt)(neighbor.x.toInt).toString.toInt
            val d = dist + dx

            pq.addOne((neighbor, dir, nbrStraight, d))
        }
      }
    }

    best.get
  }

  def getNeighbors(grid: Grid, dir: String, nbrStraight: Int, pos: Vec2, minNbrStraight: Int, maxNbrStraight: Int) = {

    val possibleDirs = {
      if (dir != "?" && nbrStraight < minNbrStraight) {
        Seq(dir)
      } else {
        dir match {
          case ">" =>
            if (nbrStraight == maxNbrStraight) {
              Seq("^", "v")
            } else {
              Seq("^", "v", ">")
            }
          case "<" =>
            if (nbrStraight == maxNbrStraight) {
              Seq("^", "v")
            } else {
              Seq("^", "v", "<")
            }
          case "^" =>
            if (nbrStraight == maxNbrStraight) {
              Seq("<", ">")
            } else {
              Seq("<", ">", "^")
            }
          case "v" =>
            if (nbrStraight == maxNbrStraight) {
              Seq("<", ">")
            } else {
              Seq("<", ">", "v")
            }
          case "?" =>
            Seq(">", "v")
        }
      }
    }

    possibleDirs
      .map(walk(pos, _))
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

}
