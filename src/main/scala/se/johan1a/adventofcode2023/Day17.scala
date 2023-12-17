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
    var dists = Map[Vec2, Int]()
    val pq = new PriorityQueue[(Vec2, Int)]()(Ordering.by(x => -x._2))
    pq.addOne((start, 0))
    while (pq.nonEmpty) {
      val (curr, dist) = pq.dequeue()

      getNeighbors(grid, prev, curr).foreach { case (neighbor, dir) =>
        val dx = grid(neighbor.y.toInt)(neighbor.x.toInt).toString.toInt
        val d = dist + dx

        if (!dists.contains(neighbor) || d < dists(neighbor)) {
          dists = dists.updated(neighbor, d)
          prev = prev.updated(neighbor, (curr, dir))
          pq.addOne((neighbor, d))
        }
      }
    }

    println("final")
    printPath(grid, prev, end)
    println()
    println(prev(Vec2(5, 1)))
    println(prev(Vec2(4, 1)))

    dists(end)
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

  def getNeighbors(grid: Grid, prev: Map[Vec2, (Vec2, String)], pos: Vec2) = {
    val prev2 = prev.get(pos)
    val prev1 = prev2.flatMap(p => prev.get(p._1))
    val prev0 = prev1.flatMap(p => prev.get(p._1))

    val prevDirs = Seq(prev0, prev1, prev2).collect { case Some(p) => p }.map(_._2)
    val dir = prev.get(pos).map(_._2).getOrElse("?")

    val possibleDirs = dir match {
      case ">" =>
        if (prevDirs.size == 3 && prevDirs.forall(_ == ">")) {
          Seq("^", "v")
        } else {
          Seq("^", "v", ">")
        }
      case "<" =>
        if (prevDirs.size == 3 && prevDirs.forall(_ == "<")) {
          Seq("^", "v")
        } else {
          Seq("^", "v", "<")
        }
      case "^" =>
        if (prevDirs.size == 3 && prevDirs.forall(_ == "^")) {
          Seq("<", ">")
        } else {
          Seq("<", ">", "^")
        }
      case "v" =>
        if (prevDirs.size == 3 && prevDirs.forall(_ == "v")) {
          Seq("<", ">")
        } else {
          Seq("<", ">", "v")
        }
      case "?" =>
        Seq(">", "v")
    }

    val r = possibleDirs
      .map { dir =>
        walk(pos, dir)
      }
      .filter { case (p, d) => inRange(grid, p) }

    r
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
