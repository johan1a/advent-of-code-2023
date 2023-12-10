package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

import scala.collection.mutable.ArrayBuffer

object Day10 {

  def part1(input: Seq[String]): Int = {
    val grid = Utils.makeGrid(input)
    val start = findStart(grid)
    maxDist(grid, start)
  }

  def maxDist(grid: ArrayBuffer[ArrayBuffer[Char]], start: Vec2): Int = {

    var seen = Set[Vec2]()
    var queue = Seq((start, 0))

    var maxDist = 0

    while (queue.nonEmpty) {
      val (curr, dist) = queue.head
      seen = seen + curr
      if (dist > maxDist) {
        maxDist = dist
      }
      queue = queue.tail

      getNeighbors(grid, curr).foreach { neighbor =>
        if(!seen.contains(neighbor)) {
          println(neighbor)
          queue = queue :+ (neighbor, dist + 1)
        }
      }
    }

    maxDist
  }

  def getNeighbors(grid: ArrayBuffer[ArrayBuffer[Char]], pos: Vec2): Seq[Vec2] = {

    val min = Vec2(0, 0)
    val max = getMax(grid)
    val diagonals = false
    neighbors(pos, min, max, diagonals).filter { case neighbor =>
      compatible(grid, Seq(pos, neighbor).sortBy(p => (p.x, p.y)))
    }
  }

  def compatible(grid: ArrayBuffer[ArrayBuffer[Char]], positions: Seq[Vec2]): Boolean = {

    val a = positions.head
    val b = positions.last
    val aChar = get(grid,a)
    val bChar = get(grid,b)
    println(s"compat $positions? $aChar $bChar")
    if(a.y==b.y) {
      // a left of b
      val x = Set(
        'S',
        '-',
        'L',
        'F',
        ).contains(get(grid,a)) &&
      Set(
        'S',
        '-',
        'J',
        '7',
        ).contains(get(grid,b))

      println(x)
      x
    } else if(a.x==b.x){
      // a above b
      val x =Set(
        'S',
        '|',
        '7',
        'F',
        ).contains(get(grid,a)) &&
      Set(
        'S',
        '|',
        'L',
        'J',
        ).contains(get(grid,b))
      println(x)
      x
    } else {
      ???
    }
  }

  def get(grid: ArrayBuffer[ArrayBuffer[Char]], pos: Vec2) = {
      grid(pos.y.toInt)(pos.x.toInt)
  }

  def findStart(grid: ArrayBuffer[ArrayBuffer[Char]]): Vec2 = {
    println(grid)
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

  def part2(input: Seq[String]): Int = {
    -1
  }
}
