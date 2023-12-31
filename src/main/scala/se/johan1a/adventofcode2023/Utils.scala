package se.johan1a.adventofcode2023

import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Failure, Success}

object Utils {

  type Grid = ArrayBuffer[ArrayBuffer[Char]]

  case class Vec2(x: Long, y: Long) {
    def leftOf(other: Vec2): Boolean = x < other.x
    def rightOf(other: Vec2): Boolean = x > other.x
    def above(other: Vec2): Boolean = y < other.y
    def below(other: Vec2): Boolean = y > other.y

    def +(other: Vec2) = add(this, other)
    def -(other: Vec2) = sub(this, other)
  }

  case class Vec3(x: Long, y: Long, z: Long)

  def add(a: Vec2, b: Vec2): Vec2 = Vec2(a.x + b.x, a.y + b.y)
  def sub(a: Vec2, b: Vec2): Vec2 = Vec2(a.x - b.x, a.y - b.y)
  def sign(vec: Vec2): Vec2 = Vec2(vec.x.sign, vec.y.sign)

  def add(a: Vec3, b: Vec3): Vec3 = Vec3(a.x + b.x, a.y + b.y, a.z + b.z)
  def sub(a: Vec3, b: Vec3): Vec3 = Vec3(a.x - b.x, a.y - b.y, a.z - b.z)
  def sign(vec: Vec3): Vec3 = Vec3(vec.x.sign, vec.y.sign, vec.z.sign)

  def move(pos: Vec2, dir: String, n: Int = 1): Vec2 = {
    dir match {
      case "U" =>
        Vec2(pos.x, pos.y - n)
      case "D" =>
        Vec2(pos.x, pos.y + n)
      case "L" =>
        Vec2(pos.x - n, pos.y)
      case "R" =>
        Vec2(pos.x + n, pos.y)
    }
  }

  def manhattan(a: (Long, Long), b: (Long, Long)): Long = {
    val diff = (a._1 - b._1, a._2 - b._2)
    diff._1.abs + diff._2.abs
  }

  def manhattan(a: Vec2, b: Vec2): Long = {
    val diff = sub(a, b)
    diff.x.abs + diff.y.abs
  }

  def manhattan(a: Vec3, b: Vec3): Long = {
    val diff = sub(a, b)
    diff.x.abs + diff.y.abs + diff.z.abs
  }

  def numbers(line: String): Seq[Long] = {
    val regex = "[-]?\\d+".r
    regex.findAllMatchIn(line).toSeq.map(_.group(0).toLong)
  }

  def split(
      input: Seq[String],
      isEmpty: String => Boolean = _.isEmpty
  ): Seq[Seq[String]] = {
    var groups = Seq[Seq[String]]()
    var i = 0
    while (i < input.size) {
      var group = Seq[String]()
      while (i < input.size && !isEmpty(input(i))) {
        group = group :+ input(i)
        i += 1
      }
      if (group.nonEmpty) {
        groups = groups :+ group
      }
      while (i < input.size && isEmpty(input(i))) {
        i += 1
      }
    }
    groups
  }

  def inRange(grid: Grid, pos: Vec2): Boolean = inRange(pos, Vec2(0, 0), getMax(grid))

  def inRange(grid: Grid, row: Int, col: Int): Boolean =
    inRange(Vec2(col, row), Vec2(0, 0), getMax(grid))

  def inRange(pos: Vec2, min: Vec2, max: Vec2): Boolean =
    pos.x >= min.x && pos.x < max.x && pos.y >= min.y && pos.y < max.y

  def inRange(pos: Vec3, min: Vec3, max: Vec3): Boolean =
    pos.x >= min.x && pos.x < max.x && pos.y >= min.y && pos.y < max.y && pos.z >= min.z && pos.z < max.z

  def positions(grid: Grid): Seq[Vec2] =
    grid.indices.flatMap { y =>
      grid.head.indices.map { x =>
        Vec2(x, y)
      }
    }

  def neighbors(
      pos: Vec2,
      min: Vec2 = Vec2(Long.MinValue, Long.MinValue),
      max: Vec2 = Vec2(Long.MaxValue, Long.MaxValue),
      includeDiagonals: Boolean = true
  ): Seq[Vec2] = {
    val offsets: Seq[Vec2] = Seq(
      Vec2(0, 1),
      Vec2(0, -1),
      Vec2(1, 0),
      Vec2(-1, 0)
    ) ++ (if (includeDiagonals) {
            Seq(
              Vec2(-1, -1),
              Vec2(1, -1),
              Vec2(-1, 1),
              Vec2(1, 1)
            )
          } else {
            Seq.empty
          })

    offsets
      .map(offset => add(pos, offset))
      .filter(inRange(_, min, max))
  }

  def neighbors3(
      pos: Vec3,
      min: Vec3 = Vec3(Long.MinValue, Long.MinValue, Long.MinValue),
      max: Vec3 = Vec3(Long.MaxValue, Long.MaxValue, Long.MaxValue)
  ): Seq[Vec3] = {
    val offsets: Seq[Vec3] = Seq(
      Vec3(0, 0, -1),
      Vec3(0, 0, 1),
      Vec3(0, -1, 0),
      Vec3(0, 1, 0),
      Vec3(-1, 0, 0),
      Vec3(1, 0, 0)
    )

    offsets
      .map(offset => add(pos, offset))
      .filter(inRange(_, min, max))
  }

  def makeGrid(lines: Seq[String]): Grid = {
    new ArrayBuffer().appendAll(lines.map(l => new ArrayBuffer().appendAll(l)))
  }

  def get(grid: Grid, pos: Vec2) = {
    grid(pos.y.toInt)(pos.x.toInt)
  }

  def get2(grid: Grid, pos: Vec2): Option[Char] = {
    Try(grid(pos.y.toInt)(pos.x.toInt)) match {
      case Success(p) => Some(p)
      case Failure(e) => None
    }
  }

  def printGrid(grid: Grid) = {
    grid.foreach { line =>
      println(line.mkString(""))
    }
  }

  def getMax(grid: Grid): Vec2 = Vec2(grid.head.size, grid.size)

  def bottomRight(grid: Grid): Vec2 = Vec2(grid.head.size - 1, grid.size - 1)

  def allPositions(grid: Grid): Seq[Vec2] = {
    0.until(grid.size).flatMap { y =>
      0.until(grid.head.size).map { x =>
        Vec2(x, y)
      }
    }
  }

  def straightPathsFromOutside(
      grid: Grid
  ): Seq[Seq[Seq[Vec2]]] = {
    val ySize = grid.size
    val xSize = grid.head.size
    val leftToRight = 0
      .until(ySize)
      .map(y => {
        0.until(xSize)
          .map(x => {
            Vec2(x, y)
          })
      })
    val rightToLeft = 0
      .until(ySize)
      .map(y => {
        0.until(xSize)
          .reverse
          .map(x => {
            Vec2(x, y)
          })
      })
    val topToBottom = 0
      .until(xSize)
      .map(x => {
        0.until(ySize)
          .map(y => {
            Vec2(x, y)
          })
      })
    val bottomToTop = 0
      .until(xSize)
      .map(x => {
        0.until(ySize)
          .reverse
          .map(y => {
            Vec2(x, y)
          })
      })
    Seq(leftToRight, rightToLeft, topToBottom, bottomToTop)
  }

  def straightPathsFromPos(
      grid: Grid,
      pos: Vec2
  ): Seq[Seq[Vec2]] = {
    val ySize = grid.size
    val xSize = grid.head.size

    val min = Vec2(0, 0)
    val max = getMax(grid)

    val right = (pos.x + 1)
      .until(xSize)
      .map(x => {
        Vec2(x, pos.y)
      })
      .filter(p => inRange(p, min, max))

    val left = 0
      .until(pos.x.toInt)
      .reverse
      .map(x => {
        Vec2(x, pos.y)
      })
      .filter(p => inRange(p, min, max))

    val down = (pos.y + 1)
      .until(ySize)
      .map(y => {
        Vec2(pos.x, y)
      })
      .filter(p => inRange(p, min, max))

    val up = 0
      .until(pos.y.toInt)
      .reverse
      .map(y => {
        Vec2(pos.x, y)
      })
      .filter(p => inRange(p, min, max))

    Seq(right, left, down, up)
  }

  def pairs[T](seq: Seq[T]): Seq[(T, T)] =
    0.until(seq.size).flatMap { i =>
      (i + 1).until(seq.size).map { j =>
        (seq(i), seq(j))
      }
    }

  def rotate(grid: Grid): Grid = {
    if (grid.size != grid.head.size) {
      throw new Exception("Sides must be equal")
    }
    val newGrid = ArrayBuffer.fill(grid.size)(ArrayBuffer.fill(grid.head.size)('?'))
    val width = grid.size
    0.to(width / 2).foreach { d =>
      0.until(width - 2 * d).foreach { i =>
        // top -> right
        newGrid(d + i)(newGrid.size - 1 - d) = grid(d)(d + i)

        // right -> bottom
        newGrid(newGrid.size - 1 - d)(newGrid.size - 1 - i - d) = grid(d + i)(newGrid.size - 1 - d)

        // bottom -> left
        newGrid(grid.size - 1 - d - i)(d) = grid(grid.size - 1 - d)(grid.size - 1 - d - i)

        // left -> top
        newGrid(d)(d + i) = grid(grid.size - 1 - i - d)(d)
      }
    }
    newGrid
  }

  // flip left - right
  def flip(grid: Grid): Grid = {
    val newData = ArrayBuffer.fill(grid.size)(ArrayBuffer.fill(grid.size)('?'))

    grid.indices.map { y =>
      grid.head.indices.map { x =>
        newData(y)(newData.size - 1 - x) = grid(y)(x)
      }
    }

    newData
  }

  // Math sux amirite?!
  def lcm(nn: Seq[Long]): Long = {
    var a = nn.head
    var i = 1
    while (i < nn.size) {
      val b = nn(i)
      a = lcm(a, b)
      i += 1
    }
    a
  }

  def lcm(a: Long, b: Long): Long = {
    Math.abs(a * b) / gcd(a, b)
  }

  def gcd(a: Long, b: Long): Long = {
    if (a == 0 && b == 0) {
      0
    } else if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }
  }
}
