package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day22 {

  case class Brick(start: Vec3, end: Vec3, id: Int = -1)

  def part1(input: Seq[String]): Int = {
    val bricks = parse(input)
    val staticBricks = fall(bricks)
    count(staticBricks)
  }

  def part2(input: Seq[String]): Int = {
    val bricks = parse(input)
    val staticBricks = fall(bricks)
    count2(staticBricks)
  }

  def count(bricks: Seq[Brick]) = {
    val supporting: Map[Brick, Seq[Brick]] = bricks.map { brick =>
      brick -> bricks.filter(b => b.end.z == brick.start.z - 1 && intersectsXY(brick, b))
    }.toMap

    bricks.filter { brick =>
      val bricksAbove = bricks
        .filter(b => b.start.z == brick.end.z + 1 && intersectsXY(brick, b))

      bricksAbove.filter(above => supporting(above).size == 1).isEmpty
    }.size
  }

  def count2(bricks: Seq[Brick]) = {
    val supporting: Map[Int, Seq[Int]] = bricks.map { brick =>
      brick.id -> bricks.filter(b => b.end.z == brick.start.z - 1 && intersectsXY(brick, b)).map(_.id)
    }.toMap
    val above = bricks.map { brick =>
      brick.id -> bricks.filter(b => b.start.z == brick.end.z + 1 && intersectsXY(brick, b)).map(_.id)
    }.toMap
    bricks.map(b => countDisintegrating(supporting, above, b) - 1).sum
  }

  def countDisintegrating(below: Map[Int, Seq[Int]], above: Map[Int, Seq[Int]], brick: Brick) = {
    var disintegrated = Set[Int]()
    def disintegrate(b: Int): Unit = {
      if (!disintegrated.contains(b)) {
        disintegrated = disintegrated + b
        above(b).foreach { parent =>
          if (below(parent).forall(disintegrated.contains)) {
            disintegrate(parent)
          }
        }
      }
    }
    disintegrate(brick.id)
    disintegrated.size
  }

  def fall(allBricks: Seq[Brick]) = {
    var bricks = allBricks
    var static = Seq[Brick]()
    var maxHeight = Map[Vec2, Long]().withDefaultValue(0L)
    var posToId = Map[Vec2, Int]()

    while (bricks.nonEmpty) {
      val brick = bricks.head
      bricks = bricks.tail
      var max = -1L
      brick.start.x.to(brick.end.x).foreach { x =>
        brick.start.y.to(brick.end.y).foreach { y =>
          val h: Long = maxHeight(Vec2(x, y))
          if (h > max) {
            max = h
            maxHeight = maxHeight.updated(Vec2(x, y), h)
            posToId = posToId.updated(Vec2(x, y), brick.id)
          }
        }
      }
      val diff = brick.end.z - brick.start.z
      brick.start.x.to(brick.end.x).foreach { x =>
        brick.start.y.to(brick.end.y).foreach { y =>
          maxHeight = maxHeight.updated(Vec2(x, y), max + 1 + diff)
          posToId = posToId.updated(Vec2(x, y), brick.id)
        }
      }

      static = brick.copy(
        start = brick.start.copy(z = max + 1),
        end = brick.end.copy(z = max + 1 + diff)
      ) +: static
    }

    assert(static.size == allBricks.size)
    static
  }

  def intersectsXY(a: Brick, b: Brick): Boolean = {
    val outside: Boolean = b.start.x > a.end.x || a.start.x > b.end.x || b.start.y > a.end.y || a.start.y > b.end.y
    !outside
  }

  def parse(input: Seq[String]): Seq[Brick] = {
    var i = 0
    input
      .map { line =>
        line match {
          case s"$x0,$y0,$z0~$x1,$y1,$z1" =>
            val b = Brick(Vec3(x0.toLong, y0.toLong, z0.toLong), Vec3(x1.toLong, y1.toLong, z1.toLong), i)
            i += 1
            assert(z0.toLong <= z1.toLong)
            assert(x0.toLong <= x1.toLong)
            assert(y0.toLong <= y1.toLong)
            b
        }
      }
      .sortBy(b => b.start.z)
  }
}
