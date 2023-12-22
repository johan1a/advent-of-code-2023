package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day22 {

  case class Brick(start: Vec3, end: Vec3, id: Int = -1)

  def part1(input: Seq[String]): Int = {
    val bricks = parse(input)
    val staticBricks = fall(bricks)
    println("bricks")
    staticBricks.foreach(println)
    // printBricks(staticBricks)
    val c = count(staticBricks)
    // println("\nstatic")
    // staticBricks.takeRight(20).foreach(println)
    c
  }

  def count(bricks: Seq[Brick]) = {
    val supporting: Map[Brick, Seq[Brick]] = bricks.map { brick =>
      brick -> bricks.filter(b => b.end.z == brick.start.z - 1 && intersectsXY(brick, b))
    }.toMap

    // supporting.foreach { (k,v)=>
    //   println(s"$k supports $v")
    // }

    var supportedBy = Map[Int, Set[Int]]().withDefaultValue(Set.empty)

    val result = bricks.filter { brick =>
      val bricksAbove = bricks
        .filter(b => b.start.z == brick.end.z + 1 && intersectsXY(brick, b))

      bricksAbove.map { b =>
        supportedBy = supportedBy.updated(b.id, supportedBy(b.id) + brick.id)
      }

      val result = bricksAbove.filter(above => supporting(above).size == 1).isEmpty

      // println(s"\nbricks above $brick:")
      // bricksAbove.foreach(b => {
      //   println(s"$b nbr supporting: ${supporting(b).size}")
      //   println("supported by: ")
      //   supporting(b).foreach(println)
      // })
      // println(s"Can be removed? $result")

      result
    }.size

    supportedBy.map { (k, v) =>
      println(s"$k resting on {${v.toSeq.sorted.mkString(", ")}}")
    }

    result
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
          val h : Long = maxHeight(Vec2(x, y))
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
    // println(s"$a intersects with $b ? ${!outside}")
    !outside
  }

  def part2(input: Seq[String]): Int = {
    -1
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
