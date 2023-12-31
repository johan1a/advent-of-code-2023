package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day24 {

  case class Inf()

  case class Vec2d(x: BigDecimal, y: BigDecimal)

  case class Stone(pos: Vec3, velocity: Vec3)

  // y = kx + m
  case class StoneXy(pos: Vec2, velocity: Vec2, k: Either[Inf, BigDecimal], m: Option[BigDecimal])

  // (x, y, z) = (vx, vy, vz) * t
  // x = vx * t
  // (18, 19) = (-1, -1) * 0 + k
  // (x, y) = (-1, -1) * t + (18, 19)
  // t=0: (18, 19)
  // t=1: (-1, -1) * 1 + (18, 19) = (17, 18)
  // t=2: (-1, -1) * 2 + (18, 19) = (16, 17)
  // t=24: (-1, -1) * 24 + (18, 19) = (18-24, 19-24) = (-6, -5)

  // (12, 31) = (-1, -2) * 0 + (12, 31)
  // (x, y) = (-1, -2) * t + (12, 31)
  // t=0: (12, 31)
  // t=1: (-1, -2) * 1 + (12, 31) = (11, 29)

  // (x, y) = (-1, -1) * t + (18, 19)
  // (x, y) = (-1, -2) * t + (12, 31)
  // (-1, -1) * t + (18, 19) = (-1, -2) * t + (12, 31)

  // t=0: (18, 19)
  // t=1: (-1, -1) * 1 + (18, 19) = (17, 18)
  // (18-19)/(17-18)=1
  // 19=1*18+k
  // 19=18+k
  // 1=k
  // y = 1*x + 1

  // t=0: (12, 31)
  // t=1: (-1, -2) * 1 + (12, 31) = (11, 29)
  // (29-31)/(11-12)=2
  // 31=2*12+k
  // 31=24+k
  // 7=k
  // y=2*x+7

  // x = (mb - ma) * kb/ka
  // 1x + 1 = 2x + 7
  // 1x = 2x + 6
  // 0 = x + 6
  // x = -6
  // y = 1x+1
  // y(-6) = -6+1=-5
  // collision at (-6, -5)
  // for which t?

  // (x, y) = (-1, -1) * t + (18, 19)
  // (-6, -5) = (-1, -1) * t + (18, 19)
  // (-24, -24) = (-1, -1) * t + (0, 0)
  // (-24, -24) = (-1, -1) * t
  // -24=-t
  // 24=t

  // for parallel lines
  // (x, y) = (-2, -2) * t + (20, 25)
  // t=0: (20,25)
  // t=1: (-2, -2) * 1 + (20, 25) = (18, 23)
  // (23-25)/(18-20)=1
  // 25=1*20+k
  // 5=k
  // y=1*x+5

  // y = 1 * x + 1
  // y = 1 * x + 5
  // parallel since 1 = 1

  // 1x + 1 = 2x + 7
  // ka*x + ma = kb*x + mb
  // ka*x = kb*x + mb - ma
  // ka*x - kb*x = mb - ma
  // x * (ka - kb) = mb - ma
  // x = (mb - ma) / (ka - kb)

  // get pos(0t) and pos(1t) (for each line L)
  // calculate y=kx+m (for each L)
  // compare La and Lb
  // if ka == kb, parallel, no collision
  // else
  // calculate x,y of collision
  // xc = (mb - ma) / (ka - kb)
  // yc = ka * xc + ma
  // collision is at (xc, yc)
  // if (xc, yc) is in test area:
  // calculate t of collision
  //
  // t = (xc - xPos) / vx
  // if t >= 0, count this collision

  def part1(input: Seq[String], min: Long = 200000000000000L, max: Long = 400000000000000L): Long = {
    val stones = parse(input).map(calculateXyLine)
    0.until(stones.size)
      .flatMap { i =>
        (i + 1).until(stones.size).filter { j =>
          collides(min, max, stones(i), stones(j))
        }
      }
      .size
  }

  def collides(min: Long, max: Long, a: StoneXy, b: StoneXy): Boolean = {
    if (a.k == b.k) {
      // parallel
      false
    } else {
      (a.k, b.k) match {
        case (Right(ak), Right(bk)) =>
          val xc = (b.m.get - a.m.get) / (ak - bk)
          val yc = ak * xc + a.m.get
          if (xc < min || xc > max || yc < min || yc > max) {
            false
          } else {
            val ta = (xc - a.pos.x) / a.velocity.x
            val tb = (xc - b.pos.x) / b.velocity.x
            ta >= 0 && tb >= 0
          }
        case (Left(Inf()), Right(bk))   => ???
        case (Right(ak), Left(Inf()))   => ???
        case (Left(Inf()), Left(Inf())) => ???
      }
    }
  }

  def parallel(a: StoneXy, b: StoneXy): Boolean = {
    a.k == b.k
  }

  def parallelXy(a: Stone, b: Stone): Boolean = {
    calculateXyLine(a).k == calculateXyLine(b).k
  }

  def getFutureCollisionXy(a: Stone, b: Stone): Option[Vec2d] = {
    getFutureCollision(calculateXyLine(a), calculateXyLine(b))
  }

  def getFutureCollision(a: StoneXy, b: StoneXy): Option[Vec2d] = {
    if (parallel(a, b)) {
      None
    } else {
      (a.k, b.k) match {
        case (Right(ak), Right(bk)) =>
          val xc = (b.m.get - a.m.get) / (ak - bk)
          val yc = ak * xc + a.m.get
          val ta = (xc - a.pos.x) / a.velocity.x
          val tb = (xc - b.pos.x) / b.velocity.x
          Option.when(ta >= 0 && tb >= 0)(Vec2d(xc, yc))
        case (Left(Inf()), Right(bk)) =>
          val xc = a.pos.x
          val yc = bk * xc + b.m.get
          val ta = 0
          val tb = (xc - b.pos.x) / b.velocity.x
          Option.when(ta >= 0 && tb >= 0)(Vec2d(xc, yc))
        case (Right(ak), Left(Inf())) =>
          val xc = b.pos.x
          val yc = ak * xc + a.m.get
          val ta = (xc - a.pos.x) / a.velocity.x
          val tb = 0
          Option.when(ta >= 0 && tb >= 0)(Vec2d(xc, yc))
        case (Left(Inf()), Left(Inf())) => ???
      }
    }
  }

  def calculateXyLine(stone: Stone): StoneXy = {
    val x0 = stone.pos.x
    val y0 = stone.pos.y
    val vX = stone.velocity.x
    val vY = stone.velocity.y

    val x1 = vX + x0
    val y1 = vY + y0
    val tolerance = 0.00001
    val k = ((BigDecimal(x1) - BigDecimal(x0))) match {
      case diff if diff > tolerance || diff < -tolerance => Right((y1 - y0) / diff)
      case _                                             => Left(Inf())
    }

    val m = k match {
      case Left(Inf()) => None
      case Right(n)    => Some(y0 - (n * x0))
    }

    StoneXy(
      pos = Vec2(x0, y0),
      velocity = Vec2(vX, vY),
      k = k,
      m = m
    )
  }

  // (x, y, z) = (x0, y0, z0) + (vx, vy, vz) * t
  // (xs, ys, zs) = (x0, y0, z0) + (vx - vxs, vy - vys, vz - vzs) * t
  def part2(input: Seq[String], maxV: Int = 100): Long = {
    val stones = parse(input)
    var answer: Option[Long] = None
    (-maxV - 1).until(maxV).find { vx =>
      if (vx % 10 == 0) {
        println(s"vx $vx")
      }
      findVy(maxV, vx, stones) match {
        case Some(vy) =>
          val modifiedStones = stones.map { stone =>
            stone.copy(velocity =
              stone.velocity.copy(
                x = stone.velocity.x - vx,
                y = stone.velocity.y - vy
              )
            )
          }
          val firstStone = modifiedStones.head
          val firstCollision = getFutureCollisionXy(firstStone, modifiedStones.last)
          val vz = findVz(maxV, vx, vy, firstCollision.get, firstStone, modifiedStones)
          val firstCollisionT = (firstCollision.get.x - firstStone.pos.x) / firstStone.velocity.x
          val collisionZ = firstStone.pos.z + firstCollisionT * (firstStone.velocity.z - vz.get)
          answer = Some((firstCollision.get.x + firstCollision.get.y + collisionZ).toLong)
          true
        case _ => false
      }
    }

    answer.get
  }

  def findVy(maxV: Int, vx: Int, stones: Seq[Stone]) = {
    val foundVY = (-maxV).until(maxV).find { vy =>
      val modifiedStones = stones.map { stone =>
        stone.copy(velocity =
          stone.velocity.copy(
            x = stone.velocity.x - vx,
            y = stone.velocity.y - vy
          )
        )
      }
      var j = 1
      val firstStone = modifiedStones(0)
      while (parallelXy(firstStone, modifiedStones(j))) {
        j += 1
      }
      val firstCollision = getFutureCollisionXy(firstStone, modifiedStones(j))
      if (firstCollision.isEmpty) {
        false
      } else {
        val nbrCollisions = modifiedStones.filter { stone =>
          val collision = getFutureCollisionXy(firstStone, stone)
          val areParallel = parallelXy(firstStone, stone)

          areParallel || vec2dAreEqual(collision, firstCollision)
        }.size
        nbrCollisions == modifiedStones.size
      }
    }
    foundVY
  }

  def findVz(maxV: Int, vx: Int, vy: Int, firstCollision: Vec2d, firstStone: Stone, modifiedStones: Seq[Stone]) = {
    (-maxV).until(maxV).find { vz =>
      val firstCollisionT = (firstCollision.x - firstStone.pos.x) / firstStone.velocity.x
      val firstCollisionZ = firstStone.pos.z + firstCollisionT * (firstStone.velocity.z - vz)
      val allZAreEqual = modifiedStones.filter(!parallelXy(firstStone, _)).forall { stone =>
        val collisionT = if (stone.velocity.x != 0) {
          (firstCollision.x - stone.pos.x) / stone.velocity.x
        } else {
          (firstCollision.y - stone.pos.y) / stone.velocity.y
        }
        val collisionZ = stone.pos.z + collisionT * (stone.velocity.z - vz)
        decimalsAreEqual(collisionZ, firstCollisionZ)
      }
      allZAreEqual
    }
  }

  def vec2dAreEqual(a: Option[Vec2d], b: Option[Vec2d]): Boolean = {
    (a, b) match {
      case (Some(Vec2d(xa, ya)), Some(Vec2d(xb, yb))) => decimalsAreEqual(xa, xb) && decimalsAreEqual(ya, yb)
      case _                                          => false
    }
  }

  def decimalsAreEqual(a: BigDecimal, b: BigDecimal): Boolean = {
    val tolerance = 0.00001d
    a - b < tolerance
  }

  def parse(input: Seq[String]) = {
    input.map { line =>
      line match {
        case s"$x, $y, $z @ $vx, $vy, $vz" =>
          Stone(
            Vec3(x.trim.toLong, y.trim.toLong, z.trim.toLong),
            Vec3(vx.trim.toLong, vy.trim.toLong, vz.trim.toLong)
          )
      }
    }
  }
}
