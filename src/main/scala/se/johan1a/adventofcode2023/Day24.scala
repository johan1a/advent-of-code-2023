package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day24 {

  case class Vec2d(x: Double, y: Double)

  case class Stone(pos: Vec3, velocity: Vec3)

  // y = kx + m
  case class StoneXy(pos: Vec2, velocity: Vec2, k: Double, m: Double)

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
          val c = collides(min, max, stones(i), stones(j))
          // if(c) {
          //   println(s"${stones(i)} collides with ${stones(j)}}")
          // }

          c
        }
      }
      .size
  }

  def collides(min: Long, max: Long, a: StoneXy, b: StoneXy): Boolean = {
    if (a.k == b.k) {
      // parallel
      false
    } else {
      val xc = (b.m - a.m) / (a.k - b.k)
      val yc = a.k * xc + a.m
      // println(s"a: $a, b: $b, collision at: ($xc, $yc)")
      if (xc < min || xc > max || yc < min || yc > max) {
        false
      } else {
        val ta = (xc - a.pos.x) / a.velocity.x
        val tb = (xc - b.pos.x) / b.velocity.x
        // if (ta>=0 && tb >=0) {
        //   println(s"${a} ${b} collision at x: $xc, ta: $ta, tb: $tb}")
        // }
        ta >= 0 && tb >= 0
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
      val xc = (b.m - a.m) / (a.k - b.k)
      val yc = a.k * xc + a.m
      // println(s"a: $a, b: $b, collision at: ($xc, $yc)")
      val ta = (xc - a.pos.x) / a.velocity.x
      val tb = (xc - b.pos.x) / b.velocity.x
      // if (ta>=0 && tb >=0) {
      //   println(s"${a} ${b} collision at x: $xc, ta: $ta, tb: $tb}")
      // }
      Option.when(ta >= 0 && tb >= 0)(Vec2d(xc, yc))
    }
  }

  def calculateXyLine(stone: Stone): StoneXy = {
    val x0 = stone.pos.x
    val y0 = stone.pos.y
    val vX = stone.velocity.x
    val vY = stone.velocity.y

    val x1 = vX + x0
    val y1 = vY + y0
    val k = (y1 - y0) / (x1.toDouble - x0)
    // println(s"$stone (x0, y0) ($x0, $y0) (x1, y1) ($x1, $y1) k: $k")
    val m = y0 - (k * x0)

    StoneXy(
      pos = Vec2(x0, y0),
      velocity = Vec2(vX, vY),
      k = k,
      m = m
    )
  }

  var mostNbrXyCollisions = 0

  // (x, y, z) = (x0, y0, z0) + (vx, vy, vz) * t
  // (xs, ys, zs) = (x0, y0, z0) + (vx - vxs, vy - vys, vz - vzs) * t
  def part2(input: Seq[String], maxV: Int = 100): Long = {
    println("Start part2")
    val stones = parse(input)
    var answer: Option[Long] = None
    mostNbrXyCollisions = 0
    // test rock pos:
    // 24 13 10
    // velocity:
    // -3 1 2
    var vx = -maxV - 1
    while (vx < maxV && answer.isEmpty) {
      vx += 1
      val (foundVY, foundAnswer) = findVy(maxV, vx, stones)

      val modifiedStones = stones.map { stone =>
        stone.copy(velocity =
          stone.velocity.copy(
            x = stone.velocity.x - vx,
            y = stone.velocity.y - foundVy.get
          )
        )
      }
      val firstStone = modifiedStones.head
      val firstCollision = getFutureCollisionXy(firstStone, modifiedStones.last)
      val foundVZ = findVz(maxV, vx, foundVy.get, firstCollision.get, firstStone, modifiedStones)
      val firstCollisionT = (firstCollision.get.x - firstStone.pos.x) / firstStone.velocity.x
      val collisionZ = firstStone.pos.z + firstCollisionT * (firstStone.velocity.z - foundVZ.get)

      println(s"Every stone collides at $firstCollision hail vz: ${foundVZ} collision Z: $collisionZ")
      println(s"Stone starts at ${firstCollision.get} ${collisionZ} velocity $vx $vy ${foundVZ.get}")
      answer = Some((firstCollision.get.x + firstCollision.get.y + collisionZ).toLong)
    }
    answer.get
  }

  def findVy(maxV: Int, vx: Int, stones: Seq[Stone]) = {
    var answer: Option[Long] = None
    val foundVy = (-maxV).until(maxV).find { vy =>
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
        if (nbrCollisions > mostNbrXyCollisions) {
          mostNbrXyCollisions = nbrCollisions
          println(s"Nbr collisions: $nbrCollisions for vx $vx vy $vy at $firstCollision")
        }
        val allCollide = nbrCollisions == modifiedStones.size
        if (allCollide) {
          println(s"Nbr collisions: $nbrCollisions for vx $vx vy $vy at $firstCollision")
        }
        if (allCollide) {
          // collisionX = x0 + t * vx
          // t = (collisionX - x0) / vx

          val foundVZ = findVz(maxV, vx, vy, firstCollision.get, firstStone, modifiedStones)
          val firstCollisionT = (firstCollision.get.x - firstStone.pos.x) / firstStone.velocity.x
          val collisionZ = firstStone.pos.z + firstCollisionT * (firstStone.velocity.z - foundVZ.get)

          println(s"Every stone collides at $firstCollision hail vz: ${foundVZ} collision Z: $collisionZ")
          println(s"Stone starts at ${firstCollision.get} ${collisionZ} velocity $vx $vy ${foundVZ.get}")
          answer = Some((firstCollision.get.x + firstCollision.get.y + collisionZ).toLong)
        }
        allCollide
      }
    }
    (foundVy, answer)
  }

  def findVz(maxV: Int, vx: Int, vy: Int, firstCollision: Vec2d, firstStone: Stone, modifiedStones: Seq[Stone]) = {

    (-maxV).until(maxV).find { vz =>
      val firstCollisionT = (firstCollision.x - firstStone.pos.x) / firstStone.velocity.x
      // collisionZ = z0 + collisionT * vz
      val firstCollisionZ = firstStone.pos.z + firstCollisionT * (firstStone.velocity.z - vz)
      val allZAreEqual = modifiedStones.filter(!parallelXy(firstStone, _)).forall { stone =>
        val collisionT = (firstCollision.x - stone.pos.x) / stone.velocity.x
        val collisionZ = stone.pos.z + collisionT * (stone.velocity.z - vz)
        collisionZ == firstCollisionZ
      }
      allZAreEqual
    }
  }

  def vec2dAreEqual(a: Option[Vec2d], b: Option[Vec2d]): Boolean = {
    (a, b) match {
      case (Some(Vec2d(xa, ya)), Some(Vec2d(xb, yb))) => doublesAreEqual(xa, xb) && doublesAreEqual(ya, yb)
      case _                                          => false
    }
  }

  def doublesAreEqual(a: Double, b: Double): Boolean = {
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
