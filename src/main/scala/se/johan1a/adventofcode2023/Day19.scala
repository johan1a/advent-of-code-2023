package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day19 {

  case class Condition(category: String, op: String, n: Int)
  case class Rule(condition: Option[Condition], destination: String)
  case class Workflow(name: String, rules: Seq[Rule])

  case class Part(x: Long, m: Long, a: Long, s: Long)

  case class Part2(var values: Map[String, (Long, Long)])

  def part1(input: Seq[String]): Long = {
    val (workflows, parts) = parse(input)
    parts.map(p => check(workflows, p)).sum
  }

  def part2(input: Seq[String], maxLimit: Int = 4000, debug: Boolean = false): Long = {
    println("\n\n--- part2 start ---\n\n")
    val (workflows, _) = parse(input)
    // each rule, two choices
    //  set to below/above limit
    //  or ignore
    // Then calculate based on the sum of all the values of successful ones
    // filter out common ranges somehow?
    // x*m*a*s?

    val parts = check2(
      workflows,
      Part2(
        Map(
          "x" -> (1L, maxLimit),
          "m" -> (1L, maxLimit),
          "a" -> (1, maxLimit),
          "s" -> (1, maxLimit)
        )
      ),
      "in"
    )

   // println(s"Original parts")
 //   sort(parts).foreach(println)
  //  println()
//    sort(parts).foreach(doubleCheck(workflows, _))

    println(s"combining ${parts.size} parts")
    //val combined = sort(combine(parts, debug).toSeq)
    //println(s"combined size ${combined.size}")

    // 0.until(combined.size - 1).foreach { i =>
    //   (i + 1).until(combined.size).foreach { j =>
    //     val part0 = combined(i)
    //     val part1 = combined(j)
    //     val overlap = Seq("x", "m", "a", "s").forall { name =>
    //       val value0 = part0.values(name)
    //       val value1 = part1.values(name)
    //       val min = Math.min(value0._1, value1._1)
    //       val max = Math.max(value0._2, value1._2)
    //       min
    //         .to(max)
    //         .find { k =>
    //           k >= value0._1 && k <= value0._2 && k >= value1._1 && k <= value1._2
    //         }
    //         .isDefined
    //     }
    //     if (overlap) {
    //       throw new Exception(s"found overlapping intervals: $part0, $part1")
    //     }
    //   }
    // }

    //combined.foreach(doubleCheck(workflows, _))

    val r = parts.toSeq.map { part =>
      val x = Seq("x", "m", "a", "s").map { name =>
        part.values(name)._2 - part.values(name)._1 + 1
      }
      x.product
    }
    //println(r)

    r.sum
  }

  def doubleCheck(workflows: Seq[Workflow], part: Part2) = {
    println(s"Checking part $part")
    val values = part.values

    for {
      x <- values("x")._1.to(values("x")._2, 100)
      m <- values("m")._1.to(values("m")._2, 100)
      a <- values("a")._1.to(values("a")._2, 100)
      s <- values("s")._1.to(values("s")._2, 100)
    } yield {
      val part = Part(x, m, a, s)
      val result = check(workflows, part)
      if (result == 0) {
        check(workflows, part, true)
        throw new Exception(s"Failing part: $part")
      }
    }

  }

  def combine(originalParts: Seq[Part2], debug0: Boolean = false): Set[Part2] = {
    val debug = debug0
    var interactive = debug

    var parts = originalParts

    println(s"parts original size ${parts.size}")

    val maxIter = 1000000
    var i = 0

    var ai = 0

    while (ai < parts.size - 1 && i < maxIter) {
      var combined = false
      if (debug) {
        println(s"outer loop start i: $i parts: $parts")
      }

      if (i % 1000 == 0) {
        println(s"i: $i parts size: ${parts.size}")
      }

      parts = sort(parts)
      var a = parts(ai)
      var b = parts(ai + 1)
      parts = parts.take(ai) ++ parts.drop(ai + 2)
      if (debug) {
        println(s"parts inner loop start, ai: $ai")
        parts.foreach(println)
      }
      Seq("x", "m", "a", "s").foreach { name =>
        val (aMin, aMax) = (a.values(name))
        val (bMin, bMax) = (b.values(name))

        if (debug) {
          println(s"\nname $name")
          println(s"a before $a")
          println(s"b before $b")
        }

        if (aMin < bMin && aMax >= bMin) {
          // split a into (amin, bmin-1), (bMin, amax) (two parts)
          // continue with bmin, amax part and add other to backlog
          val splitted = a.copy(values = a.values.updated(name, (aMin, bMin - 1)))
          a = a.copy(values = a.values.updated(name, (bMin, aMax)))
          parts = parts :+ splitted
          combined = true
          if (debug) {
            println("adding a variation")
          }
        }
        if (aMax < bMax && aMax >= bMin) {
          combined = true
          // split b into (bmin, amax), (amax+1, bmax) (two parts)
          // continue with continue with (bMin, aMax) part and add other to backlog
          val splitted = b.copy(values = b.values.updated(name, (aMax + 1, bMax)))
          b = b.copy(values = b.values.updated(name, (bMin, aMax)))
          parts = parts :+ splitted
          if (debug) {
            println("adding b variation")
          }
        }

        if (!interactive && (a.values.exists(v => v._2._1 > v._2._2)) || (b.values.exists(v => v._2._1 > v._2._2))) {
          println(s"starting debug a $a b $b")
          interactive = true
          scala.io.StdIn.readLine()
        }

        if (debug) {
          println(s"\na after $a")
          println(s"b after $b")
          println(s"parts after")
          parts.foreach(println)
          if (interactive) {
            scala.io.StdIn.readLine()
          }
        }
      }
      if (a == b) {
        parts = a +: parts
      } else {
        parts = a +: (b +: parts)
      }
      i += 1
      if (combined) {
        ai = 0
      } else {
        ai += 1
      }
    }

    if (i == maxIter) {
      throw new Exception(s"ERROR reached mat iterations $i")
    }

    parts.toSet
  }

  def sort: (Seq[Part2] => Seq[Part2]) = (parts) => {
    parts.sortBy(p =>
      (
        p.values("x")._1,
        p.values("x")._2,
        p.values("m")._1,
        p.values("m")._2,
        p.values("a")._1,
        p.values("a")._2,
        p.values("s")._1,
        p.values("s")._2
      )
    )
  }

  def check2(workflows: Seq[Workflow], part0: Part2, curr: String, debug0: Boolean = false): Seq[Part2] = {
    var debug = debug0
    curr match {
      case "A" => Seq(part0)
      case "R" => Seq.empty
      case _ =>
        val workflow = workflows.find(_.name == curr).get
        var rules = workflow.rules
        var next = Seq[(Part2, String)]()
        var partOpt: Option[Part2] = Some(part0)
        while (rules.nonEmpty && partOpt.isDefined) {
          val part = partOpt.get
          val rule = rules.head
          val matchesRule = matches2(rule, part)
          if (
            false && !matchesRule && (rule.destination == "R" || rule == Rule(Some(Condition("x", ">", 2440)), "R"))
          ) {
            debug = true
          }
          if (debug) {
            println(s"before matches?: $matchesRule next $next part: $part, rule: $rule")
          }
            if (matchesRule) {
              next = next :+ (part, rule.destination)
             // partOpt = createFailing(part, rule)
            } else {
              createSucceeding(part, rule).map { newPart =>
                next = next :+ (newPart, rule.destination)
              }
              partOpt = createFailing(part, rule)
            }

          rules = rules.tail
          if (debug) {
            println(s"after matches? $matchesRule next $next, partOpt: $partOpt")
            scala.io.StdIn.readLine()
          }
        }
        val bad = Part2(Map("x" -> (1, 2440), "m" -> (1, 4000), "a" -> (1, 4000), "s" -> (1, 1350)))

        if (next.find(_._1 == bad).isDefined) {
          debug = true
          println(s"found it $part0 curr: $curr next:")
          next.foreach(println)
        }

        next.flatMap { case (part, destination) =>
          check2(workflows, part, destination)
        }
    }
  }

  def createFailing(part: Part2, rule: Rule): Option[Part2] = {
    rule.condition match {
      case Some(Condition(category, ">", n)) =>
        val (min, max) = part.values(category)
        if (max > n) {
          val x = Some(part.copy(values = part.values.updated(category, (min, n))))
          // println(s"created failing part: $n")
          x
        } else {
          None
        }
      case Some(Condition(category, "<", n)) =>
        val (min, max) = part.values(category)
        if (min < n) {
          Some(part.copy(values = part.values.updated(category, (n, max))))
        } else {
          None
        }
      case None => None
      case _    => ???
    }
  }

  def createSucceeding(part: Part2, rule: Rule): Option[Part2] = {
    rule.condition match {
      case Some(Condition(category, ">", n)) =>
        val (min, max) = part.values(category)
        if (min < n && max > n) {
          Some(part.copy(values = part.values.updated(category, (n + 1L, max))))
        } else {
          None
        }
      case Some(Condition(category, "<", n)) =>
        val (min, max) = part.values(category)
        if (max > n && min < n) {
          Some(part.copy(values = part.values.updated(category, (min, n - 1L))))
        } else {
          None
        }
      case None => None
      case _    => ???
    }
  }

  def check(workflows: Seq[Workflow], part: Part, debug: Boolean = false): Long = {
    var destination = "in"

    var prev: Seq[Workflow] = Seq.empty
    while (destination != "A" && destination != "R") {
      val workflow = workflows.find(_.name == destination).get
      var rules = workflow.rules
      while (!matches(rules.head, part)) {
        rules = rules.tail
      }
      destination = rules.head.destination

      if (debug && destination == "R") {
        println(s"part $part failed at rule ${rules.head}")
        println(s"prev workflows: ")
        prev.foreach(println)
        println(workflow)
      }

      prev = prev :+ workflow
    }

    if (destination == "A") {
      part.x + part.m + part.a + part.s
    } else {
      0
    }
  }

  def matches(rule: Rule, part: Part) = {
    rule.condition match {
      case Some(condition) =>
        val value = condition.category match {
          case "x" => part.x
          case "m" => part.m
          case "a" => part.a
          case "s" => part.s
        }
        condition.op match {
          case ">" => value > condition.n
          case "<" => value < condition.n
        }
      case None => true
    }
  }

  def matches2(rule: Rule, part: Part2) = {
    rule.condition match {
      case Some(condition) =>
        val value = part.values(condition.category)

        assert(value._1 <= value._2)

        condition.op match {
          case ">" =>
            println(s"value: ${value._1} > ${condition.n} ?")
            value._1 > condition.n
          case "<" => value._2 < condition.n
        }
      case None => true
    }
  }

  def parse(input: Seq[String]) = {
    val splitted = split(input)
    val workflows = splitted.head.map { line =>
      line match {
        case s"$name{$rules}" =>
          Workflow(
            name,
            rules.split(",").toSeq.map { rule =>
              rule match {
                case s"$category>$n:$destination" => Rule(Some(Condition(category, ">", n.toInt)), destination)
                case s"$category<$n:$destination" => Rule(Some(Condition(category, "<", n.toInt)), destination)
                case s"$destination"              => Rule(None, destination)
              }
            }
          )
      }
    }

    val parts = splitted.last.map { line =>
      line match {
        case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)
      }
    }
    (workflows, parts)
  }
}
