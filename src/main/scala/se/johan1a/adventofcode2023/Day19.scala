package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._
import scala.annotation.tailrec

object Day19 {

  type Name = String
  type Category = String

  case class Condition(category: Category, op: String, n: Int)
  case class Rule(condition: Option[Condition], destination: Name)
  case class Workflow(name: Name, rules: Seq[Rule])

  case class Part(values: Map[Category, Long])

  case class Interval(min: Long, max: Long)
  case class PartIntervals(values: Map[Category, Interval])

  def part1(input: Seq[String]): Long = {
    val (workflows, parts) = parse(input)
    parts.map(p => isAccepted(workflows, p, "in")).sum
  }

  def part2(input: Seq[String], maxLimit: Int = 4000): Long = {
    val (workflows, _) = parse(input)
    val parts = findMatchingIntervals(
      workflows,
      PartIntervals(
        Map(
          "x" -> Interval(1, maxLimit),
          "m" -> Interval(1, maxLimit),
          "a" -> Interval(1, maxLimit),
          "s" -> Interval(1, maxLimit)
        )
      ),
      "in"
    )

    parts.map { part =>
      Seq("x", "m", "a", "s").map { category =>
        part.values(category).max - part.values(category).min + 1
      }.product
    }.sum
  }

  @tailrec
  def isAccepted(workflows: Seq[Workflow], part: Part, destination: String): Long = {
    destination match {
      case "R" => 0
      case "A" => part.values.values.sum
      case _ =>
        val workflow = workflows.find(_.name == destination).get
        var rules = workflow.rules
        while (!matches(rules.head, part)) {
          rules = rules.tail
        }
        isAccepted(workflows, part, rules.head.destination)
    }
  }

  def matches(rule: Rule, part: Part) = {
    rule.condition match {
      case Some(condition) =>
        val value = part.values(condition.category)
        condition.op match {
          case ">" => value > condition.n
          case "<" => value < condition.n
        }
      case None => true
    }
  }

  def findMatchingIntervals(workflows: Seq[Workflow], part: PartIntervals, curr: String): Seq[PartIntervals] =
    curr match {
      case "A" => Seq(part)
      case "R" => Seq.empty
      case _ =>
        val workflow = workflows.find(_.name == curr).get
        val partsToCheck = shrinkIntervals(workflow.rules, part)
        partsToCheck.flatMap((part, destination) => findMatchingIntervals(workflows, part, destination))
    }

  @tailrec
  def shrinkIntervals(
      rules: Seq[Rule],
      part: PartIntervals,
      partsToCheck: Seq[(PartIntervals, Name)] = Seq.empty
  ): Seq[(PartIntervals, Name)] =
    rules match {
      case Nil => partsToCheck
      case rule +: remainingRules =>
        var newPartsToCheck = Seq[(PartIntervals, Name)]()
        var nextPart: Option[PartIntervals] = Some(part)
        if (intervalMatches(rule, part)) {
          newPartsToCheck = newPartsToCheck :+ (part, rule.destination)
        } else {
          shrinkInterval(part, rule, shouldSucceed = true).map { newPart =>
            newPartsToCheck = newPartsToCheck :+ (newPart, rule.destination)
          }
          nextPart = shrinkInterval(part, rule, shouldSucceed = false)
        }
        nextPart match {
          case None       => partsToCheck ++ newPartsToCheck
          case Some(part) => shrinkIntervals(remainingRules, part, partsToCheck ++ newPartsToCheck)
        }
    }

  def shrinkInterval(part: PartIntervals, rule: Rule, shouldSucceed: Boolean): Option[PartIntervals] =
    rule.condition match {
      case None => None
      case Some(Condition(category, op, n)) =>
        (part.values(category), op) match {
          case (Interval(min, max), ">") =>
            if (shouldSucceed) {
              Some(part.copy(values = part.values.updated(category, Interval(n + 1L, max))))
            } else {
              Some(part.copy(values = part.values.updated(category, Interval(min, n))))
            }
          case (Interval(min, max), "<") =>
            if (shouldSucceed) {
              Some(part.copy(values = part.values.updated(category, Interval(min, n - 1L))))
            } else {
              Some(part.copy(values = part.values.updated(category, Interval(n, max))))
            }
        }
    }

  def intervalMatches(rule: Rule, part: PartIntervals) = {
    rule.condition match {
      case Some(condition) =>
        val value = part.values(condition.category)
        condition.op match {
          case ">" => value.min > condition.n
          case "<" => value.max < condition.n
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
        case s"{x=$x,m=$m,a=$a,s=$s}" =>
          Part(Map("x" -> x.toLong, "m" -> m.toLong, "a" -> a.toLong, "s" -> s.toLong))
      }
    }
    (workflows, parts)
  }
}
