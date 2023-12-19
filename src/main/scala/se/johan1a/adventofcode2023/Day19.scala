package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day19 {

  case class Condition(category: String, op: String, n: Int)
  case class Rule(condition: Option[Condition], destination: String)
  case class Workflow(name: String, rules: Seq[Rule])

  case class Part(var values: Map[String, Long])
  case class Part2(var values: Map[String, (Long, Long)])

  def part1(input: Seq[String]): Long = {
    val (workflows, parts) = parse(input)
    parts.map(p => check(workflows, p)).sum
  }

  def part2(input: Seq[String], maxLimit: Int = 4000, debug: Boolean = false): Long = {
    val (workflows, _) = parse(input)
    val parts = check2(
      workflows,
      Part2(Map("x" -> (1L, maxLimit), "m" -> (1L, maxLimit), "a" -> (1, maxLimit), "s" -> (1, maxLimit))),
      "in"
    )

    parts.map { part =>
      Seq("x", "m", "a", "s").map { category =>
        part.values(category)._2 - part.values(category)._1 + 1
      }.product
    }.sum
  }

  def check2(workflows: Seq[Workflow], part0: Part2, curr: String): Seq[Part2] = {
    curr match {
      case "A" => Seq(part0)
      case "R" => Seq.empty
      case _ =>
        var rules = workflows.find(_.name == curr).get.rules
        var next = Seq[(Part2, String)]()
        var partOpt: Option[Part2] = Some(part0)
        while (rules.nonEmpty && partOpt.isDefined) {
          val part = partOpt.get
          val rule = rules.head

          if (matches2(rule, part)) {
            next = next :+ (part, rule.destination)
          } else {
            createSucceeding(part, rule).map { newPart =>
              next = next :+ (newPart, rule.destination)
            }
            partOpt = createFailing(part, rule)
          }
          rules = rules.tail
        }
        next.flatMap((part, destination) => check2(workflows, part, destination))
    }
  }

  def createFailing(part: Part2, rule: Rule): Option[Part2] =
    rule.condition match {
      case Some(Condition(category, op, n)) =>
        val (min, max) = part.values(category)
        if (op == ">" && max > n) {
          Some(part.copy(values = part.values.updated(category, (min, n))))
        } else if (op == "<" && min < n) {
          Some(part.copy(values = part.values.updated(category, (n, max))))
        } else {
          None
        }
      case None => None
    }

  def createSucceeding(part: Part2, rule: Rule): Option[Part2] =
    rule.condition match {
      case Some(Condition(category, op, n)) =>
        val (min, max) = part.values(category)
        if (op == ">" && max > n) {
          Some(part.copy(values = part.values.updated(category, (n + 1L, max))))
        } else if (op == "<" && min < n) {
          Some(part.copy(values = part.values.updated(category, (min, n - 1L))))
        } else {
          None
        }
      case None => None
    }

  def check(workflows: Seq[Workflow], part: Part): Long = {
    var destination = "in"
    var prev: Seq[Workflow] = Seq.empty
    while (destination != "A" && destination != "R") {
      val workflow = workflows.find(_.name == destination).get
      var rules = workflow.rules
      while (!matches(rules.head, part)) {
        rules = rules.tail
      }
      destination = rules.head.destination
      prev = prev :+ workflow
    }

    if (destination == "A") {
      part.values.values.sum
    } else {
      0
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

  def matches2(rule: Rule, part: Part2) = {
    rule.condition match {
      case Some(condition) =>
        val value = part.values(condition.category)
        condition.op match {
          case ">" => value._1 > condition.n
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
        case s"{x=$x,m=$m,a=$a,s=$s}" =>
          Part(Map("x" -> x.toLong, "m" -> m.toLong, "a" -> a.toLong, "s" -> s.toLong))
      }
    }
    (workflows, parts)
  }
}
