package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

object Day19 {

  case class Condition(category: String, op: String, n: Int)
  case class Rule(condition: Option[Condition], destination: String)
  case class Workflow(name: String, rules: Seq[Rule])

  case class Part(x: Int, m: Int, a: Int, s: Int)

  def part1(input: Seq[String]): Int = {
    val (workflows, parts) = parse(input)
    parts.map(p => check(workflows, p)).sum
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  def check(workflows: Seq[Workflow], part: Part): Int = {
    var destination = "in"

    var i = 0
    val maxIter = 1000

    while (i < maxIter && destination != "A" && destination != "R") {
      val workflow = workflows.find(_.name == destination).get
      var rules = workflow.rules
      while (!matches(rules.head, part)) {
        rules = rules.tail
      }
      // println(s"part $part matched rule ${rules.head}. curr: $destination: new destination: ${rules.head.destination}")
      destination = rules.head.destination
      i += 1
    }

    if (destination == "A") {
      val score = part.x + part.m + part.a + part.s
      // println(score)
      score
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
        val result = condition.op match {
          case ">" => value > condition.n
          case "<" => value < condition.n
        }

        // println(s"part $part matches $rule? $result")
        result
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
