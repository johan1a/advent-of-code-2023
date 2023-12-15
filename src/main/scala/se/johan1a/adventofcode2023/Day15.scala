package se.johan1a.adventofcode2023

object Day15 {

  def part1(input: Seq[String]): Int = {
    input.head
      .split(",")
      .map { str =>
        hash(str)
      }
      .sum
  }

  def hash(string: String): Int = {
    string.toCharArray.foldLeft(0) { (acc, char) =>
      var value = acc
      value += char.toInt
      (value * 17) % 256
    }
  }

  def part2(input: Seq[String]): Int = {
    val finalMap = parse(input).foldLeft(Map[Int, Seq[Lens]]()) { case (map, instruction) =>
      instruction match {
        case Set(label, n) =>
          val h = hash(label)
          val lenses = map.getOrElse(h, Seq.empty)
          lenses.find(_.label == label) match {
            case Some(oldLens) =>
              oldLens.n = n
              map
            case None =>
              map + (h -> (lenses :+ Lens(label, n)))
          }
        case Remove(label) =>
          val h = hash(label)
          val lenses = map.getOrElse(h, Seq.empty)
          map + (h -> lenses.filterNot(_.label == label))
      }
    }
    finalMap.map { case (h, lenses) =>
      lenses.zipWithIndex.map { case (lens, i) =>
        (h + 1) * (i + 1) * lens.n
      }.sum
    }.sum
  }

  sealed trait Instruction
  case class Set(label: String, n: Int) extends Instruction
  case class Remove(label: String) extends Instruction

  case class Lens(label: String, var n: Int)

  def parse(input: Seq[String]) = {
    input.head.split(",").map { str =>
      str match {
        case s"$label=$n" => Set(label, n.toInt)
        case s"$label-"   => Remove(label)
      }
    }
  }
}
