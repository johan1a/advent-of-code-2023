package se.johan1a.adventofcode2023

object Day15 {

  def part1(input: Seq[String]): Int = {
    input.head.split(",").map { str =>
      hash(str)
    }.sum
  }

  def hash(string: String): Int = {
    string.toCharArray.foldLeft(0) { (acc, char) =>
      var value = acc
      value += char.toInt
      (value * 17) % 256
    }
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
