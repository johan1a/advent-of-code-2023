package se.johan1a.adventofcode2023

object Day01 {

  def part1(input: Seq[String]): Int = {
    input.map{ line =>
      val first = line.toCharArray.find(_.isDigit).get
      val last = line.reverse.toCharArray.find(_.isDigit).get
      s"$first$last".toInt
    }.sum
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
