package se.johan1a.adventofcode2023

object Day01 {

  def part1(input: Seq[String]): Int = {
    input.map { line =>
      val first = line.toCharArray.find(_.isDigit).get
      val last = line.reverse.toCharArray.find(_.isDigit).get
      s"$first$last".toInt
    }.sum
  }

  def part2(input: Seq[String]): Int = {
    input.map { line =>
      val first = findDigit(line, 0, 1)
      val last = findDigit(line, line.size-1, -1)

      val result = s"$first$last".toInt
      println(s"$line, $first, $last, $result")
      result
    }.sum
  }

  val digits = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  def findDigit(line: String, start: Int, direction: Int): Int = {
    var i = start
    while (i < line.size) {
      if (line(i).isDigit) {
        println(s"digit ${line(i)} at $i")
        return line(i).toString.toInt
      } else {
        val found = digits
          .find { case (digit, value) =>
            (line.substring(i).startsWith(digit))
          }
        if (found.isDefined) {
          println(s"digit ${found.get._1} at $i")
          return found.get._2
        }
      }

      i += direction
    }
    -1
  }
}
