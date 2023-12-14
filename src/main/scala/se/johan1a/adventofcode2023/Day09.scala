package se.johan1a.adventofcode2023

object Day09 {

  def part1(input: Seq[String]): Long = {
    parse(input).map(getEndValue).sum
  }

  def part2(input: Seq[String]): Long = {
    parse(input).map(_.reverse).map(getEndValue).sum
  }

  def getEndValue(numbers: Seq[Long]): Long = {
    extrapolate(numbers).foldLeft(0L)((lastDiff, currentRow) => currentRow.last + lastDiff)
  }

  def extrapolate(numbers: Seq[Long]): Seq[Seq[Long]] = {
    var stack = Seq[Seq[Long]](numbers)
    var rowAbove = stack.head
    while (!rowAbove.forall(_ == 0)) {
      val nextRow = rowAbove
        .sliding(2)
        .map { nn =>
          val a = nn.head
          val b = nn.last
          b - a
        }
        .toSeq
      stack = nextRow +: stack
      rowAbove = nextRow
    }
    stack
  }

  def parse(input: Seq[String]) = {
    input.map(Utils.numbers)
  }
}
