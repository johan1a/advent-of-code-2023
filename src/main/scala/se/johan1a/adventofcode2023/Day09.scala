package se.johan1a.adventofcode2023

object Day09 {

  def part1(input: Seq[String]): Long = {
    parse(input).map(getNextValue).sum
  }

  def part2(input: Seq[String]): Long = {
    parse(input).map(getPrevValue).sum
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
          (b - a)
        }
        .toSeq
      stack = nextRow +: stack
      rowAbove = nextRow
    }
    stack
  }

  def getNextValue(numbers: Seq[Long]): Long = {
    var stack = extrapolate(numbers)

    var lastDiff = 0L
    while (stack.size > 0) {
      val currentRow = stack.head
      lastDiff = currentRow.last + lastDiff
      stack = stack.tail
    }

    lastDiff
  }

  def getPrevValue(numbers: Seq[Long]): Long = {
    var stack = extrapolate(numbers)

    var lastDiff = 0L
    while (stack.size > 0) {
      val currentRow = stack.head
      lastDiff = currentRow.head - lastDiff
      stack = stack.tail
    }

    lastDiff
  }

  def parse(input: Seq[String]) = {
    input.map(Utils.numbers)
  }
}
