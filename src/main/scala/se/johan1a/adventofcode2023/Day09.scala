package se.johan1a.adventofcode2023

object Day09 {

  def part1(input: Seq[String]): Long = {
    val x= parse(input).map(getNextValue)
    println(x)
    x.sum
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  def getNextValue(numbers: Seq[Long]): Long = {
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
      println(stack)
    }


    var lastDiff = 0L
    while (stack.size > 0) {
      val currentRow = stack.head
      lastDiff = currentRow.last + lastDiff
      stack = stack.tail
      println(lastDiff)
    }

    lastDiff
  }

  def parse(input: Seq[String]) = {
    input.map(Utils.numbers)
  }
}
