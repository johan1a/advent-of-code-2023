package se.johan1a.adventofcode2023

object Day06 {

  def part1(input: Seq[String]): Int = {
    val (durations, records) = parse(input)
    0.until(durations.size)
      .map { i =>
        check(durations(i), records(i))
      }
      .product
      .toInt
  }

  def part2(input: Seq[String]): Int = {
    val (duration, distance) = parse2(input)
    check2(duration, distance).toInt
  }

  def check(duration: Long, record: Long): Long = {
    0.until(duration.toInt)
      .map { minutesWaited =>
        val speed = minutesWaited
        speed * (duration - minutesWaited)
      }
      .filter { result =>
        result > record
      }
      .size
  }

  def check2(duration: Long, distance: Long): Long = {
    val startOfWins = search(duration, distance, true)
    val endOfWins = search(duration, distance, false)
    endOfWins - startOfWins
  }

  def search(duration: Long, distance: Long, shouldWin: Boolean): Long = {
    var l = 0L
    var r = duration
    var mid = -1L
    while (l <= r) {
      mid = l + (r - l) / 2L

      val resultBefore = wins(duration, distance, mid - 1)
      val result = wins(duration, distance, mid)
      if (result == shouldWin && resultBefore != shouldWin) {
        return mid
      } else if (result != shouldWin && resultBefore != shouldWin) {
        l = mid + 1
      } else if (result == shouldWin && resultBefore == shouldWin) {
        r = mid - 1
      }
    }
    -1
  }

  def wins(duration: Long, distance: Long, minutesWaited: Long): Boolean = {
    val speed = minutesWaited
    (speed * (duration - minutesWaited)) > distance
  }

  def parse(lines: Seq[String]) = {
    (Utils.numbers(lines.head), Utils.numbers(lines.last))
  }

  def parse2(lines: Seq[String]) = {
    val duration = lines.head.split("Time:").last.replaceAll(" ", "").toLong
    val record = lines.last.split("Distance:").last.replaceAll(" ", "").toLong
    (duration, record)
  }
}
