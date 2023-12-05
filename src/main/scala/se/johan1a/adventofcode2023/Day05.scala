package se.johan1a.adventofcode2023

object Day05 {

  def part1(input: Seq[String]): Int = {
    val (seeds, conversions) = parse(input)
    val results = seeds.map(convert(conversions, "seed", _))
    results.min.toInt
  }

  def convert(conversions: Seq[Conversion], sourceLabel: String, source: Long): Long = {
    if (sourceLabel == "location") {
      source
    } else {
      val conversion = conversions.head
      val newSource = conversion.ranges.find { range =>
        range.contains(source)
      } match {
        case Some(range) => (source + range.increase)
        case None        => source
      }
      convert(conversions.drop(1), conversion.dest, newSource)
    }
  }

  def part2(input: Seq[String]): Int = {
    val (_, conversions) = parse(input)
    val seedRanges: Seq[Range] =
      Utils.numbers(input.head).grouped(2).map { nn => (Range(nn.head, nn.head + nn.last - 1, 0)) }.toSeq

    mapRanges(conversions, "seed", seedRanges).head.start.toInt
  }

  def mapRanges(conversions: Seq[Conversion], sourceLabel: String, sourceRanges: Seq[Range]): Seq[Range] = {
    if (conversions.isEmpty) {
      sourceRanges
    } else {
      val conversion = conversions.head

      var rangesA = sourceRanges.sortBy(_.end)
      var rangesB: Seq[Range] = conversion.ranges.sortBy(_.end)
      var result: Seq[Range] = Seq.empty
      while (rangesA.nonEmpty && rangesB.nonEmpty) {

        // "God, forgive me!"
        //    - Tommy Wiseau
        if (rangesA.head.start == rangesB.head.start) {
          val start = rangesA.head.start

          if (rangesB.head.end < rangesA.head.end) {
            val end = rangesB.head.end
            result = result :+ Range(start, end, rangesB.head.increase)
            rangesB = rangesB.drop(1)
            rangesA.head.start = end+1
          } else if (rangesA.head.end < rangesB.head.end) {
            val end = rangesA.head.end
            result = result :+ Range(start, end, rangesB.head.increase)
            rangesA = rangesA.drop(1)
            rangesB.head.start = end + 1
          } else {
            val end = rangesB.head.end
            result = result :+ Range(start, end, rangesB.head.increase)
            rangesB = rangesB.drop(1)
            rangesA = rangesA.drop(1)
          }
        } else if (rangesA.head.start < rangesB.head.start) {
          val start = rangesA.head.start

          if (rangesA.head.end < rangesB.head.start) {
            val end = rangesA.head.end
            result = result :+ Range(start, end, rangesA.head.increase)
            rangesA = rangesA.drop(1)
          } else {
            val end = rangesB.head.start - 1
            result = result :+ Range(start, end, rangesA.head.increase)
            rangesA.head.start = rangesB.head.start
          }
        } else {
          // rangesB.head.start < rangesA.head.start
          if (rangesB.head.end < rangesA.head.start) {
            // no mapping from source to dest for this interval, drop it
            rangesB = rangesB.drop(1)
          } else {
            // remove the part from B that falls outside the source interval
            rangesB.head.start = rangesA.head.start
          }
        }
      }

      result ++= rangesA

      result = result.map(range => Range(range.start + range.increase, range.end + range.increase, 0))
      result = mergeRanges(result.sortBy(_.start))

      mapRanges(conversions.drop(1), conversion.dest, result)
    }
  }

  def mergeRanges(allRanges: Seq[Range]): Seq[Range] = {
    var stack = Seq[Range](allRanges.head)
    var ranges = allRanges.tail
    while (ranges.nonEmpty) {
      val current = ranges.head
      ranges = ranges.tail

      if (stack.last.end < current.start) {
        stack = stack :+ current
      } else if (stack.last.end < current.end) {
        stack.last.end = current.end
      }
    }
    stack
  }

  def parse(input: Seq[String]) = {
    val splitted: Seq[Seq[String]] = Utils.split(input)
    val seeds = Utils.numbers(splitted.head.head)
    val conversions = splitted
      .drop(1)
      .map(parseConversion)
    (seeds, conversions)
  }

  case class Range(var start: Long, var end: Long, increase: Long) {
    def contains(n: Long) = start <= n && end >= n
  }

  case class Conversion(source: String, dest: String, ranges: Seq[Range])

  def parseConversion(lines: Seq[String]) = {
    val (source, dest) = lines.head match {
      case s"${source}-to-${dest} map:" =>
        (source, dest)
    }
    val ranges = lines
      .drop(1)
      .map(Utils.numbers)
      .map { numbers =>
        val dest = numbers.head
        val start = numbers(1)
        val size = numbers(2)
        val end = start + size - 1
        val increase = dest - start
        Range(start, end, increase)
      }
      .sortBy(_.end)
    Conversion(source, dest, ranges)
  }
}
