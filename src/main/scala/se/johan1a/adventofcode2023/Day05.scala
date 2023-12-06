package se.johan1a.adventofcode2023

object Day05 {

  case class Range(var start: Long, var end: Long, increase: Long) {
    def contains(n: Long) = start <= n && end >= n
  }

  case class Conversion(source: String, dest: String, ranges: Seq[Range])

  def part1(input: Seq[String]): Int = {
    val (seeds, conversions) = parse(input)
    seeds.map(convert(conversions, _)).min.toInt
  }

  def convert(conversions: Seq[Conversion], source: Long): Long = {
    conversions match {
      case Nil => source
      case conversion +: tail =>
        val newSource = conversion.ranges.find { range =>
          range.contains(source)
        } match {
          case Some(range) => (source + range.increase)
          case None        => source
        }
        convert(tail, newSource)
    }
  }

  def part2(input: Seq[String]): Int = {
    val (seedRanges, conversions) = parse2(input)
    mapRanges(conversions, seedRanges).head.start.toInt
  }

  def mapRanges(conversions: Seq[Conversion], sourceRanges: Seq[Range]): Seq[Range] = {
    conversions match {
      case Nil => sourceRanges
      case conversion +: tail =>
        var rangesFrom = sourceRanges
        var rangesTo: Seq[Range] = conversion.ranges
        var result: Seq[Range] = Seq.empty

        while (rangesFrom.nonEmpty && rangesTo.nonEmpty) {
          val nextFrom = rangesFrom.head
          val nextTo = rangesTo.head

          // "God, forgive me!"
          //    - Tommy Wiseau
          if (nextFrom.start == nextTo.start) {
            val start = nextFrom.start

            if (nextTo.end < nextFrom.end) {
              val end = nextTo.end
              result = result :+ Range(start, end, nextTo.increase)
              rangesTo = rangesTo.drop(1)
              nextFrom.start = end + 1
            } else if (nextFrom.end < nextTo.end) {
              val end = nextFrom.end
              result = result :+ Range(start, end, nextTo.increase)
              rangesFrom = rangesFrom.drop(1)
              nextTo.start = end + 1
            } else {
              val end = nextTo.end
              result = result :+ Range(start, end, nextTo.increase)
              rangesTo = rangesTo.drop(1)
              rangesFrom = rangesFrom.drop(1)
            }
          } else if (nextFrom.start < nextTo.start) {
            val start = nextFrom.start

            if (nextFrom.end < nextTo.start) {
              val end = nextFrom.end
              result = result :+ Range(start, end, nextFrom.increase)
              rangesFrom = rangesFrom.drop(1)
            } else {
              val end = nextTo.start - 1
              result = result :+ Range(start, end, nextFrom.increase)
              nextFrom.start = nextTo.start
            }
          } else {
            if (nextTo.end < nextFrom.start) {
              // no mapping from source to dest for this interval, drop it
              rangesTo = rangesTo.drop(1)
            } else {
              // remove the part from rangesTo that falls outside the source interval
              nextTo.start = nextFrom.start
            }
          }
        }

        result ++= rangesFrom
        result = result.map(range => Range(range.start + range.increase, range.end + range.increase, 0))
        result = mergeRanges(result.sortBy(_.start))

        mapRanges(tail, result)
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

  def parse2(input: Seq[String]) = {
    val (_, conversions) = parse(input)
    val seedRanges: Seq[Range] =
      Utils
        .numbers(input.head)
        .grouped(2)
        .map { nn => (Range(nn.head, nn.head + nn.last - 1, 0)) }
        .toSeq
        .sortBy(_.end)
    (seedRanges, conversions)
  }

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
