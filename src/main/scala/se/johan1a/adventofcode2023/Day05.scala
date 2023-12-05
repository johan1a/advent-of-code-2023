package se.johan1a.adventofcode2023

object Day05 {

  def part1(input: Seq[String]): Int = {
    val (seeds, conversions) = parse(input)
    val results = seeds.map(convert(conversions, "seed", _))
    results.min.toInt
  }

  def convert(conversions: Seq[Conversion], sourceLabel: String, source: BigInt): BigInt = {
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
    val seedRanges: Seq[(BigInt, BigInt)] =
      Utils.numbers(input.head).grouped(2).map { nn => (BigInt(nn.head), BigInt(nn.head + nn.last - 1)) }.toSeq
    // conversions.foreach(println)

    var best: Option[BigInt] = None
    seedRanges.foreach { case (start, end) =>
      start.to(end).foreach { seed =>
        val result = (convert(conversions, "seed", seed))
        if (best.isEmpty || result < best.get) {
          best = Some(result)
          println(s"best: $best")
        }
      }
    // println(s"start $start, end $end, r $r")
    }

    best.get.toInt
  }

  //def part2Wip(input: Seq[String]): Int = {
  //  val (_, conversions) = parse(input)
  //  val seedRanges: Seq[(BigInt, BigInt)] =
  //    Utils.numbers(input.head).grouped(2).map { nn => (BigInt(nn.head), BigInt(nn.head + nn.last - 1)) }.toSeq

  //  var best: Option[BigInt] = None
  //  seedRanges.foreach { case (start, end) =>
  //    //val result: Seq[(BigInt, BigInt)] = (convert2(conversions, "seed", Seq(Range(start, end, 0))))
  //    // TODO check best
  //  }

  //  best.get.toInt
  //}

  // def convert2(conversions: Seq[Conversion], sourceLabel: String, sourceRanges: Seq[Range]) = {
  //   if(conversions.isEmpty){
  //     sourceRanges
  //   } else {

  //   val conversion = conversions.head
  //   var rangesA: Seq[Range] = conversion.ranges
  //   var rangesB = sourceRanges
  //   var result: Seq[Range] = Seq.empty
  //   while (rangesA.nonEmpty && rangesB.nonEmpty) {
  //   }

  //   }
  // }

  def parse(input: Seq[String]) = {
    val splitted: Seq[Seq[String]] = Utils.split(input)
    val seeds = Utils.numbers(splitted.head.head)
    val conversions = splitted
      .drop(1)
      .map(parseConversion)
    (seeds, conversions)
  }

  case class Range(start: BigInt, end: BigInt, increase: BigInt) {
    def contains(n: BigInt) = start <= n && end >= n
  }

  case class Conversion(source: String, dest: String, ranges: Seq[Range])

  def parseConversion(lines: Seq[String]) = {
    val (source, dest) = lines.head match {
      case s"${source}-to-${dest} map:" =>
        (source, dest)
    }
    val ranges = lines.drop(1).map(Utils.numbers).map { numbers =>
      val dest = numbers.head
      val start = numbers(1)
      val size = numbers(2)
      val end = start + size - 1
      val increase = dest - start
      Range(start, end, increase)
    }.sortBy(_.end)
    Conversion(source, dest, ranges)
  }
}
