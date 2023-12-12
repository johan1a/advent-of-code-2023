package se.johan1a.adventofcode2023

import scala.collection.mutable.ArrayBuffer

object Day12 {

  def part1(input: Seq[String]): Long = {
    input
      .map(parse)
      .map { case (line, nbrs) =>
        cache = Map()
        nbrArrangements(line, nbrs, 0, 0)
      }
      .sum
  }

  def part2(input: Seq[String]): Long = {
    input
      .map(parse)
      .map(unfold)
      .map { case (line, nbrs) =>
        cache = Map()
        nbrArrangements(line, nbrs, 0, 0)
      }
      .sum
  }

  type Key = (Seq[Int], Int, Int)
  var cache = Map[Key, Long]()

  def nbrArrangements(chars: ArrayBuffer[Char], nbrs: Seq[Int], i: Int, currBroken: Int): Long = {
    val key = (nbrs, i, currBroken)
    if (cache.contains(key)) {
      cache(key)
    } else {
      val result: Long = if (nbrs.nonEmpty && nbrs.forall(currBroken > _)) {
        0
      } else if (i == chars.size) {
        if (nbrs.isEmpty && currBroken == 0) {
          1
        } else if (nbrs.size == 1 && nbrs.head == currBroken) {
          1
        } else {
          0
        }
      } else {
        chars(i) match {
          case '.' =>
            if (currBroken == 0) {
              nbrArrangements(chars, nbrs, i + 1, currBroken)
            } else if (nbrs.nonEmpty && nbrs.head == currBroken) {
              nbrArrangements(chars, nbrs.drop(1), i + 1, 0)
            } else {
              0
            }
          case '#' =>
            nbrArrangements(chars, nbrs, i + 1, currBroken + 1)
          case '?' =>
            val brokenResult = if (nbrs.nonEmpty) {
              nbrArrangements(chars, nbrs, i + 1, currBroken + 1)
            } else {
              0
            }

            val notBrokenResult = if (currBroken == 0) {
              nbrArrangements(chars, nbrs, i + 1, currBroken)
            } else if (nbrs.nonEmpty && nbrs.head == currBroken) {
              nbrArrangements(chars, nbrs.drop(1), i + 1, 0)
            } else {
              0
            }
            brokenResult + notBrokenResult
        }
      }
      cache = cache + (key -> result)
      result
    }
  }

  def unfold: ((ArrayBuffer[Char], Seq[Int])) => (ArrayBuffer[Char], Seq[Int]) = { case (chars, nbrs) =>
    val n = 5
    val newChars = ArrayBuffer[Char]()
    0.until(n).foreach { i =>
      newChars.addAll(chars)
      if (i < n - 1) {
        newChars.addOne('?')
      }
    }

    val newNbrs = Seq.fill(n)(nbrs).flatten

    (newChars, newNbrs)
  }

  def parse(line: String): (ArrayBuffer[Char], Seq[Int]) = {
    line match {
      case s"$chars $numbers" =>
        (ArrayBuffer().addAll(chars.toCharArray), numbers.split(",").map(_.toInt).toSeq)
    }
  }
}
