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
    println(s"input size ${input.size}")
    input
      .map(parse)
      .map(unfold)
      .map { case (line, nbrs) =>
        cache = Map()
        val r = nbrArrangements(line, nbrs, 0, 0)
        println(s"got $r")

        r
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
      val result: Long = if (nbrs.nonEmpty && nbrs.forall(n => currBroken > n)) {
        0
      } else if (i == chars.size) {
        if (nbrs.isEmpty && currBroken == 0) {
          1
        } else {
          if (nbrs.size == 1 && nbrs.head == currBroken) {
            1
          } else {
            0
          }
        }
      } else {
        chars(i) match {
          case '.' =>
            if (currBroken == 0) {
              nbrArrangements(chars, nbrs, i + 1, currBroken)
            } else {
              if (nbrs.nonEmpty && nbrs.head == currBroken) {
                nbrArrangements(chars, nbrs.drop(1), i + 1, 0)
              } else {
                0
              }
            }
          case '#' =>
            if (currBroken == 0) {
              nbrArrangements(chars, nbrs, i + 1, currBroken + 1)
            } else {
              nbrArrangements(chars, nbrs, i + 1, currBroken + 1)
            }
          case '?' =>
            // chars(i) = '#'
            val a = if (nbrs.nonEmpty) {
              if (currBroken == 0) {
                nbrArrangements(chars, nbrs, i + 1, currBroken + 1)
              } else {
                nbrArrangements(chars, nbrs, i + 1, currBroken + 1)
              }
            } else {
              0
            }
            // chars(i) = '?'

            // chars(i) = '.'
            val b = if (currBroken == 0) {
              nbrArrangements(chars, nbrs, i + 1, currBroken)
            } else {
              if (nbrs.nonEmpty && nbrs.head == currBroken) {
                nbrArrangements(chars, nbrs.drop(1), i + 1, 0)
              } else {
                0
              }
            }
            // chars(i) = '?'
            a + b
        }
      }
      if (cache.contains(key) && cache(key) != result) {
        println(
          s"i $i nbrs $nbrs broken $currBroken ${chars.mkString("")} key $key result $result cached ${cache(key)}"
        )
      }
      cache = cache + (key -> result)
      result
    }
  }

  def unfold(charsNumbers: (ArrayBuffer[Char], Seq[Int])): (ArrayBuffer[Char], Seq[Int]) = {
    val originalChars = charsNumbers.head
    val chars = ArrayBuffer[Char]()
    var nbrs = Seq[Int]()

    0.until(5).foreach { i =>
      chars.addAll(originalChars)
      if (i < 4) {
        chars.addOne('?')
      }
    }
    0.until(5).map { _ =>
      nbrs = nbrs ++ charsNumbers.last
    }

    (chars, nbrs)
  }

  def parse(line: String): (ArrayBuffer[Char], Seq[Int]) = {
    line match {
      case s"$chars $numbers" =>
        (ArrayBuffer().addAll(chars.toCharArray), numbers.split(",").map(_.toInt).toSeq)
    }
  }
}
