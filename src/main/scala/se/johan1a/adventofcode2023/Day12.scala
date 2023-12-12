package se.johan1a.adventofcode2023

import scala.collection.mutable.ArrayBuffer

object Day12 {

  def part1(input: Seq[String]): Int = {
    println(s"\n")
    input.map(parse).map { case (line, nbrs) => nbrArrangements(line, nbrs, 0, 0) }.sum
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  def nbrArrangements(chars: ArrayBuffer[Char], nbrs: Seq[Int], i: Int, currBroken: Int): Int = {
    if (i == chars.size) {
      if (nbrs.isEmpty && currBroken == 0) {
        println(s"i $i broken $currBroken $nbrs ${chars.mkString("")}")
        1
      } else {
        if (nbrs.size==1 && nbrs.head == currBroken) {
          println(s"i $i broken $currBroken $nbrs ${chars.mkString("")}")
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
          nbrArrangements(chars, nbrs, i + 1, currBroken + 1)
        case '?' =>
          chars(i) = '#'
          val a = nbrArrangements(chars, nbrs, i + 1, currBroken + 1)
          chars(i) = '?'

          chars(i) = '.'
          val b = if (currBroken == 0) {
            nbrArrangements(chars, nbrs, i + 1, currBroken)
          } else {
            if (nbrs.nonEmpty && nbrs.head == currBroken) {
              nbrArrangements(chars, nbrs.drop(1), i + 1, 0)
            } else {
              0
            }
          }
          chars(i) = '?'
          a + b
      }
    }
  }

  def parse(line: String): (ArrayBuffer[Char], Seq[Int]) = {
    line match {
      case s"$chars $numbers" =>
        (ArrayBuffer().addAll(chars.toCharArray), numbers.split(",").map(_.toInt).toSeq)
    }
  }
}
