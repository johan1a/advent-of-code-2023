package se.johan1a.adventofcode2023

import scala.collection.mutable.ArrayBuffer

object Day08 {

  def part1(input: Seq[String]): Int = {
    val (instructions, nodes) = parse(input)
    println(instructions)
    println(nodes)
    count(instructions, nodes, "AAA", 0, 0)
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  def count(
      instructions: ArrayBuffer[String],
      nodes: Map[(String, String), String],
      pos: String,
      stepsTaken: Int,
      i: Int
  ): Int = {
    if (pos == "ZZZ") {
      stepsTaken
    } else {
      val instruction = instructions(i)
      val nextPos = nodes((pos, instruction))
      count(instructions, nodes, nextPos, stepsTaken + 1, (i + 1) % instructions.size)
    }
  }

  def parse(input: Seq[String]): (ArrayBuffer[String], Map[(String, String), String]) = {
    val instructions = ArrayBuffer().appendAll(input.head.toArray).map(_.toString)
    val nodes = input
      .drop(2)
      .flatMap { line =>
        line match {
          case s"$src = ($left, $right)" =>
            Seq(
              (src, "L") -> left,
              (src, "R") -> right
            )
        }
      }
      .toMap

    (instructions, nodes)
  }
}
