package se.johan1a.adventofcode2023

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day08 {

  def part1(input: Seq[String]): Int = {
    val (instructions, nodes) = parse(input)
    count(instructions, nodes, "AAA", 0, 0)
  }

  def part2(input: Seq[String]): Long = {
    val (instructions, nodes) = parse(input)
    val startingPositions = nodes.keys.map(_._1).filter(_.endsWith("A")).toSet
    count2(instructions, nodes, startingPositions, 0, 0)
  }

  @tailrec
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

  @tailrec
  def count2(
      instructions: ArrayBuffer[String],
      nodes: Map[(String, String), String],
      currentPositions: Set[String],
      stepsTaken: Long,
      i: Int
  ): Long = {
    if (stepsTaken % 100000 == 0) {
      println(stepsTaken)
    }

    if (currentPositions.forall(_.endsWith("Z"))) {
      stepsTaken
    } else {
      val instruction = instructions(i)

      val nextPositions = currentPositions.map { pos =>
        nodes((pos, instruction))
      }

      count2(instructions, nodes, nextPositions, stepsTaken + 1, (i + 1) % instructions.size)
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
