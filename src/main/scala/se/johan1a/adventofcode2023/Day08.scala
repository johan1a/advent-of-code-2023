package se.johan1a.adventofcode2023

import se.johan1a.adventofcode2023.Utils._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day08 {

  def part1(input: Seq[String]): Int = {
    val (instructions, nodes) = parse(input)
    count(instructions, nodes, "AAA", 0, 0)
  }

  def part2(input: Seq[String]): Long = {
    val (instructions, nodes) = parse(input)
    val startingPositions = nodes.keys.map(_._1).filter(_.endsWith("A")).toSeq.sorted
    val periods = getPeriods(instructions, nodes, startingPositions)
    lcm(periods)
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

  type Id = Int
  type NbrSteps = Long
  type Period = Long
  type InstructionIndex = Int

  def getPeriods(
      instructions: ArrayBuffer[String],
      nodes: Map[(String, String), String],
      startingPositions: Seq[String]
  ): Seq[Period] = {
    var seenStates = Map[(Id, String, InstructionIndex), NbrSteps]()
    var periods = Map[Id, Period]()
    var nbrSteps: NbrSteps = 0
    var currentPositions = startingPositions
    while (periods.size < startingPositions.size) {

      val instructionIndex = (nbrSteps % instructions.size).toInt
      val instruction = instructions(instructionIndex)

      currentPositions = currentPositions.zipWithIndex.map { case (pos, id) =>
        if (seenStates.contains((id, pos, instructionIndex)) && !periods.contains(id)) {
          val startOfPeriod = seenStates((id, pos, instructionIndex))
          val period = nbrSteps - startOfPeriod
          periods = periods + (id -> period)
        }
        if (!seenStates.contains((id, pos, instructionIndex))) {
          seenStates = seenStates + ((id, pos, instructionIndex) -> nbrSteps)
        }

        nodes((pos, instruction))
      }

      nbrSteps += 1
    }

    periods.values.toSeq
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
