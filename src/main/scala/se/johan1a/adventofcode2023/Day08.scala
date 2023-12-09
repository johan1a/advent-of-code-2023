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
    val startingPositions = nodes.keys.map(_._1).filter(_.endsWith("A")).toSeq.sorted
    val (periods, seenStates) = getPeriods(instructions, nodes, startingPositions)
    println(periods.map(_._3))
    0.until(startingPositions.size).map { id =>
      // println(seenStates.filter { case (k, v) => k._1 == id }.toSeq)
    }
    count2(instructions, nodes, seenStates, periods, startingPositions)
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
  type StartRepeat = Long
  type InstructionIndex = Int

  def count2(
      instructions: ArrayBuffer[String],
      nodes: Map[(String, String), String],
      seenStates: Map[(Id, NbrSteps), String],
      allPeriods: Seq[(Id, StartRepeat, Period)],
      startingPositions: Seq[String]
  ) = {
    var period: Period = 1
    var periodsLeft = allPeriods
    val initialSteps = periodsLeft.head._2
    var nextPeriod = periodsLeft.head._3

    var prevIds: Seq[Id] = Seq()

    var nextPeriodId: Id = periodsLeft.head._1
    periodsLeft = periodsLeft.tail

    // println("initial jump")
    var currentPositions = jump(seenStates, allPeriods, startingPositions, 0, initialSteps)
    var currentNbrSteps = initialSteps
    var instructionIndex = currentNbrSteps % instructions.size

    val maxIter = 6000000L

    while (!finished(currentPositions) && currentNbrSteps < maxIter) {

      // println(s"currentPositions $currentPositions nbrSteps $currentNbrSteps")
      if (currentNbrSteps % 1000000 == 0) {
        println(currentNbrSteps)
      }

      if (endsWithZ(currentPositions, prevIds :+ nextPeriodId)) {
        period = nextPeriod
        prevIds = prevIds :+ nextPeriodId
        if (periodsLeft.isEmpty) {
           println(s"empty at steps $currentNbrSteps")
        }
        nextPeriodId = periodsLeft.head._1
        nextPeriod = periodsLeft.head._3
        periodsLeft = periodsLeft.tail
        println(s"new period $period nbr steps $currentNbrSteps periods left: $periodsLeft matched ids: $prevIds")
      }

      currentPositions = jump(seenStates, allPeriods, currentPositions, currentNbrSteps, period)
      instructionIndex = currentNbrSteps % instructions.size
      currentNbrSteps += period
    }
    currentNbrSteps
  }

  // TODO check that all have started repeating first?
  def jump(
      seenStates: Map[(Id, NbrSteps), String],
      allPeriods: Seq[(Id, StartRepeat, Period)],
      currentPositions: Seq[String],
      nbrSteps: NbrSteps,
      jumpSize: Long
  ) = {
    val nextPositions = currentPositions.zipWithIndex.map { case (pos, id) =>
      val (_, start, period) = allPeriods(id)
      val stateKey = (id, start + (nbrSteps - start + jumpSize) % period)
      val nextPos = seenStates(stateKey)
      // println(s"jump id $id nbrSteps $nbrSteps jumpSize $jumpSize period $period key $stateKey pos $pos nextpos $nextPos")
      nextPos
    }

    nextPositions
  }

  def finished(positions: Seq[String]): Boolean = positions.forall(_.endsWith("Z"))

  def endsWithZ(positions: Seq[String], idsToMatch: Seq[Int]): Boolean = {
    idsToMatch.forall { id =>
      positions(id).endsWith("Z")
    }
  }

  def getPeriods(
      instructions: ArrayBuffer[String],
      nodes: Map[(String, String), String],
      startingPositions: Seq[String]
  ) = {
    var seenStates = Map[(Id, String, InstructionIndex), NbrSteps]()
    var posCache = Map[(Id, NbrSteps), String]()
    var periods = Map[Id, (StartRepeat, Period)]()
    var nbrSteps: NbrSteps = 0
    var currentPositions = startingPositions
    while (periods.size < startingPositions.size) {

      val instructionIndex = (nbrSteps % instructions.size).toInt
      val instruction = instructions(instructionIndex)

      currentPositions = currentPositions.zipWithIndex.map { case (pos, id) =>
        if (seenStates.contains((id, pos, instructionIndex)) && !periods.contains(id)) {
          val startOfPeriod = seenStates((id, pos, instructionIndex))
          val period = nbrSteps - startOfPeriod
          periods = periods + (id -> (nbrSteps - period, period))
        }
        if (!seenStates.contains((id, pos, instructionIndex))) {
          seenStates = seenStates + ((id, pos, instructionIndex) -> nbrSteps)
        }
        posCache = posCache + ((id, nbrSteps) -> pos)

        val nextPos = nodes((pos, instruction))
        // println(s"id $id nbrSteps $nbrSteps pos $pos nextPos $nextPos")
        nextPos
      }
      // //println(seenStates)

      nbrSteps += 1
    }

    val sortedPeriods = periods
      .map { case (id, startAndPeriod) =>
        (id, startAndPeriod._1, startAndPeriod._2)
      }
      .toSeq
      .sortBy(_._3)

    (sortedPeriods, posCache)
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
