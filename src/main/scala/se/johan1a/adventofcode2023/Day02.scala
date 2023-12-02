package se.johan1a.adventofcode2023

object Day02 {

  case class Game(id: Int, sets: Seq[Map[String, Int]])

  def part1(input: Seq[String]): Int = {
    input.map(parseLine).map(checkGame).sum
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  def checkGame(game: Game): Int = {
    val limits = Map("red" -> 12, "green" -> 13, "blue" -> 14)
    val possible = game.sets.forall { set =>
      set.forall { case (color, amount) =>
        val limit = limits.getOrElse(color, 0)
        amount <= limit
      }
    }
    if (possible) {
      game.id
    } else {
      0
    }
  }

  def parseLine(line: String): Game = {
    val split0 = line.split(": ")
    val id = split0.head.split("Game ").last.toInt
    val sets = split0.last.split("; ").map { set =>
      set.split(", ").map { str =>
        val colorAmount = str.split(" ")
        colorAmount.last -> colorAmount.head.toInt
      }.toMap
    }
    Game(id.toInt, sets)
  }
}
