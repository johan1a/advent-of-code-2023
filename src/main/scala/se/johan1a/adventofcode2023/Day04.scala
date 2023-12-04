package se.johan1a.adventofcode2023

object Day04 {

  def part1(input: Seq[String]): Int = {
    input.map(parseLine).map(check).sum
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  def parseLine(line: String): (Seq[Int], Seq[Int]) = {
    val splitted = line.split(": ").last
    println(splitted.toSeq)
    val winningAndMine = splitted.split('|')
    println(winningAndMine.toSeq)
    val winning = winningAndMine.head.split(" ").filter(_.nonEmpty).map(_.trim.toInt).toSeq
    println(winningAndMine.head.split(" ").toSeq)
    val mine = winningAndMine.last.split(" ").filter(_.nonEmpty).map(_.trim.toInt).toSeq
    (winning, mine)
  }

  def check(input: (Seq[Int], Seq[Int])): Int = {
    input match {
      case (winning: Seq[Int], mine: Seq[Int]) => {
        val nbrWinning = winning.filter(mine.contains).size
        if (nbrWinning > 0) {
          Math.pow(2, nbrWinning - 1).toInt
        } else {
          0
        }
      }
    }
  }

}
