package se.johan1a.adventofcode2023

object Day04 {

  def part1(input: Seq[String]): Int = {
    input.map(parseLine).map(check).sum
  }

  def part2(input: Seq[String]): Int = {
    val cards: Map[Int, (Seq[Int], Seq[Int])] = input.map(parseLine).zipWithIndex.map { case (card, i ) => (i+1) -> card  }.toMap
    check2(cards)
  }

  def check2(cards: Map[Int, (Seq[Int], Seq[Int])]): Int = {
   var amounts = Map[Int, Int]()

    1.to(cards.size).foreach { n =>
      amounts = amounts + (n -> 1)
    }


    1.to(cards.size).foreach { n =>
      val (winning, mine) = cards(n)
      val nbrMatching: Int = winning.filter(mine.contains).size

      val won = (n + 1).to(n + nbrMatching)
      val amountOfCurrent = amounts(n)
      won.foreach { w =>
        amounts = amounts + (w -> (amountOfCurrent + amounts.getOrElse(w, 0)))
      }
    }

    amounts.values.sum
  }

  def parseLine(line: String): (Seq[Int], Seq[Int]) = {
    val splitted = line.split(": ").last
    val winningAndMine = splitted.split('|')
    val winning = winningAndMine.head.split(" ").filter(_.nonEmpty).map(_.trim.toInt).toSeq
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
