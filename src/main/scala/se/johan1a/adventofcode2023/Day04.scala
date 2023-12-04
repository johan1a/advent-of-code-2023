package se.johan1a.adventofcode2023

object Day04 {

  case class Card(cardNbr: Int, winning: Seq[Int], mine: Seq[Int])

  def part1(input: Seq[String]): Int = {
    input.map(parseLine).map(check).sum
  }

  def part2(input: Seq[String]): Int = {
    val cards: Map[Int, Card] = input
      .map(parseLine)
      .map(card => card.cardNbr -> card)
      .toMap
    check2(cards)
  }

  def check(card: Card): Int = {
    card match {
      case Card(_: Int, winning: Seq[Int], mine: Seq[Int]) => {
        val nbrWinning = winning.filter(mine.contains).size
        if (nbrWinning > 0) {
          Math.pow(2, nbrWinning - 1).toInt
        } else {
          0
        }
      }
    }
  }

  def check2(cards: Map[Int, Card]): Int = {
    var amounts: Map[Int, Int] = 1
      .to(cards.size)
      .map(n => (n -> 1))
      .toMap

    1.to(cards.size).foreach { cardNbr =>
      val card = cards(cardNbr)
      val nbrMatching = card.winning.filter(card.mine.contains).size
      val wonCards = (cardNbr + 1).to(cardNbr + nbrMatching)
      val amountOfCurrent = amounts(cardNbr)
      wonCards.foreach { wonCardNbr =>
        amounts = amounts + (wonCardNbr -> (amountOfCurrent + amounts.getOrElse(wonCardNbr, 0)))
      }
    }

    amounts.values.sum
  }

  def parseLine(line: String): Card = {
    line match {
      case s"Card $cardNbr: $winningStr | $mineStr" =>
        def parseNumbers = (str: String) => str.split(" ").filter(_.nonEmpty).map(_.trim.toInt).toSeq
        val winning = parseNumbers(winningStr)
        val mine = parseNumbers(mineStr)
        Card(cardNbr.trim.toInt, winning, mine)
    }
  }

}
