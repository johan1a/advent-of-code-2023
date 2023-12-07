package se.johan1a.adventofcode2023

object Day07 {

  case class Hand(cards: Seq[Char], bid: Int)

  val values = Map(
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '5' -> 5,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
    'T' -> 10,
    'J' -> 11,
    'Q' -> 12,
    'K' -> 13,
    'A' -> 14
  )

  def part1(input: Seq[String]): Int = {
    parse(input)
      .sortWith(sortFunc)
      .reverse
      .zipWithIndex
      .map { case (hand, i) =>
        hand.bid * (i + 1)
      }
      .sum
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  def sortFunc = { (a: Hand, b: Hand) =>
    {
      val aHandScore = handScore(a)
      val bHandScore = handScore(b)
      if (aHandScore == bHandScore) {
        a.cards
          .zip(b.cards)
          .filter { case (aCard, bCard) => aCard != bCard }
          .map { case (aCard, bCard) =>
            values(aCard) > values(bCard)
          }
          .head
      } else {
        aHandScore > bHandScore
      }
    }
  }

  def handScore(hand: Hand): Int = {
    val groups = hand.cards.groupBy(identity).view.mapValues(_.size).toMap.values
    if (groups.size == 1) {
      7
    } else if (groups.size == 2 && groups.exists(_ == 4)) {
      6
    } else if (groups.size == 2 && groups.exists(_ == 3) && groups.exists(_ == 2)) {
      5
    } else if (groups.size == 3 && groups.exists(_ == 3)) {
      4
    } else if (groups.size == 3 && groups.filter(_ == 2).size == 2) {
      3
    } else if (groups.size == 4 && groups.exists(_ == 2)) {
      2
    } else if (groups.size == 5) {
      1
    } else {
      throw new Exception(s"unrecognized hand: $hand groups: ${groups.toSeq}")
    }
  }

  def parse(lines: Seq[String]): Seq[Hand] = {
    lines.map { line =>
      line match {
        case s"$hand $bid" =>
          Hand(hand.toCharArray.toSeq, bid.toInt)
      }
    }
  }
}
