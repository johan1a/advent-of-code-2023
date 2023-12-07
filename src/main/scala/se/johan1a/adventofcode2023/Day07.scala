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

  val values2 = Map(
    'J' -> 1,
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '5' -> 5,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
    'T' -> 10,
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
    parse(input)
      .sortWith(sortFunc2)
      .reverse
      .zipWithIndex
      .map { case (hand, i) =>
        hand.bid * (i + 1)
      }
      .sum
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

  def sortFunc2 = { (a: Hand, b: Hand) =>
    {
      val aHandScore = handScore2(a)
      val bHandScore = handScore2(b)
      if (aHandScore == bHandScore) {
        a.cards
          .zip(b.cards)
          .filter { case (aCard, bCard) => aCard != bCard }
          .map { case (aCard, bCard) =>
            values2(aCard) > values2(bCard)
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

  def handScore2(hand: Hand): Int = {
    val counts = hand.cards.groupBy(identity).view.mapValues(_.size).toMap

    val jCount = counts.getOrElse('J', 0)
    val groups = counts.filter(_._1 != 'J').values.toSeq.sorted.reverse // largest first

    if (jCount == 5 || groups.max + jCount == 5) {
      7
    } else if (groups.max == 4 || groups.max + jCount == 4) {
      6
    } else if (hasFullHouse(groups, jCount)) {
      5
    } else if (hasThreeOfAKind(groups, jCount)) {
      4
    } else if (hasTwoPair(groups, jCount)) {
      3
    } else if (hasOnePair(groups, jCount)) {
      2
    } else {
      1
    }
  }

  def hasOnePair(groups0: Seq[Int], jCount: Int): Boolean = {
    var j = jCount
    var groups: Seq[Int] = groups0
    var firstGroup = groups.head
    groups = groups.tail
    while (firstGroup < 2 && j > 0) {
      firstGroup += 1
      j -= 1
    }
    if (firstGroup != 2) { return false }

    firstGroup == 2 && (j + groups.size) == 3
  }

  def hasTwoPair(groups0: Seq[Int], jCount: Int): Boolean = {
    var j = jCount
    var groups: Seq[Int] = groups0
    var firstGroup = groups.head
    groups = groups.tail
    while (firstGroup < 2 && j > 0) {
      firstGroup += 1
      j -= 1
    }
    if (firstGroup != 2) { return false }

    var secondGroup = groups.head // TODO check for empty?
    groups = groups.tail
    while (secondGroup < 2 && j > 0) {
      secondGroup += 1
      j -= 1
    }

    firstGroup == 2 && secondGroup == 2 && (j == 1 || groups.contains(1))
  }

  def hasThreeOfAKind(groups0: Seq[Int], jCount: Int): Boolean = {
    var j = jCount
    var groups = groups0
    var bigGroup = groups.head
    groups = groups.tail
    while (bigGroup < 3 && j > 0) {
      bigGroup += 1
      j -= 1
    }
    bigGroup == 3 && (j == 2 || groups.size == 2 || (groups.size == 1 && j == 1))
  }

  def hasFullHouse(groups0: Seq[Int], jCount: Int): Boolean = {
    var j = jCount
    var groups = groups0
    var bigGroup = groups.head
    groups = groups.tail
    while (bigGroup < 3 && j > 0) {
      bigGroup += 1
      j -= 1
    }

    bigGroup == 3 && (j == 2 || (groups.head + j == 2))
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
