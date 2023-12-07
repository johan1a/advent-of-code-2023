package se.johan1a.adventofcode2023

object Day07 {

  case class Hand(cards: Seq[Char], bid: Int)

  def part1(input: Seq[String]): Int = {
    getTotalWinnings(parse(input))
  }

  def part2(input: Seq[String]): Int = {
    val modifiedInput = input.map(l => l.replaceAll("J", "X"))
    getTotalWinnings(parse(modifiedInput))
  }

  def getTotalWinnings(hands: Seq[Hand]): Int = {
    hands
      .sortWith(sortHands)
      .reverse
      .zipWithIndex
      .map { case (hand, i) =>
        hand.bid * (i + 1)
      }
      .sum
  }

  def sortHands(a: Hand, b: Hand): Boolean = {
    val aHandScore = handScore(a)
    val bHandScore = handScore(b)
    if (aHandScore == bHandScore) {
      a.cards
        .zip(b.cards)
        .filter { case (aCard, bCard) => aCard != bCard }
        .map { case (aCard, bCard) =>
          cardValues(aCard) > cardValues(bCard)
        }
        .head
    } else {
      aHandScore > bHandScore
    }
  }

  def handScore(hand: Hand): Int = {
    val counts: Map[Char, Int] = hand.cards.groupBy(identity).view.mapValues(_.size).toMap

    val nbrJokers = counts.getOrElse('X', 0)
    val groupSizes = counts.filter(_._1 != 'X').values.toSeq.sorted.reverse // largest first

    if (nbrJokers == 5 || groupSizes.max + nbrJokers == 5) {
      7
    } else if (groupSizes.max + nbrJokers == 4) {
      6
    } else if (hasFullHouse(groupSizes, nbrJokers)) {
      5
    } else if (hasThreeOfAKind(groupSizes, nbrJokers)) {
      4
    } else if (hasTwoPair(groupSizes, nbrJokers)) {
      3
    } else if (hasOnePair(groupSizes, nbrJokers)) {
      2
    } else {
      1
    }
  }

  def hasFullHouse(groups: Seq[Int], nbrJokers: Int): Boolean = {
    var bigGroup = groups.head
    val groupsLeft = groups.tail
    var nbrJokersLeft = nbrJokers
    while (bigGroup < 3 && nbrJokersLeft > 0) {
      bigGroup += 1
      nbrJokersLeft -= 1
    }
    bigGroup == 3 && (nbrJokersLeft == 2 || (groupsLeft.head + nbrJokersLeft == 2))
  }

  def hasThreeOfAKind(groups: Seq[Int], nbrJokers: Int): Boolean = {
    var bigGroup = groups.head
    val groupsLeft = groups.tail
    var nbrJokersLeft = nbrJokers
    while (bigGroup < 3 && nbrJokersLeft > 0) {
      bigGroup += 1
      nbrJokersLeft -= 1
    }
    bigGroup == 3 && groupsLeft.size + nbrJokersLeft == 2
  }

  def hasTwoPair(groups: Seq[Int], nbrJokers: Int): Boolean = {
    var firstGroup = groups.head
    var groupsLeft = groups.tail
    var nbrJokersLeft = nbrJokers
    while (firstGroup < 2 && nbrJokersLeft > 0) {
      firstGroup += 1
      nbrJokersLeft -= 1
    }

    var secondGroup = groupsLeft.head
    groupsLeft = groupsLeft.tail
    while (secondGroup < 2 && nbrJokersLeft > 0) {
      secondGroup += 1
      nbrJokersLeft -= 1
    }

    firstGroup == 2 && secondGroup == 2 && (nbrJokersLeft == 1 || groupsLeft.contains(1))
  }

  def hasOnePair(groups: Seq[Int], nbrJokers: Int): Boolean = {
    var firstGroup = groups.head
    val groupsLeft = groups.tail
    var nbrJokersLeft = nbrJokers
    while (firstGroup < 2 && nbrJokersLeft > 0) {
      firstGroup += 1
      nbrJokersLeft -= 1
    }

    firstGroup == 2 && (nbrJokersLeft + groupsLeft.size) == 3
  }

  val cardValues = Map(
    'X' -> 1,
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

  def parse(lines: Seq[String]): Seq[Hand] = {
    lines.map { line =>
      line match {
        case s"$hand $bid" =>
          Hand(hand.toCharArray.toSeq, bid.toInt)
      }
    }
  }
}
