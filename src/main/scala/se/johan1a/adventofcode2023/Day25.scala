package se.johan1a.adventofcode2023

import scala.collection.mutable.PriorityQueue

object Day25 {

  def part1(input: Seq[String]): Int = {
    val (graph, nbrComponents) = parse(input)
    var edgeCounts = Map[Seq[String], Int]().withDefaultValue(0)
    var mostUsedEdges = Seq[Seq[String]]()
    0.until(3).foreach { i =>
      graph.keys.foreach { start =>
        if (!mostUsedEdges.exists(e => e.contains(start))) {
          val counts = countEdges(graph, start, mostUsedEdges)
          counts.foreach { case (edge, count) =>
            edgeCounts = edgeCounts.updated(edge, edgeCounts(edge) + count)
          }
        }
      }
      val mostUsedEdge = edgeCounts.maxBy(_._2)._1
      mostUsedEdges = mostUsedEdges :+ mostUsedEdge
    }

    val start = mostUsedEdges.head.head
    val aSize = groupSize(graph, mostUsedEdges, start)
    val bSize = nbrComponents - aSize
    aSize * bSize
  }

  def countEdges(graph: Map[String, Seq[String]], start: String, disconnectedEdges: Seq[Seq[String]]) = {
    var edgeCounts = Map[Seq[String], Int]().withDefaultValue(0)
    var seen = Set[String]()
    var dists = Map[String, Int]().withDefaultValue(Int.MaxValue)
    val queue = new PriorityQueue[(String, Int)]()(Ordering.by(t => -t._2))
    queue += ((start, 0))
    var last = start
    while (queue.nonEmpty) {
      val (curr, d) = queue.dequeue()
      if (!seen.contains(curr)) {
        seen = seen + curr
        last = curr
        graph(curr).foreach { neighbor =>
          val edge = Seq(curr, neighbor).sorted
          if (!disconnectedEdges.contains(edge) && !seen.contains(neighbor) && d + 1 < dists(neighbor)) {
            dists = dists.updated(neighbor, d + 1)
            queue += ((neighbor, d + 1))
            val edge = Seq(curr, neighbor).sorted
            edgeCounts = edgeCounts.updated(edge, edgeCounts(edge) + 1)
          }
        }
      }
    }
    edgeCounts
  }

  def groupSize(
      graph: Map[String, Seq[String]],
      disconnected: Seq[Seq[String]],
      start: String
  ) = {
    var seen = Set[String](start)
    var queue = Seq(start)
    while (queue.nonEmpty) {
      val curr = queue.head
      queue = queue.tail
      graph(curr).foreach { neighbor =>
        if (!seen.contains(neighbor) && !disconnected.contains(Seq(curr, neighbor).sorted)) {
          seen = seen + neighbor
          queue = queue :+ neighbor
        }
      }
    }
    seen.size
  }

  def parse(input: Seq[String]) = {
    var allComponents = Set[String]()
    var neighbors = Map[String, Seq[String]]().withDefaultValue(Seq.empty)
    input.foreach { line =>
      line match {
        case s"$src: $dests" =>
          allComponents = allComponents + src ++ dests.split(" ")
          neighbors = neighbors + (src -> (neighbors(src) ++ dests.split(" ").toSeq))
          dests.split(" ").foreach { dest =>
            neighbors = neighbors + (dest -> (neighbors(dest) :+ src))
          }
      }

    }
    (neighbors, allComponents.size)
  }
}
