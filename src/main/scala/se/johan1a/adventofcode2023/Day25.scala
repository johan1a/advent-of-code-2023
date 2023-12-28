package se.johan1a.adventofcode2023

import scala.collection.mutable.PriorityQueue
import java.io.FileWriter
import java.io.File

object Day25 {

  def part1(input: Seq[String]): Int = {
    val (graph, nbrComponents) = parse(input)
    var edgeCounts = Map[Seq[String], Int]().withDefaultValue(0)
    var mostUsedEdges = Seq[Seq[String]]()
    0.until(3).foreach { i =>
      println(s"i: $i")
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
      println("edgeCounts top 10:")
      edgeCounts.toSeq.sortBy(_._2).takeRight(10).reverse.foreach(println)

      val edges = edgeCounts.toSeq.sortBy(_._2).reverse.map(_._1).take(6)

      val fileWriter = new FileWriter(new File("scalagraph.txt"))
      graph.keys.toSeq.sorted.foreach { node =>
        fileWriter.write(s"$node\n")
        graph(node).sorted.foreach { neighbor =>
          fileWriter.write(s" $neighbor\n")
        }
      }
      fileWriter.close()

      0.until(edges.size - 2).foreach { i =>
        (i + 1).until(edges.size).foreach { j =>
          (j + 1).until(edges.size).foreach { k =>
            val disconnected = Seq(edges(i), edges(j), edges(k))
            // val start = disconnected.head.head
            val start = "hcf"
            // dfk -> nxk
            // fpg -> ldl
            // hcf -> lhn
            val a = groupSize(graph, disconnected, start)
            val b = nbrComponents - a
            val ans = a * b
            if (
              disconnected.toSet == Set(
                Seq("dfk", "nxk"),
                Seq("fpg", "ldl"),
                Seq("hcf", "lhn")
              )
            ) {
              println(s"trying with disconnected: ${disconnected} start: $start")
              println(s"a $a b $b ans? $ans")
            }
          }
        }
      }

      // val a = groupSize(graph, mostUsedEdges, start)
      // val b = nbrComponents - a
      // val ans = a * b
      // println(a)
      // println(b)
      // println(ans)

    }
    println(mostUsedEdges)

    val start = graph.keys.find { case node =>
      !mostUsedEdges.exists(e => e(0) == node || e(1) == node)
    }.get
    println(s"counting group size for start: $start")
    val a = groupSize(graph, mostUsedEdges, start)
    val b = nbrComponents - a
    assert(a + b == nbrComponents)
    val ans = a * b
    println(a)
    println(b)
    println(ans)
    ans
  }

  def countEdges(graph: Map[String, Seq[String]], start: String, disconnectedEdges: Seq[Seq[String]]) = {
    var edgeCounts = Map[Seq[String], Int]().withDefaultValue(0)
    var seen = Set[String]()
    var dists = Map[String, Int]().withDefaultValue(Int.MaxValue)
    val queue = new PriorityQueue[(String, Int)]()(Ordering.by(t => -t._2))
    var prevNodes = Map[String, String]()
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
            prevNodes = prevNodes.updated(neighbor, curr)
            queue += ((neighbor, d + 1))
          }
        }
      }
    }
    graph.keys.foreach { target =>
      if (target != start) {
        var path = Seq[String]()
        var curr = target
        while (prevNodes.contains(curr)) {
          val prev = prevNodes(curr)
          val edge = Seq(curr, prev).sorted
          edgeCounts = edgeCounts.updated(edge, edgeCounts(edge) + 1)
          path = curr +: path
          curr = prev
        }
        path = curr +: path
      }
    }
    edgeCounts
  }

  def getFurthestAway(graph: Map[String, Seq[String]], start: String): String = {
    var seen = Set[String]()
    var queue = Seq(start)
    var last = start
    while (queue.nonEmpty) {
      val curr = queue.head
      queue = queue.tail
      if (!seen.contains(curr)) {
        seen = seen + curr
        last = curr
        graph(curr).foreach { neighbor =>
          queue = queue :+ neighbor
        }
      }
    }
    last
  }

  def groupSize(
      graph: Map[String, Seq[String]],
      disconnected: Seq[Seq[String]],
      start: String
  ) = {
    var seen = Set[String](start)
    var queue = Seq(start)
    while (queue.nonEmpty) {
      var next = Seq[String]()
      queue.foreach { curr =>
        graph(curr).foreach { neighbor =>
          if (!seen.contains(neighbor) && !disconnected.contains(Seq(curr, neighbor).sorted)) {
            seen = seen + neighbor
            next = next :+ neighbor
          }
        }
      }
      queue = next
    }
    seen.size
  }

  def part2(input: Seq[String]): Int = {
    -1
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
