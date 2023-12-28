package se.johan1a.adventofcode2023

object Day25 {

  def part1(input: Seq[String]): Int = {
    val (graph, nbrComponents) = parse(input)
    val connections: Seq[(String, String)] = graph.toSeq.flatMap { case (k, values) =>
      values.map { v =>
        k -> v
      }
    }.distinct
    var best = -1
    val size = 0
      .until(connections.size)
      .flatMap { i =>
        (i + 1).until(connections.size).flatMap { j =>
          (j + 1).until(connections.size).map { k =>
            if (k % 10 == 0) {
              println(s"i $i j $j k $k / ${connections.size}")
            }
            val disconnected = Seq(
              connections(i),
              connections(j),
              connections(k),
              (connections(i)._2, connections(i)._1),
              (connections(j)._2, connections(j)._1),
              (connections(k)._2, connections(k)._1)
            )
            val start = graph.keys.find(c => !disconnected.exists(d => d._1 == c || d._2 == c)).get
            val debug = false && (
              disconnected.contains(("hfx" -> "pzl")) && disconnected.contains(("bvb" -> "cmg"))
                && disconnected.contains(("nvd" -> "jqt"))
            )
            val a = groupSize(graph, disconnected, start, debug)
            val b = nbrComponents - a
            val ans = a * b
            if (ans > best) {
              best = ans
              println(s"new best: $best, a $a b $b disconnected $disconnected")
            }
            // if (debug) {
            //   println(s"start: $start, a: $a, disconnected: $disconnected")
            // }
            a
          }
        }
      }
      .min
    println(s"size: $size nbrComponents: $nbrComponents")

    // graph.foreach(println)
    val b = nbrComponents - size
    size * b
  }

  def groupSize(
      graph: Map[String, Seq[String]],
      disconnected: Seq[(String, String)],
      start: String,
      debug: Boolean = false
  ) = {
    var seen = Set[String]()
    var queue = Seq(start)
    while (queue.nonEmpty) {
      val curr = queue.head
      if (debug) {
        println(curr)
      }
      queue = queue.tail
      if (!seen.contains(curr)) {
        seen = seen + curr
        graph(curr).foreach { neighbor =>
          if (!disconnected.contains((curr, neighbor))) {
            queue = queue :+ neighbor
          }
        }
      }
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
          neighbors = neighbors + (src -> dests.split(" ").toSeq)
          dests.split(" ").foreach { dest =>
            neighbors = neighbors + (dest -> (neighbors(dest) :+ src))
          }
      }

    }
    (neighbors, allComponents.size)
  }
}
