package se.johan1a.adventofcode2023

object Day25 {

  def part1(input: Seq[String]): Int = {
    val graph = parse(input)
    val source = graph.keys.head
    val sink = getFurthestAway(graph, source)
    val groupSize = getMinCutGroupSize(graph, source, sink)
    val otherGroupSize = graph.keys.size - groupSize
    groupSize * otherGroupSize
  }

  def getMinCutGroupSize(originalGraph: Map[String, Map[String, Int]], source: String, sink: String) = {
    var graph = originalGraph
    var parent = Map[String, String]()
    var maxFlow = 0

    var existsResidualPath = false
    val (newParent, newExistsResidualPath) = findPath(graph, source, sink)
    parent = newParent
    existsResidualPath = newExistsResidualPath
    while (existsResidualPath) {

      var pathFlow = Int.MaxValue
      var curr = sink
      while (curr != source) {
        val p = parent(curr)
        pathFlow = Math.min(pathFlow, graph(p)(curr))
        curr = p
      }
      maxFlow += pathFlow

      curr = sink
      while (curr != source) {
        val p = parent(curr)
        graph = graph.updated(p, graph(p) + (curr -> (graph(p)(curr) - pathFlow)))
        graph = graph.updated(curr, graph(curr) + (p -> (graph(curr)(p) + pathFlow)))
        curr = p
      }

      val (newParent, newExistsResidualPath) = findPath(graph, source, sink)
      parent = parent ++ newParent
      existsResidualPath = newExistsResidualPath
    }

    val visited = flood(graph, source)

    originalGraph.foreach { case (node, neighbors) =>
      neighbors.foreach { case (neighbor, originalCapacity) =>
        if (visited(node) && !visited(neighbor)) {
          // println(s"$node -> $neighbor should be disconnected")
        }
      }
    }
    visited.size
  }

  def flood(graph: Map[String, Map[String, Int]], source: String) = {
    var visited = Set[String]()
    var queue = Seq(source)
    while (queue.nonEmpty) {
      val curr = queue.head
      queue = queue.tail
      visited = visited + curr
      graph(curr).foreach { case (neighbor, residual) =>
        if (!visited.contains(neighbor) && graph(curr)(neighbor) > 0) {
          queue = queue :+ neighbor
        }
      }
    }
    visited
  }

  def findPath(graph: Map[String, Map[String, Int]], source: String, sink: String) = {
    var parent = Map[String, String]()
    var queue = Seq(source)
    var seen = Set[String]()
    var found = false
    while (queue.nonEmpty) {
      val curr = queue.head
      queue = queue.tail
      if (curr == sink) {
        found = true
      }
      seen = seen + curr
      graph(curr).foreach { (neighbor, remainingFlow) =>
        if (!seen.contains(neighbor) && remainingFlow > 0) {
          seen = seen + neighbor
          queue = queue :+ neighbor
          parent = parent + (neighbor -> curr)
        }
      }
    }
    if (found) {
      (parent, true)
    } else {
      (Map.empty, false)
    }
  }

  def getFurthestAway(graph: Map[String, Map[String, Int]], start: String) = {
    var furthestAway = start
    var queue = Seq(start)
    var seen = Set[String]()
    while (queue.nonEmpty) {
      val curr = queue.head
      queue = queue.tail
      if (!seen.contains(curr)) {
        seen = seen + curr
        graph(curr).foreach { (neighbor, _) =>
          furthestAway = neighbor
          queue = queue :+ neighbor
        }
      }
    }
    furthestAway
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
    var neighbors = Map[String, Map[String, Int]]().withDefaultValue(Map.empty)
    input.foreach { line =>
      line match {
        case s"$src: $dests" =>
          neighbors = neighbors + (src -> (neighbors(src) ++ dests.split(" ").toSeq.map(d => (d -> 1))))
          dests.split(" ").foreach { dest =>
            neighbors = neighbors + (dest -> (neighbors(dest) + (src -> 1)))
          }
      }

    }
    neighbors
  }
}
