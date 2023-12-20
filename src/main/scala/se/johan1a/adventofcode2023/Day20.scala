package se.johan1a.adventofcode2023

object Day20 {

  sealed trait Pulse
  case object Low extends Pulse
  case object High extends Pulse

  type Name = String

  case class Message(source: Name, destination: Name, pulse: Pulse)

  sealed trait Module {
    def name: Name
  }
  case class Broadcast(name: Name, outputs: Seq[Name]) extends Module
  case class FlipFlop(name: Name, outputs: Seq[Name], on: Boolean) extends Module
  case class Conjunction(name: Name, outputs: Seq[Name], lastReceived: Map[Name, Pulse]) extends Module
  case class Output(name: Name) extends Module

  def part1(input: Seq[String]): Long = {
    val modules = parse(input)
    simulate(modules)
  }

  def part2(input: Seq[String]): Long = {
    -1
  }

  def simulate(startingModules: Map[Name, Module]): Long = {
    var modules = startingModules
    var nbrSent = Map[Pulse, Long](Low -> 0L, High -> 0L)
    var messages = Seq[Message]()
    0.until(1000).foreach { _ =>
      messages = messages :+ Message("button", "broadcaster", Low)
      nbrSent = nbrSent.updated(Low, nbrSent(Low) + 1)

      while (messages.nonEmpty) {
        messages.head match {
          case Message(source, destination, pulse) =>
            val module = modules(destination)
            module match {
              case Broadcast(name, outputs) =>
                messages = messages ++ outputs.map(output => Message(name, output, pulse))
                nbrSent = nbrSent.updated(pulse, nbrSent(pulse) + outputs.size)
              case FlipFlop(name, outputs, on) =>
                if (pulse == Low) {
                  val outputPulse = if (on) {
                    Low
                  } else {
                    High
                  }
                  messages = messages ++ outputs.map(output => Message(name, output, outputPulse))
                  nbrSent = nbrSent.updated(outputPulse, nbrSent(outputPulse) + outputs.size)
                  modules = modules.updated(name, FlipFlop(name, outputs, !on))
                }
              case Conjunction(name, outputs, lastReceived) =>
                val newLastReceived = lastReceived.updated(source, pulse)
                modules = modules.updated(name, Conjunction(name, outputs, newLastReceived))
                val outputPulse = if (newLastReceived.forall(_._2 == High)) {
                  Low
                } else {
                  High
                }
                messages = messages ++ outputs.map(output => Message(name, output, outputPulse))
                nbrSent = nbrSent.updated(outputPulse, nbrSent(outputPulse) + outputs.size)
              case Output(_) =>
            }
        }
        messages = messages.tail
      }
    }
    nbrSent.values.product
  }

  // TODO make snippet for this
  def parse(input: Seq[String]): Map[Name, Module] = {
    var inputs = Map[Name, Seq[Name]]().withDefaultValue(Seq.empty)
    var modules = input
      .map { line =>
        line match {
          case s"broadcaster -> $outputsStr" =>
            val outputs = outputsStr.split(", ").toSeq
            outputs.foreach { output =>
              inputs = inputs.updated(output, inputs(output) :+ "broadcaster")
            }
            Broadcast("broadcaster", outputs)
          case s"%$name -> $outputsStr" =>
            val outputs = outputsStr.split(", ").toSeq
            outputs.foreach { output =>
              inputs = inputs.updated(output, inputs(output) :+ name)
            }
            FlipFlop(name, outputs, on = false)
          case s"&$name -> $outputsStr" =>
            val outputs = outputsStr.split(", ").toSeq
            outputs.foreach { output =>
              inputs = inputs.updated(output, inputs(output) :+ name)
            }
            Conjunction(name, outputs, Map.empty)
        }
      }
      .map {
        case c: Conjunction => c.copy(lastReceived = inputs(c.name).map(input => input -> Low).toMap)
        case c   => c
      }
      .map { module =>
        module.name -> module
      }
      .toMap
      inputs.map { case (name, inputs) =>
        if(!modules.contains(name)) {
          modules = modules.updated(name, Output(name))
        }
      }
    modules
  }
}
