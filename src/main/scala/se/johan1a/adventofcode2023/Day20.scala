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
  case class Broadcast(name: Name, outputs: Seq[Name]) extends Module {
    override def toString() = s"""Broadcast(name: "$name", outputs: ${outputs.mkString(",")})"""
  }
  case class FlipFlop(name: Name, outputs: Seq[Name], on: Boolean) extends Module {
    override def toString() = s"""FlipFlop(name: "$name", on: $on, outputs: ${outputs.mkString(",")})"""
  }
  case class Conjunction(name: Name, outputs: Seq[Name], lastReceived: Map[Name, Pulse]) extends Module {
    override def toString() = {
      //s"""Conjunction(name: "$name", lastReceived: $lastReceived, outputs: ${outputs.mkString(",")})"""
      s"""Conjunction(name: "$name", ${getBits()})"""
    }

    def getBits() = {
      lastReceived.toSeq.sortBy(_._1).map{ case(_, pulse) =>
        pulse match {
          case High => 1
          case Low => 0
        }
      }.mkString("")
    }
  }
  case class Output(name: Name) extends Module

  def part1(input: Seq[String]): Long = {
    val (_, modules) = parse(input)
    simulate(modules)
  }

  def part2(input: Seq[String], target: String = "rx"): Long = {
    val (inputs, modules) = parse(input)
    //findInputs(inputs, modules, target)
    simulate2(inputs, modules)
    -1
  }

  var printed= Set[Name]()

  def findInputs(
      allInputs: Map[Name, Seq[Name]],
      modules: Map[Name, Module],
      target: Name,
      indent: String = ""
  ): Unit = {
    if(!printed(target)) {
      printed = printed + target
      val inputs = allInputs(target)
      val inputModules = inputs.map(i => modules(i))
      if (inputModules.nonEmpty) {
        println(s"${indent}Inputs of $target: ${inputs.mkString(",")}")
        inputModules.foreach(m => {
          println(s"${indent}$m")
          findInputs(allInputs, modules, m.name, indent + " ")
        })
      }
    }
  }

  def simulate2(inputs: Map[Name, Seq[Name]], startingModules: Map[Name, Module]): Long = {
    var modules = startingModules
    var messages = Seq[Message]()
    var nbrButtonPresses = 0L
    var continue = true

    // "mk,fp,xt,zc -> kl"
    // "&kl -> rx"
    val special: Seq[Name] = inputs("kl")
    println(special)

    val maxIter = 10000
    var i = 0

    var flippedAt = Map[Name, Seq[Long]]().withDefaultValue(Seq.empty)
//    var seen = Map[(Name, String), Long]()

    while (i < maxIter && continue) {
      nbrButtonPresses += 1
      messages = messages :+ Message("button", "broadcaster", Low)

      // println("")
      // val conjunctions = modules.collect { case (name, c: Conjunction) => c}
      // conjunctions.foreach { c =>
      //   val bits = c.getBits()
      //   if(seen.contains((c.name, bits))) {
      //     println(s"Already seen ${c.name} $bits at ${seen((c.name, bits))}")
      //   } else {
      //     seen = seen + ((c.name, bits) -> nbrButtonPresses)
      //   }
      //   println(c)
      // }

      while (messages.nonEmpty) {
        messages.head match {
          case Message(source, destination, pulse) =>
            val module = modules(destination)
            module match {
              case Broadcast(name, outputs) =>
                messages = messages ++ outputs.map(output => Message(name, output, pulse))
              case FlipFlop(name, outputs, on) =>
                if (pulse == Low) {
                  val outputPulse = if (on) {
                    Low
                  } else {
                    High
                  }
                  messages = messages ++ outputs.map(output => Message(name, output, outputPulse))
                  modules = modules.updated(name, FlipFlop(name, outputs, !on))
                }
              case Conjunction(name, outputs, lastReceived) =>
                if (pulse == Low && special.contains(name)) {
                  println(s"Got low pulse for $name at $nbrButtonPresses")
                }


                val newLastReceived = lastReceived.updated(source, pulse)
                modules = modules.updated(name, Conjunction(name, outputs, newLastReceived))
                val outputPulse = if (newLastReceived.forall(_._2 == High)) {
                  Low
                } else {
                  High
                }
                flippedAt = flippedAt.updated(name, flippedAt(name) :+ nbrButtonPresses)
                messages = messages ++ outputs.map(output => Message(name, output, outputPulse))
              case Output(name) =>
                if (pulse == Low && name == "rx") {
                  continue = false
                }
            }
        }
        messages = messages.tail
      }
      i += 1
    }

    // flippedAt.foreach { case (name, indices) =>
    //   println(s"$name flipped at ${indices.mkString(",")}")
    // }


    if(i == maxIter){
      throw new Exception("Max iter reached")
    }
    nbrButtonPresses
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
  def parse(input: Seq[String]) = {
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
        case c              => c
      }
      .map { module =>
        module.name -> module
      }
      .toMap
    inputs.map { case (name, inputs) =>
      if (!modules.contains(name)) {
        modules = modules.updated(name, Output(name))
      }
    }
    (inputs, modules)
  }
}
