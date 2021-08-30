package com.github.kory33.s2mctest.protocolconversion

import com.github.kory33.s2mctest.protocolconversion.packetintents.{DefinitionConverter, ProtocolParser, ProtocolPrinter}

import scala.io.Source.*

object Main {
  def processPacketIntentsAt(packetIntentDefinitionFilePath: String): Unit = {
    val lines = fromFile(packetIntentDefinitionFilePath).getLines.mkString("\n")

    println(ProtocolPrinter.show(DefinitionConverter.convert(ProtocolParser(lines))))
  }

  def main(args: Array[String]): Unit = {
    args match {
      case Array(path) =>
        processPacketIntentsAt(path)
      case _ =>
        println("Expected an input file path as the only argument. Quitting...")
        sys.exit(1)
    }
  }
}
