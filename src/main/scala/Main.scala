package com.github.kory33.s2mctest.protocolconversion

import scala.io.Source.*

object Main {
  def processFileAt(protocolDefinitionFilePath: String): Unit = {
    val lines = fromFile(protocolDefinitionFilePath).getLines.mkString("\n")

    println(ProtocolPrinter.show(ProtocolParser(lines)))
  }

  def main(args: Array[String]): Unit = {
    args match {
      case Array(path) =>
        processFileAt(path)
      case _ =>
        println("Expected an input file path as the only argument. Quitting...")
        sys.exit(1)
    }
  }
}
