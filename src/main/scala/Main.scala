package com.github.kory33.s2mctest.protocolconversion

import com.github.kory33.s2mctest.protocolconversion.idbindings.{BindingsConverter, BindingsParser, BindingsPrinter}
import com.github.kory33.s2mctest.protocolconversion.idbindings.definition.{BindingEntry, VersionedBindings, WithState}
import com.github.kory33.s2mctest.protocolconversion.packetintents.{DefinitionConverter, ProtocolParser, ProtocolPrinter}

import java.io.File
import scala.io.Source.*

object Main {
  def processPacketIntentsAt(packetIntentDefinitionFilePath: String): Unit = {
    val lines = fromFile(packetIntentDefinitionFilePath).getLines.mkString("\n")

    println(ProtocolPrinter.show(DefinitionConverter.convert(ProtocolParser(lines))))
  }

  def processIdBindingsAt(versionsDirectoryPath: String): Unit = {
    val dir = File(versionsDirectoryPath)
    if (!dir.exists() && !dir.isDirectory)
      throw RuntimeException("Expected an existing directory for <dirPath>")

    val files =
      dir.listFiles()
        .filter(_.isFile)
        .filter(_.getName.endsWith(".rs"))
        .toList

    val versionedBindings = files.map { rustFile =>
      val versionName = rustFile.getName.dropRight(".rs".length)
      val content = fromFile(rustFile).getLines().mkString("\n")

      BindingsConverter.convert(BindingsParser(versionName, content))
    }

    println(BindingsPrinter.show(versionedBindings))
  }

  def main(args: Array[String]): Unit = {
    args match {
      case Array("convert-intents", path) =>
        processPacketIntentsAt(path)
      case Array("convert-bindings", dirPath) =>
        processIdBindingsAt(dirPath)
      case _ =>
        println {
          """Expected one of
            | - convert-intents <path>
            | - convert-bindings <dirPath>
            |but input did not match any. Quitting...
            |""".stripMargin
        }
        sys.exit(1)
    }
  }
}
