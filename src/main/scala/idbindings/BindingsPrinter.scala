package com.github.kory33.s2mctest.protocolconversion
package idbindings

import idbindings.definition.{BindingEntry, VersionedBindings, WithState}

object BindingsPrinter {

  def prefixNonemptyLinesWith(string: String)(lines: String): String =
    lines
      .split("\n", -1) // don't discard trailing lines
      .map(l => if l.isEmpty then l else string + l)
      .mkString("\n")

  def indent(input: String): String = prefixNonemptyLinesWith("  ")(input)

  def showEntries(list: List[BindingEntry]): String =
    list.map { case BindingEntry(numberLiteral, className) =>
      s"$numberLiteral -> ByteCodec.summon[$className],\n"
    }.mkString("")

  def show(versionedBindings: List[VersionedBindings]): String = {
    indent(indent {
      versionedBindings.map { bindings =>
        s"object ${bindings.versionName} {\n" + indent {
          bindings.states.map { case WithState(stateName, serverBoundEntry, clientBoundEntry) =>
            s"val ${stateName}Protocol = Protocol(\n" + indent {
              "PacketIdBindings((\n" + indent {
                showEntries(serverBoundEntry)
              } + ")),\n" +
                "PacketIdBindings((\n" + indent {
                showEntries(clientBoundEntry)
              } + "))\n"
            } + ")\n"
          }.mkString("\n")
        } + "}\n"
      }.mkString("\n")
    })
  }
}
