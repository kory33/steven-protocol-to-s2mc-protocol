package com.github.kory33.s2mctest.protocolconversion

import definition.{PacketDefinitionSection, ProtocolDefinition}

import monocle.law.TraversalLaws
import monocle.macros.{GenLens, GenPrism}
import monocle.{Iso, Optional, Prism, PIso, Traversal}

object DefinitionConverter {
  import monocle.syntax.all._

  def convertPacketDefinition(pd: PacketDefinitionSection.PacketDefinition): PacketDefinitionSection.PacketDefinition =
    pd

  def convert(definition: ProtocolDefinition): ProtocolDefinition =
    val stateTraversal: Traversal[ProtocolDefinition, PacketDefinitionSection] =
      GenLens[ProtocolDefinition](_.definitions.each.definitions.each.sections.each)

    val packetDefinitionPrism: Prism[PacketDefinitionSection, PacketDefinitionSection.PacketDefinition] =
      GenPrism[PacketDefinitionSection, PacketDefinitionSection.PacketDefinition]

    val packetDefinitionTraversal = stateTraversal.andThen(packetDefinitionPrism)

    packetDefinitionTraversal.modify(convertPacketDefinition)(definition)
}
