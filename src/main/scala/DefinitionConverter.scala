package com.github.kory33.s2mctest.protocolconversion

import definition.{FieldDefinitionSection, FieldType, PacketDefinitionSection, ProtocolDefinition}

import monocle.law.TraversalLaws
import monocle.macros.{GenLens, GenPrism}
import monocle.{Iso, Optional, PIso, Prism, Traversal}

import scala.util.matching.Regex

object DefinitionConverter {
  import monocle.syntax.all._
  import scala.util.chaining.given

  import PacketDefinitionSection.PacketDefinition

  def convertPacketDefinition(pd: PacketDefinition): PacketDefinition =
    import FieldDefinitionSection.FieldDefinition

    val fieldDefinitionTraversal: Traversal[PacketDefinition, FieldDefinition] =
      GenLens[PacketDefinition](_.sections.each).andThen {
        GenPrism[FieldDefinitionSection, FieldDefinition]
      }

    val typeTraversal: Traversal[PacketDefinition, FieldType] =
      fieldDefinitionTraversal.andThen {
        GenLens[FieldDefinition](_.typeName)
      }

    val conditionBodyTraversal: Traversal[PacketDefinition, String] =
      fieldDefinitionTraversal.andThen {
        GenLens[FieldDefinition](_.fieldCondition.some)
      }

    def refineTypes(fieldType: FieldType): FieldType = {
      def rawToKnownTypes(typeName: FieldType.Raw): Option[FieldType.Known] =
        val mappings = Map(
          "()" -> "Unit",
          "bool" -> "Boolean",
          "u8" -> "UByte",
          "i8" -> "Byte",
          "u16" -> "UShort",
          "i16" -> "Short",
          "i32" -> "Int",
          "i64" -> "Long",
          "u64" -> "ULong",
          "f32" -> "Float",
          "f64" -> "Double",
          "String" -> "String",
          "VarShort" -> "VarShort",
          "VarInt" -> "VarInt",
          "VarLong" -> "VarLong",
          "UUID" -> "UUID",
          "Position" -> "Position",
          "Biomes3D" -> "Biomes3D",
          "format::Component" -> "Component",
          "item::Stack" -> "Stack",
          "nbt::NamedTag" -> "NamedTag",
          "packet::BlockChangeRecord" -> "BlockChangeRecord",
          "packet::ChunkMeta" -> "ChunkMeta",
          "packet::CommandNode" -> "CommandNode",
          "packet::EntityEquipments" -> "EntityEquipments",
          "packet::EntityProperty" -> "EntityProperty",
          "packet::EntityProperty_i16" -> "EntityPropertyShort",
          "packet::ExplosionRecord" -> "ExplosionRecord",
          "packet::MapIcon" -> "MapIcon",
          "packet::PlayerInfoData" -> "PlayerInfoData",
          "packet::Recipe" -> "Recipe",
          "packet::SpawnProperty" -> "SpawnProperty",
          "packet::Statistic" -> "Statistic",
          "packet::Tags" -> "Tags",
          "packet::Trade" -> "Trade",
          "types::Metadata" -> "Metadata",
        )

        mappings.get(typeName.raw).map(FieldType.Known.apply)

      def convertTyConsName(raw: String): String =
        raw match
          case "Vec" => "Vector"
          case _ => raw

      fieldType match {
        case FieldType.AppliedType(tyCons, args) =>
          FieldType.AppliedType(convertTyConsName(tyCons), args.map(refineTypes))
        case raw: FieldType.Raw => rawToKnownTypes(raw).getOrElse(raw)
        case _: FieldType.Known => fieldType
      }
    }

    def rewriteComparisonWithVarInts(lambdaBody: String, varIntField: String): String = {
      lambdaBody.replaceAll(
        """(""" + Regex.quote(varIntField) + """)\.0 == (\d+)""",
        """$1 == VarInt($2)"""
      )
    }

    def rewriteComparisonWithVarLongs(lambdaBody: String, varLongField: String): String = {
      lambdaBody.replaceAll(
        """(""" + Regex.quote(varLongField) + """)\.0 == (\d+)""",
        """$1 == VarLong($2)"""
      )
    }

    pd
      .pipe(typeTraversal.modify(refineTypes))
      .pipe { pd =>
        val varIntFields =
          fieldDefinitionTraversal
            .filter(_.typeName == FieldType.Known("VarInt"))
            .getAll(pd)
            .map(_.fieldName)
        conditionBodyTraversal.modify(varIntFields.foldLeft(_)(rewriteComparisonWithVarInts))(pd)
      }
      .pipe { pd =>
        val varLongFields =
          fieldDefinitionTraversal
            .filter(_.typeName == FieldType.Known("VarLong"))
            .getAll(pd)
            .map(_.fieldName)
        conditionBodyTraversal.modify(varLongFields.foldLeft(_)(rewriteComparisonWithVarLongs))(pd)
      }

  def convert(definition: ProtocolDefinition): ProtocolDefinition =
    val packetDefinitionTraversal =
      GenLens[ProtocolDefinition](_.definitions.each.definitions.each.sections.each).andThen {
        GenPrism[PacketDefinitionSection, PacketDefinition]
      }

    packetDefinitionTraversal.modify(convertPacketDefinition)(definition)
}
