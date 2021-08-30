package com.github.kory33.s2mctest.protocolconversion
package packetintents

import packetintents.definition.*

import FieldDefinitionSection.*
import FieldType.*
import PacketDefinitionSection.*
import PacketTarget.*

import monocle.Traversal
import monocle.macros.{GenLens, GenPrism}

import scala.util.matching.Regex

object DefinitionConverter {

  import scala.util.chaining.given

  def convertPacketDefinition(pd: PacketDefinition): PacketDefinition =

    val fieldDefinitionTraversal: Traversal[PacketDefinition, FieldDefinition] =
      GenLens[PacketDefinition](_.sections.each).andThen {
        GenPrism[FieldDefinitionSection, FieldDefinition]
      }

    val fieldNameTraversal: Traversal[PacketDefinition, String] =
      fieldDefinitionTraversal.andThen {
        GenLens[FieldDefinition](_.fieldName)
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

    def rewriteFieldNames(packetDefinition: PacketDefinition): PacketDefinition = {
      case class FieldNameConversion(old: String, replaced: String)

      def fieldNameConversion(fieldName: String) = {
        def snakeCaseToLowerCamelCase(input: String): String = {
          val underscores = """_([a-z0-9])""".r
          underscores.replaceAllIn(input, m => m.group(1).toUpperCase)
        }

        fieldName match {
          case "new" => "isNew" // used for ChunkData_Biomes3D
          case s => snakeCaseToLowerCamelCase(s)
        }
      }

      def rewriteConditionBody(old: String, conversion: FieldNameConversion): String =
        old.replaceAll(conversion.old, conversion.replaced)

      val conversions =
        fieldNameTraversal
          .getAll(packetDefinition)
          .map(fieldName => FieldNameConversion(fieldName, fieldNameConversion(fieldName)))

      packetDefinition
        .pipe(fieldNameTraversal.modify(fieldNameConversion))
        .pipe(conditionBodyTraversal.modify(conversions.foldLeft(_)(rewriteConditionBody)))
    }

    def rewriteComparisonWithVarInts(packetDefinition: PacketDefinition): PacketDefinition = {
      def rewriteLambdaBody(lambdaBody: String, varIntField: String): String = {
        lambdaBody.replaceAll(
          """(""" + Regex.quote(varIntField) + """)\.0 == (\d+)""",
          """$1 == VarInt($2)"""
        )
      }

      val varIntFields =
        fieldDefinitionTraversal
          .filter(_.typeName == FieldType.Known("VarInt"))
          .getAll(packetDefinition)
          .map(_.fieldName)

      conditionBodyTraversal.modify(varIntFields.foldLeft(_)(rewriteLambdaBody))(packetDefinition)
    }

    def rewriteComparisonWithVarLongs(packetDefinition: PacketDefinition): PacketDefinition = {
      def rewriteLambdaBody(lambdaBody: String, varLongField: String): String = {
        lambdaBody.replaceAll(
          """(""" + Regex.quote(varLongField) + """)\.0 == (\d+)""",
          """$1 == VarLong($2)"""
        )
      }

      val varLongFields =
        fieldDefinitionTraversal
          .filter(_.typeName == FieldType.Known("VarLong"))
          .getAll(packetDefinition)
          .map(_.fieldName)
      conditionBodyTraversal.modify(varLongFields.foldLeft(_)(rewriteLambdaBody))(packetDefinition)
    }

    def rewriteComparisonWithUbytes(packetDefinition: PacketDefinition): PacketDefinition = {
      def rewriteLambdaBody(lambdaBody: String, varLongField: String): String = {
        lambdaBody
          // comparison with literals
          .replaceAll(
            """(""" + Regex.quote(varLongField) + """) (==|!=) (\d+)""",
            """$1 $2 UByte($3)"""
          )
          // nonzero test
          .replaceAll(
            """(""" + Regex.quote(varLongField) + """) \> 0""",
            """$1 != UByte(0)"""
          )
          // bits up test
          .replaceAll(
            """(""" + Regex.quote(varLongField) + """) & (0x[0-9a-f]+) != 0""",
            """($1 & $2) != UByte(0)"""
          )
      }

      val uByteFields =
        fieldDefinitionTraversal
          .filter(_.typeName == FieldType.Known("UByte"))
          .getAll(packetDefinition)
          .map(_.fieldName)
      conditionBodyTraversal.modify(uByteFields.foldLeft(_)(rewriteLambdaBody))(packetDefinition)
    }

    pd
      .pipe(typeTraversal.modify(refineTypes))
      .pipe(rewriteFieldNames)
      .pipe(rewriteComparisonWithVarInts)
      .pipe(rewriteComparisonWithVarLongs)
      .pipe(rewriteComparisonWithUbytes)

  def convert(definition: ProtocolDefinition): ProtocolDefinition =
    val packetDefinitionTraversal: Traversal[ProtocolDefinition, PacketDefinition] =
      GenLens[ProtocolDefinition](_.definitions.each.definitions.each.sections.each).andThen {
        GenPrism[PacketDefinitionSection, PacketDefinition]
      }

    packetDefinitionTraversal.modify(convertPacketDefinition)(definition)
}
