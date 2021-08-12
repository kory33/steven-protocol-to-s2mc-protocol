package com.github.kory33.s2mctest.protocolconversion

import ProtocolPrinter.indent

object ProtocolPrinter {
  def showCommentLines(lines: Vector[String]): String =
    if (lines.length == 0)
      throw IllegalArgumentException("expected nonempty comment lines")
    else if (lines.length == 1)
      s"/**${lines.head} */"
    else
      s"/**\n" + lines.map { line =>
        s" *$line\n"
      }.mkString + " */"

  def repString(n: Int)(input: String): String = List.fill(n)(input).mkString

  def prefixNonemptyLinesWith(string: String)(lines: String): String =
    lines
      .split("\n", -1)  // don't discard trailing lines
      .map(l => if l.isEmpty then l else string + l)
      .mkString("\n")

  def indent(input: String): String = prefixNonemptyLinesWith("  ")(input)

  def showPacketDefinition(definition: PacketDefinitionSection.PacketDefinition): String =
    def showFieldType(fieldType: FieldType): String = fieldType match {
      case FieldType.Known(counterpart) =>
        counterpart
      case FieldType.AppliedType(typeConstructorName, types) =>
        s"$typeConstructorName[${types.map(showFieldType).mkString(", ")}]"
      case FieldType.Raw(raw) =>
        s"RustType[$raw]"
    }

    def showFieldDefinitionSection(fieldDefinitionSection: FieldDefinitionSection) = fieldDefinitionSection match {
      case FieldDefinitionSection.FieldDefinition(name, fieldType, Some(_)) =>
        s"$name: Option[${showFieldType(fieldType)}],"
      case FieldDefinitionSection.FieldDefinition(name, fieldType, None) =>
        s"$name: ${showFieldType(fieldType)},"
      case FieldDefinitionSection.FieldComment(lines) => showCommentLines(lines)
    }

    val PacketDefinitionSection.PacketDefinition(packetName, fDefSections) = definition

    val classDeclInitiator = s"case class $packetName"
    val fieldDeclIndentation = repString(classDeclInitiator.length)(" ")

    val requirements = {
      val requireLines = fDefSections.flatMap {
        case FieldDefinitionSection.FieldDefinition(name, typeName, Some(fieldConditionLambda)) =>
          Some(name, fieldConditionLambda)
        case _ => None
      }.map { case (fieldName, condition) =>
        s"require($fieldName.nonEmpty == (${condition}))"
      }

      if requireLines.isEmpty then
        ""
      else
        s" {\n" + indent {
          requireLines.map(_ + "\n").mkString
        } + "}"
    }

    s"$classDeclInitiator(\n" + {
      fDefSections.map { s =>
        prefixNonemptyLinesWith(s"$fieldDeclIndentation ")(showFieldDefinitionSection(s))
      }.mkString("", "\n", "\n")
    } + s"$fieldDeclIndentation) extends PacketIntent${requirements}\n"

  def show(protocol: ProtocolDefinition): String =
    val stateDefinitions = protocol.definitions

    indent {
      stateDefinitions.map { case StateDefinition(state, targets) =>
        s"object $state {\n" + indent(targets.map { case TargetDefinition(target, tDefSections) =>
          s"object $target {\n" + indent(tDefSections.map {
            case PacketDefinitionSection.PacketComment(lines) =>
              showCommentLines(lines) // no trailing line break
            case d @ PacketDefinitionSection.PacketDefinition(_, _) =>
              showPacketDefinition(d)
          }.mkString("\n")) + s"}\n"
        }.mkString) + s"}\n"
      }.mkString
    }
}
