package com.github.kory33.s2mctest.protocolconversion

import cats.kernel.Monoid

enum FieldType:
  case Known(counterpartName: String)
  case AppliedType(typeConstructorName: String, args: Vector[FieldType])
  case Raw(raw: String)

enum FieldDefinitionSection:
  case FieldComment(lines: Vector[String])
  case FieldDefinition(fieldName: String, typeName: FieldType, fieldConditionLambda: Option[String])

enum PacketDefinitionSection:
  case PacketComment(lines: Vector[String])
  case PacketDefinition(packetName: String, sections: Vector[FieldDefinitionSection])

enum PacketTarget:
  case ServerBound
  case ClientBound

enum ConnectionState:
  case Handshaking
  case Status
  case Login
  case Play

case class TargetDefinition(target: PacketTarget, sections: Vector[PacketDefinitionSection])

case class StateDefinition(state: ConnectionState, definitions: Vector[TargetDefinition])

case class ProtocolDefinition(definitions: Vector[StateDefinition])
