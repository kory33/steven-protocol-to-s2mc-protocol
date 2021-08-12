package com.github.kory33.s2mctest.protocolconversion

import cats.kernel.Monoid

enum FieldDefinitionSection:
  case FieldComment(lines: Vector[String])
  case FieldDefinition(fieldName: String, typeName: String, fieldConditionLambda: Option[String])

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
