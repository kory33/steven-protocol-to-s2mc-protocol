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

/**
 * Aggregate some elements of [[xs]].
 *
 * This function uses [[extract]] to extract certain elements from [[xs]],
 * and if consective elements in [[xs]] gave rise to values of [[M]],
 * they will be merged using [[Monoid]] operation provided on [[M]].
 *
 * Finally, all merged values will be inserted back to `xs` using [[compress]] function
 * that converts (combined) extracted values to [[A]].
 */
private def aggregateConsecutive[A, M](xs: Vector[A])(extract: A => Option[M])(compress: M => A)
                                      (using M: Monoid[M]): Vector[A] =
  import cats.implicits.given

  val (accumulated, lastM) = xs.foldLeft((Vector.empty[A], M.empty)) { (pair, next) =>
    val (accumulatedXs, prevM) = pair
    extract(next) match {
      case Some(m) => (accumulatedXs, prevM |+| m)
      case None => (accumulatedXs.appended(compress(prevM)).appended(next), M.empty)
    }
  }

  accumulated.appended(compress(lastM))

case class TargetDefinition(target: PacketTarget, sections: Vector[PacketDefinitionSection]):
  def commentsAggregated: TargetDefinition =
    import PacketDefinitionSection.PacketComment

    def commentLines(section: PacketDefinitionSection) = section match {
      case PacketComment(lines) => Some(lines)
      case _ => None
    }

    this.copy(sections = aggregateConsecutive(sections)(commentLines)(PacketComment(_)))

case class StateDefinition(state: ConnectionState, definitions: Vector[TargetDefinition])

case class ProtocolDefinition(definitions: Vector[StateDefinition]):
  def toMap: Map[ConnectionState, Map[PacketTarget, Vector[PacketDefinitionSection]]] =
    val flattened: Vector[(ConnectionState, PacketTarget, PacketDefinitionSection)] =
      for {
        StateDefinition(state, targetDefs) <- definitions
        TargetDefinition(target, packetDefs) <- targetDefs
        packetDef <- packetDefs
      } yield (state, target, packetDef)

    val stateGrouped: Map[ConnectionState, Vector[(PacketTarget, PacketDefinitionSection)]] =
      flattened.groupMap(_._1)(t => (t._2, t._3))

    stateGrouped.view.mapValues(v => v.groupMap(_._1)(_._2)).toMap
