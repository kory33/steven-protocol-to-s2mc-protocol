package com.github.kory33.s2mctest.protocolconversion

import com.github.kory33.s2mctest.protocolconversion.FieldDefinitionSection.*
import com.github.kory33.s2mctest.protocolconversion.PacketDefinitionSection.*
import com.github.kory33.s2mctest.protocolconversion.PacketTarget.*

import scala.util.parsing.combinator.RegexParsers

object ProtocolParser extends RegexParsers {

  override val skipWhitespace = false

  // region utility combinators
  def bracketedList[I, C](initiator: Parser[I], content: Parser[C]): Parser[(I, C)] =
    for {
      init <- """\s*""".r ~> initiator <~ """\s*\{""".r
      c    <-   content
      _    <- """\s*\}""".r
    } yield (init, c)

  def repVector[T](parser: Parser[T]): Parser[Vector[T]] = rep(parser).map(_.toVector)
  // endregion

  val aggregatedComment: Parser[Vector[String]] =
    val commentLine: Parser[String] = """\s*///""".r ~> """[^\n]*""".r
    repVector(commentLine).filter(_.nonEmpty)

  val fieldDefinition: Parser[FieldDefinition] =
    for {
      fieldName <- """\s*field """.r ~> """\w+""".r
      typeName <- literal(": ") ~> """[^=]+(?= =)""".r <~ literal(" =")
      conditionLambda <- opt {
        not(literal(",")) ~> literal(" when(") ~> """[^\)]+""".r <~ literal(")")
      }
      _ <- literal(",")
    } yield FieldDefinition(fieldName, typeName, conditionLambda)

  val packetName: Parser[String] = """\s*packet """.r ~> """\w+""".r

  val packetDefinition: Parser[PacketDefinition] =
    val mappedCommentParser = aggregatedComment.map(FieldComment(_))
    bracketedList(packetName, repVector(mappedCommentParser | fieldDefinition)).map {
      case(packetName, sections) => PacketDefinition(packetName, sections)
    }

  val packetTarget: Parser[PacketTarget] =
    literal("serverbound Serverbound").map(_ => ServerBound) |
    literal("clientbound Clientbound").map(_ => ServerBound)

  val targetDefinition: Parser[TargetDefinition] =
    val mappedCommentParser = aggregatedComment.map(PacketComment(_))
    bracketedList(packetTarget, repVector(mappedCommentParser | packetDefinition)).map {
      case (target, sections) => TargetDefinition(target, sections)
    }

  val connectionState: Parser[ConnectionState] =
    import ConnectionState._

    val List(handshaking, status, login, play) = List(
      "Handshaking" -> Handshaking,
      "Status" -> Status,
      "Login" -> Login,
      "Play" -> Play
    ).map { case (str, state) => literal(str).map(_ => state) }

    """\w+ """.r ~> (handshaking | status | login | play)

  val stateDefinition: Parser[StateDefinition] =
    bracketedList(connectionState, repVector(targetDefinition)).map {
      case (state, defs) => StateDefinition(state, defs)
    }

  val wholeProtocol: Parser[ProtocolDefinition] =
    rep(stateDefinition).map { stateDefs =>
      ProtocolDefinition(stateDefs.toVector)
    }

  def apply(input: String): ProtocolDefinition = parseAll(wholeProtocol, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}
