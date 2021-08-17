package com.github.kory33.s2mctest.protocolconversion

import definition.*
import definition.FieldDefinitionSection.*
import definition.PacketDefinitionSection.*
import definition.PacketTarget.*

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

  def fieldType: Parser[FieldType] = {
    def mapPrimitive(typeName: String): Option[FieldType.Known] =
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

      mappings.get(typeName).map(FieldType.Known.apply)

    def convertTyConsName(raw: String): String = raw match {
      case "Vec" => "Vector"
      case _ => raw
    }

    val plainType = """[^=,<>\s]+(?=,|\s|>)""".r.map { typeName =>
      mapPrimitive(typeName).getOrElse(FieldType.Raw(typeName))
    }

    val wrappedType = for {
      tyConsName <- """[^=,<>\s]+(?=<)""".r <~ literal("<")
      first <- fieldType
      rest <- repVector(""",\s?""".r ~> fieldType)
      _ <- """\s?>""".r
    } yield FieldType.AppliedType(convertTyConsName(tyConsName), rest.prepended(first))

    plainType | wrappedType
  }

  val convertedLambdaBody = for {
    lambdaArg <- literal("|") ~> """\w+""".r <~ """: ?&?\w+\| ?""".r
    lambdaBody <- """[^\)]+""".r
  } yield {
    // need to remove "p."
    lambdaBody.replaceFirst("""(?<!\w)""" + lambdaArg + """\.""".r, "")
  }

  val fieldDefinition: Parser[FieldDefinition] =
    for {
      fieldName <- """\s*field """.r ~> """\w+""".r
      typeName <- literal(": ") ~> fieldType <~ literal(" =")
      conditionLambda <- opt {
        not(literal(",")) ~> literal(" when(") ~> convertedLambdaBody <~ literal(")")
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
    literal("clientbound Clientbound").map(_ => ClientBound)

  val targetDefinition: Parser[TargetDefinition] =
    val mappedCommentParser = aggregatedComment.map(PacketComment(_))
    bracketedList(packetTarget, repVector(mappedCommentParser | packetDefinition)).map {
      case (target, sections) => TargetDefinition(target, sections)
    }

  val connectionState: Parser[ConnectionState] =
    import com.github.kory33.s2mctest.protocolconversion.definition.ConnectionState._

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
