package com.github.kory33.s2mctest.protocolconversion
package idbindings

import com.github.kory33.s2mctest.protocolconversion.idbindings.definition.{BindingEntry, VersionedBindings, WithState}

import scala.util.parsing.combinator.RegexParsers

object BindingsParser extends RegexParsers {

  override val skipWhitespace = false

  val entry: Parser[BindingEntry] =
    for {
      numberLiteral <- """-?0x(\d|[a-f])+""".r
      _ <- literal(" => ")
      className <- """\w+""".r <~ """(\s*//[^\n]+)?""".r // remove comments
    } yield BindingEntry(numberLiteral, className)

  val state: Parser[WithState] =
    for {
      stateName <-
        whiteSpace ~> """\w+""".r <~ """ \w+ \{""".r

      serverBoundEntry <-
        whiteSpace ~> literal("serverbound Serverbound {") ~>
          rep(whiteSpace ~> entry) <~
        whiteSpace <~ literal("}")

      clientBoundEntry <-
        whiteSpace ~> literal("clientbound Clientbound {") ~>
          rep(whiteSpace ~> entry) <~
        whiteSpace <~ literal("}")

      _ <-
        whiteSpace <~ literal("}")
    } yield WithState(stateName, serverBoundEntry, clientBoundEntry)

  val wholeBindings: Parser[List[WithState]] =
    """protocol_packet_ids!(""" ~> rep(state) <~ whiteSpace <~ literal(");")

  def apply(versionName: String, input: String): VersionedBindings = parseAll(wholeBindings, input) match {
    case Success(result, _) => VersionedBindings(versionName, result)
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}
