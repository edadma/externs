package io.github.edadma.externs

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.{CharSequenceReader, Position, Positional}

object ExternsParser extends RegexParsers with PackratParsers {

  override protected val whiteSpace: Regex = """(\s|//.*)+""".r // """(\s|/\*(.|[\r\n])*?\*/|//.*)+""".r

  lazy val pos: PackratParser[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  def kw(s: String): Regex = s"$s\\b".r

  lazy val externs: PackratParser[ExternDeclarationsAST] =
    rep(extern) ^^ ExternDeclarationsAST

  lazy val extern: PackratParser[ExternDeclarationAST] =
    kw("extern") ~> ctype ~ ident ~ ("(" ~> repsep(externParams, ",") <~ ")") <~ ";" ^^ {
      case t ~ n ~ ps => ExternDeclarationAST(n, t, ps)
    }

  lazy val simpleType: PackratParser[TypeAST] =
    opt(kw("_Xconst")) ~ opt(kw("unsigned")) ~ (kw("int") | kw("char") | kw("long")) ^^ {
      case c ~ None ~ t    => PrimitiveType(s"C${t.head.toUpper}${t.tail}", c.isDefined)
      case c ~ Some(_) ~ t => PrimitiveType(s"CUnsigned${t.head.toUpper}${t.tail}", c.isDefined)
    } | opt(kw("_Xconst")) ~ ident ^^ {
      case c ~ n => TypedefType(n, c.isDefined)
    }

  lazy val ctype: PackratParser[TypeAST] = simpleType ~ opt(kw("_Xconst")) ~ opt("*") ^^ {
    case t ~ _ ~ None    => t
    case t ~ c ~ Some(_) => PointerType(t, c.isDefined)
  }

  lazy val externParams: PackratParser[ParameterAST] =
    ctype ~ "/*" ~ ident ~ "*/" ^^ {
      case t ~ _ ~ n ~ _ => ParameterAST(n, t)
    }

  lazy val value: PackratParser[String] =
    """0x[0-9a-fA-F]+|-?[0-9]+""".r ^^ identity

  lazy val ident: PackratParser[Ident] =
    pos ~ "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ {
      case p ~ n => Ident(p, n)
    }

  def parseHeader(input: String): ExternDeclarationsAST =
    parseAll(phrase(externs), new PackratReader(new CharSequenceReader(input))) match {
      case Success(result, _)     => result
      case NoSuccess(error, rest) => problem(rest.pos, error)
    }

}
