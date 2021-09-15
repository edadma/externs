package io.github.edadma.externs

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.{CharSequenceReader, Position, Positional}

object ExternsParser extends RegexParsers with PackratParsers {

  override protected val whiteSpace: Regex = """(\s|/\*(.|[\r\n])*?\*/|//.*)+""".r

  lazy val pos: PackratParser[Position] = positioned(success(new Positional {})) ^^ (_.pos)

  def kw(s: String): Regex = s"$s\\b".r

  lazy val externs: PackratParser[ExternDeclarationsAST] =
    rep(extern) ^^ ExternDeclarationsAST

  lazy val extern: PackratParser[ExternDeclarationAST] =
    ((kw("YAML_DECLARE") ~ "(" ~> ctype <~ ")") | (kw("cairo_public") | kw("extern")) ~> ctype) ~ ident ~ ("(" ~> repsep(
      externParam,
      ",") <~ ")") <~ ";" ^^ {
      case t ~ n ~ List(ParameterAST(name, PrimitiveType("Unit", "Unit", false, _))) => ExternDeclarationAST(n, t, Nil)
      case t ~ n ~ ps                                                                => ExternDeclarationAST(n, t, ps)
    }

  def nativeTypes(unsigned: Option[String], typ: String): String =
    if (typ == "void") "Unit"
    else if (unsigned.isDefined) s"CUnsigned${typ.head.toUpper}${typ.tail}"
    else s"C${typ.head.toUpper}${typ.tail}"

  def scalaTypes(typ: String): String =
    if (typ == "void") "Unit"
    else s"${typ.head.toUpper}${typ.tail}"

  lazy val simpleType: PackratParser[TypeAST] =
    opt(kw("_Xconst") | kw("const")) ~ opt(kw("unsigned")) ~ (kw("int") | kw("char") | kw("long") | kw("double") | kw(
      "void")) ^^ {
      case c ~ u ~ t => PrimitiveType(nativeTypes(u, t), scalaTypes(t), u.isDefined, c.isDefined)
    } | opt(kw("struct")) ~> opt(kw("_Xconst") | kw("const")) ~ ident ^^ {
      case c ~ n => TypedefType(n, c.isDefined)
    }

  @tailrec
  def pointers(n: Int, c: Boolean, t: TypeAST): TypeAST =
    if (n == 0) t
    else pointers(n - 1, c = false, PointerType(t, c))

  lazy val ctype: PackratParser[TypeAST] = simpleType ~ opt(kw("_Xconst")) ~ rep("*") ^^ {
    case t ~ c ~ ps => pointers(ps.length, c.isDefined, t)
  }

  lazy val externParam: PackratParser[ParameterAST] =
    ctype ~ opt("/*" ~> ident <~ "*/" | ident) ^^ {
      case t ~ n => ParameterAST(n, t)
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
