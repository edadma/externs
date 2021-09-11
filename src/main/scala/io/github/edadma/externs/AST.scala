package io.github.edadma.externs

import scala.util.parsing.input.Position

case class Ident(pos: Position, s: String)

trait AST
case class ExternDeclarationsAST(enums: List[ExternDeclarationAST])                    extends AST
case class ExternDeclarationAST(name: Ident, typ: TypeAST, params: List[ParameterAST]) extends AST
case class ParameterAST(name: Ident, typ: TypeAST)                                     extends AST

trait TypeAST                                          extends AST { val const: Boolean }
case class PrimitiveType(name: String, const: Boolean) extends TypeAST
case class TypedefType(name: Ident, const: Boolean)    extends TypeAST
case class PointerType(typ: TypeAST, const: Boolean)   extends TypeAST
