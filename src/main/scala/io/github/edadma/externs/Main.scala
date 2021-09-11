package io.github.edadma.externs

import io.github.edadma.json
import io.github.edadma.mustache._

import scopt.OParser

import java.io.File
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Main extends App {
  case class Config(file: File, start: Int, end: Option[Int])

  val builder = OParser.builder[Config]
  val parser = {
    import builder._

    OParser.sequence(
      programName("externs"),
      head("Bindings Extern Generator", "v0.1.0"),
      opt[Option[Int]]('e', "end")
        .optional()
        .action((e, c) => c.copy(end = e))
        .text("start line number (optional)"),
      help('h', "help").text("prints this usage text"),
      opt[Int]('s', "start")
        .optional()
        .action((s, c) => c.copy(start = s))
        .text("start line number (optional)"),
      version('v', "version").text("prints the version"),
      arg[File]("<file>")
        .required()
        .action((f, c) => c.copy(file = f))
        .validate(f =>
          if (f.exists && f.isFile && f.canRead) success
          else failure("<file> must exist and be a readable file"))
        .text("path to text file to open")
    )
  }

  val type2string: PartialFunction[TypeAST, String] = {
    case PointerType(PrimitiveType("CChar", const), _) => s"${if (const) "/*const*/ " else ""}CString"
    case PointerType(typ, const)                       => s"${if (const) "/*const*/ " else ""}Ptr[${type2string(typ)}]"
    case PrimitiveType(name, const)                    => s"${if (const) "/*const*/ " else ""}$name"
    case TypedefType(Ident(_, name), const)            => s"${if (const) "/*const*/ " else ""}$name"
  }

  OParser.parse(parser, args, Config(null, 1, None)) match {
    case Some(conf) => app(conf)
    case _          =>
  }

  def app(conf: Config): Unit = {
    val lines                          = util.Using(scala.io.Source.fromFile(conf.file.getPath))(_.getLines() to ArraySeq).get
    val section                        = lines dropRight (lines.length - conf.end.getOrElse(0)) drop (conf.start - 1)
    val ExternDeclarationsAST(externs) = ExternsParser.parseHeader(section mkString "\n")
    val list                           = new ListBuffer[json.Object]

    for (ExternDeclarationAST(name, typ, params) <- externs) {
      list += json.Object("name"   -> name.s,
                          "params" -> externParams(params),
                          "type"   -> type2string(typ),
                          "line"   -> (conf.start + name.pos.line - 1).toString)
    }

    val data = json.Object("externs" -> json.Array(list))

    val template =
      """
        |{{#externs}}
        |def {{name}}({{#params}}{{name}}: {{type}}{{comma}}{{/params}}): {{type}} = extern //{{line}}
        |{{/externs}}
        |""".trim.stripMargin

    println(processMustache(data, template, "trim" -> false, "removeNonSectionBlanks" -> false))
  }

  def externParams(params: List[ParameterAST]): json.Array = {
    val array = for (ParameterAST(Some(Ident(_, name)), typ) <- params) yield {
      (name, type2string(typ))
    }

    json.Array(array.zipWithIndex map {
      case ((n, t), i) => json.Object("name" -> n, "type" -> t, "comma" -> (if (i == array.length - 1) "" else ", "))
    })
  }

}
