package amyc

import amyc.utils._
import amyc.ast._
import parsing._
import analyzer._
import codegen._

import java.io.File
import scala.collection.mutable.ListBuffer

object Main {
  private def parseArgs(args: Array[String]): Context = {
    val files = ListBuffer[String]()
    var charset = "UTF-8"
    for (arg <- args)
      arg match
        case x if x.startsWith("--charset=") =>
          charset = x.drop("--charset=".size)
        case _ => files.append(arg)

    Context(new Reporter, files.toList, charset = charset)
  }

  def main(args: Array[String]): Unit = {
    val ctx = parseArgs(args)
    val pipeline =
      Lexer andThen
        Parser andThen
        NameAnalyzer andThen
        TypeChecker andThen
        CodeGen andThen
        CodePrinter

    val files = ctx.files.map(new File(_))

    try {
      if (files.isEmpty) {
        ctx.reporter.fatal("No input files")
      }
      files.find(!_.exists()).foreach { f =>
        ctx.reporter.fatal(s"File not found: ${f.getName}")
      }
      pipeline.run(ctx)(files)
      ctx.reporter.terminateIfErrors()
    } catch {
      case AmycFatalError(_) =>
        sys.exit(1)
    }
  }

  import SymbolicTreeModule.{Program => SP}
  import NominalTreeModule.{Program => NP}

  def treePrinterS(title: String): Pipeline[(SP, SymbolTable), Unit] = {
    new Pipeline[(SP, SymbolTable), Unit] {
      def run(ctx: Context)(v: (SP, SymbolTable)) = {
        println(title)
        println(SymbolicPrinter(v._1)(true))
      }
    }
  }

  def treePrinterN(title: String): Pipeline[NP, Unit] = {
    new Pipeline[NP, Unit] {
      def run(ctx: Context)(v: NP) = {
        println(title)
        println(NominalPrinter(v))
      }
    }
  }
}
